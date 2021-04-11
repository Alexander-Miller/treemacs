;;; treemacs.el --- A tree style file viewer package -*- lexical-binding: t -*-

;; Copyright (C) 2021 Alexander Miller

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; Minor mode to follow the tag at point in the treemacs view on an idle timer
;; Finding the current tag is a fairly involved process:
;; * Grab current buffer's imenu output
;; * Flatten the list to create full tag paths
;; * Sort according to tag position
;; * Beware of edge cases: org-mode headlines are containers, but also hold a position, hidden as a text property and
;;   semantic-mode parsed buffers use overlays instead of markers
;; * Find the last tag whose position begins before point
;; * Jump to that tag path
;; * No jump when there's no buffer file, or no imenu, or buffer file is not seen in treemacs etc.
;;; NOTE: This module is lazy-loaded.

;;; Code:

(require 'imenu)
(require 'hl-line)
(require 'treemacs-customization)
(require 'treemacs-core-utils)
(require 'treemacs-tags)
(require 'treemacs-scope)
(require 'treemacs-follow-mode)
(require 'treemacs-logging)

(eval-when-compile
  (require 'inline)
  (require 'cl-lib)
  (require 'treemacs-macros))

(defvar treemacs--tag-follow-timer nil
  "The idle timer object for `treemacs-tag-follow-mode'.
Active while tag follow mode is enabled and nil/cancelled otherwise.")

(defvar-local treemacs--previously-followed-tag-position nil
  "Records the last node and path whose tags were expanded by tag follow mode.
Is made up of a cons of the last expanded node and its path.  Both are kept to
make sure that the position has not become invalidated in the meantime.
When `treemacs-tag-follow-cleanup' it t this button's tags will be closed up
again when tag follow mode moves to another button.")

(defvar-local treemacs--imenu-cache nil
  "Cache for the current buffer's flattened and sorted imenu index.
Is reset in `first-change-hook' will only be set again after the buffer has been
saved.")

(define-inline treemacs--reset-imenu-cache ()
  "Reset `treemacs--imenu-cache'."
  (inline-quote (setq-local treemacs--imenu-cache nil)))

(define-inline treemacs--forget-previously-follow-tag-btn ()
  "Forget the previously followed button when treemacs is killed or rebuilt."
  (inline-quote (setq treemacs--previously-followed-tag-position nil)))

;;;###autoload
(defun treemacs--flatten&sort-imenu-index ()
  "Flatten current file's imenu index and sort it by tag position.
The tags are sorted into the order in which they appear, regardless of section
or nesting depth."
  (if (eq major-mode 'pdf-view-mode)
      'unsupported
    (let* ((imenu-auto-rescan t)
           (org? (eq major-mode 'org-mode))
           (index (-> (buffer-file-name) (treemacs--get-imenu-index)))
           (flat-index (if org?
                           (treemacs--flatten-org-mode-imenu-index index)
                         (treemacs--flatten-imenu-index index)))
           (first (caar flat-index))
           ;; in org mode buffers the first item may not be a cons since its position
           ;; is still stored as a text property
           (semantic? (and (consp first) (overlayp (cdr first))))
           (compare-func (if (memq major-mode '(markdown-mode adoc-mode))
                             #'treemacs--compare-markdown-tag-paths
                           #'treemacs--compare-tag-paths)))
      (cond
       (semantic?
        ;; go ahead and just transform semantic overlays into markers so we dont
        ;; have trouble with comparisons when searching a position
        (dolist (tag-path flat-index)
          (let ((leaf (car tag-path))
                (marker (make-marker)))
            (setcdr leaf (move-marker marker (overlay-start (cdr leaf)))))))
       ;; same goes for an org index, since headlines with children store their
       ;; positions as text properties
       (org?
        (dolist (tag-path flat-index)
          (let ((leaf (car tag-path)))
            (when (stringp leaf)
              (setcar tag-path (cons leaf (get-text-property 0 'org-imenu-marker leaf))))))))
      (sort flat-index compare-func))))

(defun treemacs--flatten-imenu-index (index &optional path)
  "Flatten a nested imenu INDEX to a flat list of tag paths.
The function works recursively with PATH being the already collected tag path in
each iteration.

INDEX: Imenu Tag Index
PATH: String List"
  (declare (pure t) (side-effect-free t))
  (let (result)
    (--each index
     (cond
      ((imenu--subalist-p it)
       (setq result
             (append result (treemacs--flatten-imenu-index (cdr it) (cons (car it) path)))))
      ;; make sure our leaf elements have a cdr where a location should be stored, it looks like there are cases,
      ;; at least on emacs 25, where we only get what amounts to an empty section
      ;; https://github.com/Alexander-Miller/treemacs/issues/283#issuecomment-427281977
      ((and (consp it) (cdr it))
       (setq result (cons (cons it (nreverse (copy-sequence path))) result)))))
    result))

(defun treemacs--flatten-org-mode-imenu-index (index &optional path)
  "Specialisation of `treemacs--flatten-imenu-index' for org mode.
An index produced in an `org-mode' buffer is special in that tag sections act
not just as a means of grouping tags (being bags of functions, classes etc).
Each tag section is instead also a headline which can be moved to.  The
flattening algorithm must therefore be slightly adjusted.

INDEX: Org Imenu Tag Index
PATH: String List"
  (declare (pure t) (side-effect-free t))
  (let (result)
    (--each index
      (let ((is-subalist? (imenu--subalist-p it)))
        (setq result (cons (cons (if is-subalist? (car it) it) (nreverse (copy-sequence path))) result))
        (when is-subalist?
          (setq result (append result (treemacs--flatten-org-mode-imenu-index (cdr it) (cons (car it) path)))))))
    result))

(define-inline treemacs--compare-tag-paths (p1 p2)
  "Compare two tag paths P1 & P2 by the position of the tags they lead to.
Used to sort tag paths according to the order their tags appear in.

P1: Tag-Path
P2: Tag-Path"
  (declare (pure t) (side-effect-free t))
  (inline-letevals (p1 p2)
    (inline-quote
     (< (-> ,p1 (cdar) (marker-position))
        (-> ,p2 (cdar) (marker-position))))))

(define-inline treemacs--compare-markdown-tag-paths (p1 p2)
  "Specialised version of `treemacs--compare-tag-paths' for markdown and adoc.
P1: Tag-Path
P2: Tag-Path"
  (declare (pure t) (side-effect-free t))
  (inline-letevals (p1 p2)
    (inline-quote
     (< (cdar ,p1) (cdar ,p2)))))

(defun treemacs--find-index-pos (point list)
  "Find the tag at POINT within a flat tag-path LIST.
Returns the tag-path whose tag is the last to be situated before POINT (meaning
that the next tag is after POINT and thus too far).  Accounts for POINT being
located either before the first or after the last tag.

POINT: Int
LIST: Sorted Tag Path List"
  (declare (pure t) (side-effect-free t))
  (when list
    (let ((first (car list))
          (last (nth (1- (length list)) list)))
      (cond
       ((<= point (-> first car cdr))
        first)
       ((>= point (-> last car cdr))
        last)
       (t (treemacs--binary-index-search point list))))))

(cl-defun treemacs--binary-index-search (point list &optional (start 0) (end (1- (length list))))
  "Find the position of POINT in LIST using a binary search.
Continuation of `treemacs--find-index-pos'.  Search LIST between START & END.

POINT: Integer
LIST: Sorted Tag Path List
START: Integer
END: Integer"
  (declare (pure t) (side-effect-free t))
  (let* ((index  (+ start (/ (- end start) 2)))
         (elem1  (nth index list))
         (elem2  (nth (1+ index) list))
         (pos1   (-> elem1 car cdr))
         (pos2   (-> elem2 car cdr)))
    (cond
     ((and (> point pos1)
           (<= point pos2))
      elem1)
     ((> pos2 point)
      (treemacs--binary-index-search point list 0 index))
     ((< pos2 point)
      (treemacs--binary-index-search point list index end)))))

(defun treemacs--do-follow-tag (flat-index treemacs-window buffer-file project)
  "Actual tag-follow implementation, run once the necessary data is gathered.

FLAT-INDEX: Sorted list of tag paths
TREEMACS-WINDOW: Window
BUFFER-FILE: Filepath
PROJECT: Project Struct"
  (let* ((tag-path (treemacs--find-index-pos (point) flat-index))
         (file-states '(file-node-open file-node-closed root-node-open root-node-closed))
         (btn))
    (when tag-path
      (treemacs-without-following
       (with-selected-window treemacs-window
         (setq btn (treemacs-current-button))
         (if btn
             ;; first move to the nearest file when we're on a tag
             (if (memq (treemacs-button-get btn :state) '(tag-node-open tag-node-closed tag-node))
                 (while (not (memq (treemacs-button-get btn :state) file-states))
                   (setq btn (treemacs-button-get btn :parent)))
               ;; when that doesnt work move manually to the correct file
               (-let [btn-path (treemacs-button-get btn :path)]
                 (unless (and (stringp btn-path) (treemacs-is-path buffer-file :same-as btn-path))
                   (treemacs-goto-file-node buffer-file project)
                   (setq btn (treemacs-current-button)))))
           ;; also move manually when there is no button at point
           (treemacs-goto-file-node buffer-file project)
           (setq btn (treemacs-current-button)))
         ;; close the button that was opened on the previous follow
         (goto-char (treemacs-button-start btn))
         ;; imenu already rescanned when fetching the tag path
         (let ((imenu-auto-rescan nil)
               (new-file-btn))
           ;; make a copy since this tag-path will be saved as cache, and the two modifications made here
           ;; make it impossible to find the current position in `treemacs--find-index-pos'
           (let* ((tag-path (copy-sequence tag-path))
                  (target-tag (list (car (car tag-path)))))
             ;; remove position marker from target tag and move it
             ;; to the end of the tag path
             (setf tag-path (nconc (cdr tag-path) target-tag))
             ;; the tag path also needs its file
             (setf tag-path (cons buffer-file tag-path))
             ;; workaround: goto routines assume that at least the very first element of the followed
             ;; path has a dom entry with a valid position, but this is not the case when moving to tags
             ;; in a previously never-expanded file node, so we first find the file to make sure its
             ;; position is known
             (setf new-file-btn (treemacs-find-file-node buffer-file))
             (treemacs-goto-node tag-path)
             (when (and treemacs--previously-followed-tag-position
                        (not (equal (car treemacs--previously-followed-tag-position) new-file-btn)))
               (-let [(prev-followed-pos . _) treemacs--previously-followed-tag-position]
                 (save-excursion
                   (when  (eq 'file-node-open (treemacs-button-get prev-followed-pos :state))
                     (goto-char prev-followed-pos)
                     (treemacs--collapse-file-node prev-followed-pos)))))
             (setf treemacs--previously-followed-tag-position
                   (cons new-file-btn (treemacs-button-get new-file-btn :path)))))
         (hl-line-highlight)
         (treemacs--evade-image)
         (when treemacs-recenter-after-tag-follow
           (treemacs--maybe-recenter treemacs-recenter-after-tag-follow)))))))

(defun treemacs--follow-tag-at-point ()
  "Follow the tag at point in the treemacs view."
  (interactive)
  (let* ((treemacs-window (treemacs-get-local-window))
         (buffer (current-buffer))
         (buffer-file (when buffer (buffer-file-name)))
         (project (treemacs--find-project-for-buffer)))
    (when (and treemacs-window buffer-file project)
      (condition-case e
          (-when-let (index (or treemacs--imenu-cache
                                (treemacs--flatten&sort-imenu-index)))
            (unless (eq index 'unsupported)
              (unless (buffer-modified-p)
                (setq-local treemacs--imenu-cache (copy-sequence index)))
              (treemacs--do-follow-tag index treemacs-window buffer-file project)))
        (imenu-unavailable (ignore e))
        (error (treemacs-log-err "Encountered error while following tag at point: %s" e))))))

(defun treemacs--setup-tag-follow-mode ()
  "Setup tag follow mode."
  (treemacs-follow-mode -1)
  (--each (buffer-list)
    (with-current-buffer it
      (treemacs--reset-imenu-cache)))
  (add-hook 'first-change-hook #'treemacs--reset-imenu-cache)
  (setq treemacs--tag-follow-timer
        (run-with-idle-timer treemacs-tag-follow-delay t #'treemacs--follow-tag-at-point)))

(defun treemacs--tear-down-tag-follow-mode ()
  "Tear down tag follow mode."
  (remove-hook 'first-change-hook #'treemacs--reset-imenu-cache)
  (when treemacs--tag-follow-timer
    (cancel-timer treemacs--tag-follow-timer)))

;;;###autoload
(define-minor-mode treemacs-tag-follow-mode
  "Toggle `treemacs-tag-follow-mode'.

This acts as more fine-grained alternative to `treemacs-follow-mode' and will
thus disable `treemacs-follow-mode' on activation.  When enabled treemacs will
focus not only the file of the current buffer, but also the tag at point.

The follow action is attached to Emacs' idle timer and will run
`treemacs-tag-follow-delay' seconds of idle time.  The delay value is not an
integer, meaning it accepts floating point values like 1.5.

Every time a tag is followed a re--scan of the imenu index is forced by
temporarily setting `imenu-auto-rescan' to t (though a cache is applied as long
as the buffer is unmodified).  This is necessary to assure that creation or
deletion of tags does not lead to errors and guarantees an always up-to-date tag
view.

Note that in order to move to a tag in treemacs the treemacs buffer's window
needs to be temporarily selected, which will reset blink-cursor-mode's timer if
it is enabled.  This will result in the cursor blinking seemingly pausing for a
short time and giving the appearance of the tag follow action lasting much
longer than it really does."
  :init-value nil
  :global     t
  :lighter    nil
  :group      'treemacs
  (if treemacs-tag-follow-mode
      (treemacs--setup-tag-follow-mode)
    (treemacs--tear-down-tag-follow-mode)))

(provide 'treemacs-tag-follow-mode)

;;; treemacs-tag-follow-mode.el ends here
