;;; treemacs.el --- A tree style file viewer package -*- lexical-binding: t -*-

;; Copyright (C) 2018 Alexander Miller

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

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

;;; Code:

(require 'imenu)
(require 'f)
(require 'hl-line)
(require 'treemacs-customization)
(require 'treemacs-impl)
(require 'treemacs-tags)
(require 'treemacs-follow-mode)
(eval-and-compile
  (require 'cl-lib)
  (require 'treemacs-macros))

(defvar treemacs--tag-follow-timer nil
  "The idle timer object for `treemacs-tag-follow-mode'.
Active while tag follow mode is enabled and nil/canceled otherwise.")

(defvar-local treemacs--previously-followed-tag-btn nil
  "Records the last button whose tags were expanded by tag follow mode.
When `treemacs-tag-follow-cleanup' it t this button's tags will be closed up
again when tag follow mode moves to another button.")

(defsubst treemacs--forget-previously-follow-tag-btn ()
  "Forget the previously followed button when treemacs is killed or rebuilt."
  (setq treemacs--previously-followed-tag-btn nil))

(defsubst treemacs--flatten&sort-imenu-index ()
  "Flatten current file's imenu index and sort it by tag position.
The tags are sorted into the order in which they appear, reguardless of section
or nesting depth."
  (let* ((imenu-auto-rescan t)
         (org? (eq major-mode 'org-mode))
         (index (-> (buffer-file-name) (treemacs--get-imenu-index)))
         (flat-index (if org?
                         (treemacs--flatten-org-mode-imenu-index index)
                       (treemacs--flatten-imenu-index index)))
         (first (caar flat-index))
         ;; in org mode buffers the first item may not be a cons since its position
         ;; is still stored as a text property
         (semantic? (and (consp first) (overlayp (cdr first)))))
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
    (sort flat-index #'treemacs--compare-tag-paths)))

(defun treemacs--flatten-imenu-index (index &optional path)
  "Flatten a nested imenu INDEX to a flat list of tag paths.
The function works recursively with PATH being the already collected tag path in
each iteration.

INDEX: Imenu Tag Index
PATH: String List"
  (declare (pure t) (side-effect-free t))
  (let (result)
    (--each index
     (if (imenu--subalist-p it)
         (setq result
               (append result (treemacs--flatten-imenu-index (cdr it) (cons (car it) path))))
       (setq result (cons (cons it (nreverse (copy-sequence path))) result))))
    result))

(defun treemacs--flatten-org-mode-imenu-index (index &optional path)
  "Specialization of `treemacs--flatten-imenu-index' for org mode.
An index produced in an `org-mode' buffer is special in that tag sections act
not just as a means of grouping tags (being bags of functions, classes etc).
Each tag section is instead also a headline which can be moved to. The
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

(defun treemacs--compare-tag-paths (p1 p2)
  "Compare two tag paths P1 & P2 by the position of the tags they lead to.
Used to sort tag paths according to the order their tags appear in.

P1: Tag-Path
P2: Tag-Path"
  (declare (pure t) (side-effect-free t))
  (< (-> p1 (cdar) (marker-position))
     (-> p2 (cdar) (marker-position))))

(defun treemacs--find-index-pos (point list)
  "Find the tag at POINT within a flat tag-path LIST.
Returns the tag-path whose tag is the last to be situated before POINT (meaning
that the next tag is after POINT and thus too far). Accounts for POINT being
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
  "Finds the position of POINT in LIST using a binary search.
Continuation of `treemacs--find-index-pos'. Search LIST between START & END.

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

(defsubst treemacs--do-follow-tag (flat-index treemacs-window buffer-file project)
  "Actual tag-follow implementation, run once the necessary data is gathered.

FLAT-INDEX: Sorted list of tag paths
TREEMACS-WINDOW: Window
BUFFER-FILE: Path
PROJECT: `cl-struct-treemacs-project'"
  (let* ((tag-path (treemacs--find-index-pos (point) flat-index))
         (file-states '(file-node-open file-node-closed root-node-open root-node-closed))
         (btn))
    (when tag-path
      (treemacs-without-following
       (with-selected-window treemacs-window
         (setq btn (treemacs-current-button))
         (if btn
             (progn
               ;; first move to the nearest file when we're on a tag
               (when (memq (button-get btn :state) '(tag-node-open tag-node-closed tag-node))
                 (while (not (memq (button-get btn :state) file-states))
                   (setq btn (button-get btn :parent))))
               ;; close the button that was opened on the previous follow
               (when (and treemacs--previously-followed-tag-btn
                          (not (eq treemacs--previously-followed-tag-btn btn)))
                 (save-excursion
                   (goto-char treemacs--previously-followed-tag-btn)
                   (when  (and (string= (button-get (treemacs-current-button) :path)
                                        (button-get treemacs--previously-followed-tag-btn :path))
                               (eq 'file-node-open (button-get treemacs--previously-followed-tag-btn :state)))
                     (treemacs--collapse-file-node treemacs--previously-followed-tag-btn))))
               ;; when that doesnt work move manually to the correct file
               (unless (string-equal buffer-file (button-get btn :path))
                 (treemacs-goto-button buffer-file project)
                 (setq btn (treemacs-current-button))))
           ;; also move manually when there is no button at point
           (treemacs-goto-button buffer-file project)
           (setq btn (treemacs-current-button)))
         (goto-char (button-start btn))
         (setq treemacs--previously-followed-tag-btn btn)
         ;; imenu already rescanned when fetching the tag path
         (let ((imenu-auto-rescan nil))
           ;; the target tag still has its position marker attached
           (setcar tag-path (car (car tag-path)))
           ;; the tag path also needs its file
           (setcdr tag-path (cons buffer-file (cdr tag-path)))
           (treemacs--goto-tag-button-at tag-path))
         (hl-line-highlight)
         (treemacs--evade-image)
         (when treemacs-recenter-after-tag-follow
           (treemacs--maybe-recenter)))))))

(defun treemacs--follow-tag-at-point ()
  "Follow the tag at point in the treemacs view."
  (interactive)
  (let* ((treemacs-window (treemacs--is-visible?))
         (buffer (current-buffer))
         (buffer-file (when buffer (buffer-file-name)))
         (project (treemacs--find-project-for-buffer)))
    (when (and treemacs-window buffer-file project)
      (condition-case e
          (-when-let (index (treemacs--flatten&sort-imenu-index))
            (treemacs--do-follow-tag index treemacs-window buffer-file project))
        (error (treemacs-log "Encountered error while following tag at point: %s" e))))))

(defsubst treemacs--setup-tag-follow-mode ()
  "Setup tag follow mode."
  (treemacs-follow-mode -1)
  (setq treemacs--tag-follow-timer
        (run-with-idle-timer treemacs-tag-follow-delay t #'treemacs--follow-tag-at-point)))

(defsubst treemacs--tear-down-tag-follow-mode ()
  "Tear down tag follow mode."
  (when treemacs--tag-follow-timer
    (cancel-timer treemacs--tag-follow-timer)))

(define-minor-mode treemacs-tag-follow-mode
  "Toggle `treemacs-tag-follow-mode'.

This acts as more fine-grained alternative to `treemacs-follow-mode' and will
thus disable `treemacs-follow-mode' on activation. When enabled treemacs will
focus not only the file of the current buffer, but also the tag at point.

The follow action is attached to Emacs' idle timer and will run
`treemacs-tag-follow-delay' seconds of idle time. The delay value is not an
integer, meaning it accepts floating point values like 1.5.

Every time a tag is followed a rescan of the imenu index is forced by
temporarily setting `imenu-auto-rescan' to t. This is necessary to assure that
creation or deletion of tags does not lead to errors and guarantees an always
up-to-date tag view.

Note that in order to move to a tag in treemacs the treemacs buffer's window
needs to be temporarily selected, which will reset `blink-cursor-mode's timer if
it is enabled. This will result in the cursor blinking seemingly pausing for a
short time and giving the appereance of the tag follow action lasting much
longer than it really does."
  :init-value nil
  :global     t
  :lighter    nil
  (if treemacs-tag-follow-mode
      (treemacs--setup-tag-follow-mode)
    (treemacs--tear-down-tag-follow-mode)))

(provide 'treemacs-tag-follow-mode)

;;; treemacs-tag-follow-mode.el ends here
