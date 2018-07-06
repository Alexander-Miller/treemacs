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
;;; General implementation details.

;;; Code:

;;;;;;;;;;;;;;;;;;
;; Requirements ;;
;;;;;;;;;;;;;;;;;;

(require 'hl-line)
(require 'dash)
(require 's)
(require 'ht)
(require 'f)
(require 'ace-window)
(require 'vc-hooks)
(require 'pfuture)
(require 'treemacs-customization)
(eval-and-compile
  (require 'cl-lib)
  (require 'treemacs-macros))

(treemacs-import-functions-from "treemacs-tags"
  treemacs--expand-file-node
  treemacs--collapse-file-node
  treemacs--expand-tag-node
  treemacs--collapse-tag-node
  treemacs--extract-position
  treemacs--goto-tag
  treemacs--tags-path-of
  treemacs--goto-tag-button-at)

(treemacs-import-functions-from "treemacs"
  treemacs-refresh)

(treemacs-import-functions-from "treemacs-branch-creation"
  treemacs--add-root-element
  treemacs--expand-root-node
  treemacs--collapse-root-node
  treemacs--expand-dir-node
  treemacs--collapse-dir-node
  treemacs--create-branch)

(treemacs-import-functions-from "treemacs-filewatch-mode"
  treemacs--start-watching
  treemacs--stop-filewatch-for-current-buffer
  treemacs--stop-watching
  treemacs--cancel-refresh-timer)

(treemacs-import-functions-from "treemacs-follow-mode"
  treemacs--follow)

(treemacs-import-functions-from "treemacs-visuals"
  treemacs-pulse-on-success
  treemacs--tear-down-icon-highlight
  treemacs--forget-previously-follow-tag-btn
  treemacs--forget-last-highlight)

(treemacs-import-functions-from "treemacs-async"
  treemacs--git-status-process-function
  treemacs--collapsed-dirs-process)

(treemacs-import-functions-from "treemacs-structure"
  treemacs-on-collapse
  treemacs-get-from-shadow-index
  treemacs-get-position-of
  treemacs-shadow-node->children
  treemacs-shadow-node->key
  treemacs-shadow-node->closed
  treemacs-shadow-node->position
  treemacs--reset-index
  treemacs--on-rename
  treemacs--invalidate-position-cache)

(treemacs-import-functions-from "treemacs-workspaces"
  make-treemacs-project
  treemacs--reset-project-positions
  treemacs-current-workspace
  treemacs-workspace->projects
  treemacs-workspace->is-empty?
  treemacs-add-project-at
  treemacs-project->is-expanded?
  treemacs-project->path
  treemacs-project->refresh
  treemacs-project->position
  treemacs--find-project-for-path)

(treemacs-import-functions-from "treemacs-visuals"
  treemacs-pulse-on-failure)

(declare-function treemacs-mode "treemacs-mode")

;;;;;;;;;;;;;;;;;;
;; Private vars ;;
;;;;;;;;;;;;;;;;;;

(defvar treemacs--buffer-access nil
  "Alist mapping treemacs buffers to frames.")

(defvar treemacs--scope-id 0
  "Used as a frame parameter to identify a frame over multiple sessions.
Used to restore the frame -> buffer mapping in `treemacs--buffer-access' with
desktop save mode.")

(defvar treemacs--taken-scopes nil
  "List of already taken scope ids that can no longer be used.
Especially important after a session restore, since the list of used ids may no
longer be contigious.")

(defconst treemacs--buffer-name-prefix " *Treemacs-")

(defconst treemacs-dir
  ;; locally we're in src/elisp, installed from melpa we're at the package root
  (-let [dir (-> (if load-file-name
                     (file-name-directory load-file-name)
                   default-directory)
                 (expand-file-name))]
    (if (s-ends-with? "src/elisp/" dir)
        (-> dir (f-parent) (f-parent))
      dir))
  "The directory treemacs.el is stored in.")

(defvar treemacs--no-messages nil
  "When set to t `treemacs-log' will produce no output.
Not used directly, but as part of `treemacs-without-messages'.")

(defvar-local treemacs--width-is-locked t
  "Keeps track of whether the width of the treemacs window is locked.")

(defvar treemacs--pre-peek-state nil
  "List of window, buffer to restore and buffer to kill treemacs used for peeking.")

;;;;;;;;;;;;;;;;;;;
;; Substitutions ;;
;;;;;;;;;;;;;;;;;;;

(defsubst treemacs--nearest-parent-directory (path)
  "Return the parent of PATH is it's a file, or PATH if it is a directory."
  (if (file-directory-p path)
      path
    (treemacs--parent path)))

(defsubst treemacs-current-button ()
  "Get the button in the current line.
Returns nil when point is between projects."
  (if (get-text-property (point-at-bol) 'button)
      (button-at (point-at-bol))
    (let ((p (next-single-property-change (point-at-bol) 'button nil (point-at-eol))))
      (when (and (get-char-property p 'button))
          (copy-marker p t)))))

(defsubst treemacs--unslash (path)
  "Remove the final slash in PATH."
  (declare (pure t) (side-effect-free t))
  (if (and (> (length path) 1)
           (eq ?/ (aref path (1- (length path)))))
      (substring path 0 -1)
    path))

(defsubst treemacs--get-label-of (btn)
  "Return the text label of BTN."
  (interactive)
  (buffer-substring-no-properties (button-start btn) (button-end btn)))

(defsubst treemacs--is-path-in-dir? (path dir)
  "Is PATH in directory DIR?"
  (or (string= path dir)
      (s-starts-with? (f-slash dir) path)))

(defsubst treemacs--replace-recentf-entry (old-file new-file)
  "Replace OLD-FILE with NEW-FILE in the recent file list."
  ;; code taken from spacemacs - is-bound check due to being introduced after emacs24?
  ;; better safe than sorry so let's keep it
  (with-no-warnings
    (when (fboundp 'recentf-add-file)
      (recentf-add-file new-file)
      (recentf-remove-if-non-kept old-file))))

(defsubst treemacs--is-visible? ()
  "Inidicates whether this frame's treemacs buffer is currently visible.
Will return the treemacs window if true."
  (->> (window-list (selected-frame))
       (--first (->> it
                     (window-buffer)
                     (buffer-name)
                     (s-starts-with? treemacs--buffer-name-prefix)))))

(defsubst treemacs-buffer-exists? ()
  "Indicates whether this frame's treemacs buffer exists.
Returns the buffer if it does exist."
  (let ((b (cdr (assoc (selected-frame) treemacs--buffer-access))))
    (when (buffer-live-p b) b)))

(defsubst treemacs--select-visible-window ()
  "Switch to treemacs buffer, given that it is currently visible."
  (->> treemacs--buffer-access
       (assoc (selected-frame))
       (cdr)
       (get-buffer-window)
       (select-window)))

(defsubst treemacs--select-not-visible-window ()
  "Switch to treemacs buffer, given that it not visible."
  (treemacs--setup-buffer))

(defsubst treemacs--button-symbol-switch (new-sym)
  "Replace icon in current line with NEW-SYM."
  (save-excursion
    (-let [len (length new-sym)]
      (goto-char (- (button-start (next-button (point-at-bol) t)) len))
      (insert new-sym)
      (delete-char len))))

(defsubst treemacs--prop-at-point (prop)
  "Grab property PROP of the button at point.
Returns nil when there is no button at point."
  (-when-let (b (treemacs-current-button))
    (button-get b prop)))

(defsubst treemacs-parent-of (btn)
  "Return the parent path of BTN."
  (--if-let (button-get btn :parent)
      (button-get it :path)
    (treemacs--parent (button-get btn :path))))

(defsubst treemacs--reject-ignored-files (file)
  "Return t if FILE is *not* an ignored file.
FILE here is a list consisting of an absolute path and file attributes."
  (-let [filename (f-filename file)]
    (--none? (funcall it filename file) treemacs-ignored-file-predicates)))

(defsubst treemacs--reject-ignored-and-dotfiles (file)
  "Return t when FILE is neither ignored, nor a dotfile.
FILE here is a list consisting of an absolute path and file attributes."
  (let ((filename (f-filename file)))
    (and (not (s-matches? treemacs-dotfiles-regex filename))
         (--none? (funcall it filename file) treemacs-ignored-file-predicates))))

(defsubst treemacs--file-extension (file)
  "Same as `file-name-extension', but also works with leading periods.

This is something a of workaround to easily allow assigning icons to a FILE with
a name like '.gitignore' without always having to check for both file extensions
and special names like this."
  (declare (pure t) (side-effect-free t))
  (-let [filename (f-filename file)]
    (save-match-data
      (if (string-match "\\.[^.]*\\'" filename)
          (substring filename (1+ (match-beginning 0)))
        filename))))

(defsubst treemacs-is-treemacs-window? (window)
  "Return t when WINDOW is showing a treemacs buffer."
  (declare (side-effect-free t))
  (->> window window-buffer buffer-name (s-starts-with? treemacs--buffer-name-prefix)))

(defsubst treemacs--get-framelocal-buffer ()
  "Get this frame's local buffer, creating it if necessary.
Will also perform cleanup if the buffer is dead."
  (-let*- [(frame (selected-frame))
           (buf   (assq frame treemacs--buffer-access))]
    (when (or (null buf)
              (not (buffer-live-p buf)))
      (setq treemacs--buffer-access
            (assq-delete-all frame treemacs--buffer-access))
      (unless (frame-parameter frame 'treemacs-id)
        (while (memq (setq treemacs--scope-id (1+ treemacs--scope-id)) treemacs--taken-scopes))
        (push treemacs--scope-id treemacs--taken-scopes)
        (set-frame-parameter frame 'treemacs-id (number-to-string treemacs--scope-id)))
      (setq buf (get-buffer-create (format "%sFramebuffer-%s*" treemacs--buffer-name-prefix (frame-parameter frame 'treemacs-id))))
      (push (cons frame buf) treemacs--buffer-access))
    buf))

(defsubst treemacs--next-neighbour-of (btn)
  "Get the next same-level neighbour of BTN, if any."
  (declare (side-effect-free t))
  (-let- [(depth (button-get btn :depth))
          (next (next-button (button-end btn)))]
    (while (and next (< depth (button-get next :depth)))
      (setq next (next-button (button-end next))))
    (when (and next (= depth (button-get next :depth))) next)))

(defsubst treemacs--prev-neighbour-of (btn)
  "Get the previous same-level neighbour of BTN, if any."
  (declare (side-effect-free t))
  (-let- [(depth (button-get btn :depth))
          (prev (previous-button (button-start btn)))]
    (while (and prev (< depth (button-get prev :depth)))
      (setq prev (previous-button (button-start prev))))
    (when (and prev (= depth (button-get prev :depth))) prev)))

(defsubst treemacs--next-non-child-button (btn)
  "Return the next button after BTN that is not a child of BTN."
  (declare (side-effect-free t))
  (when btn
    (-let- [(depth (button-get btn :depth))
            (next (next-button (button-end btn) t))]
      (while (and next (< depth (button-get next :depth)))
        (setq next (next-button (button-end next) t)))
      next)))

(defsubst treemacs--remove-framelocal-buffer ()
  "Remove the frame-local buffer from the current frame.
To be run in the kill buffer hook as it removes the mapping
of the `current-buffer'."
  (setq treemacs--buffer-access
        (rassq-delete-all (current-buffer) treemacs--buffer-access)))

(defsubst treemacs--on-file-deletion (path &optional no-buffer-delete)
  "Cleanup to run when treemacs file at PATH was deleted.
Do not try to delete buffers for PATH when NO-BUFFER-DELETE is non-nil. This is
necessary since interacting with magit can cause file delete events for files
being edited to trigger."
  (unless no-buffer-delete (treemacs--kill-buffers-after-deletion path t))
  (treemacs--stop-watching path t)
  (treemacs-run-in-every-buffer
   (treemacs-on-collapse path t)))

(defsubst treemacs--refresh-dir (path)
  "Local refresh for button at PATH.
Simply collapses and re-expands the button (if it has not been closed)."
  (-let [btn (treemacs-goto-button path)]
    (when (memq (button-get btn :state) '(dir-node-open file-node-open))
      (goto-char (button-start btn))
      (treemacs--push-button btn)
      (goto-char (button-start btn))
      (treemacs--push-button btn))))

(defsubst treemacs--follow-each-dir (btn dir-parts)
  "BTN Follow (goto and open) every single dir in DIR-PARTS under ROOT.
Return the button that is found or the symbol 'follow-failed' if the search
failed."
  (-let*- [(root (button-get btn :path))
           (git-future (treemacs--git-status-process-function root))
           (last-index (- (length dir-parts) 1))]
    (goto-char btn)
    ;; point is currently on the next closest dir to the followed file we could get
    ;; from the shadow index, so we expand it to keep going
    (-pcase (button-get btn :state)
      ['dir-node-closed (treemacs--expand-dir-node btn :git-future git-future)]
      ['root-node-closed (treemacs--expand-root-node btn)])
    (catch 'follow-failed
      (-let- [(index 0)
              (dir-part nil)]
        (while dir-parts
          (setq dir-part (pop dir-parts)
                root (f-join root dir-part))
          ;; for every manual part add it to the current root and find the first button below
          ;; btn whose :path is the root, expand it, keep looping
          (setq btn (first-child-btn-where btn
                      (-if-let- [collapse-count (button-get child-btn :collapsed)]
                          ;; button has collapsed path, so a direct comparison is impossble
                          ;; for example we are following [root app src model file] but the path
                          ;; is collapsed as app/src/model. normally we would check if the button's
                          ;; path is root/app, but that obviously won't work. since two path elements
                          ;; are appended to the original path the button's :collapsed property has
                          ;; a value of 2, so we append the next two elemnts of dir-parts and make
                          ;; the comparison then
                          (when (>= (length dir-parts) collapse-count)
                            (-let [coll-root root]
                              (dotimes (n collapse-count) ;; TODO reduce fjoin use
                                (setq coll-root (f-join coll-root (nth n dir-parts))))
                              (when (string= coll-root (button-get child-btn :path))
                                (setq root coll-root
                                      index (+ index collapse-count)
                                      ;; when we have a hit the collapsed dirs must not be iterated over
                                      dir-parts (nthcdr (1+ collapse-count) dir-parts))
                                child-btn)))
                        (string= root (button-get child-btn :path)))))
          (unless btn (throw 'follow-failed 'follow-failed))
          (goto-char btn)
          ;; don't open dir at the very end of the list since we only want to put
          ;; point in its line
          (when (and (eq 'dir-node-closed (button-get btn :state))
                     (< index last-index))
            (treemacs--expand-dir-node btn :git-future git-future))
          (setq index (1+ index))))
      btn)))

;;;;;;;;;;;;;;;
;; Functions ;;
;;;;;;;;;;;;;;;

(defun treemacs--canonical-path (path)
  "The canonical version of PATH for being handled by treemacs.
In practice this means expand PATH and remove its final slash."
  (-> path (f-long) (treemacs--unslash)))

(defun treemacs-is-file-git-ignored? (file git-info)
  "Determined if FILE is ignored by git by means of GIT-INFO."
  (eq ?! (ht-get git-info file)))

(defun treemacs-is-treemacs-window-selected? ()
  "Return t when the treemacs window is selected."
  (s-starts-with? treemacs--buffer-name-prefix (buffer-name)))

(defun treemacs--reload-buffers-after-rename (old-path new-path)
  "Reload buffers and windows after OLD-PATH was renamed to NEW-PATH."
  ;; first buffers shown in windows
  (dolist (frame (frame-list))
    (dolist (window (window-list frame))
      (let* ((win-buff  (window-buffer window))
             (buff-file (buffer-file-name win-buff)))
        (when buff-file
          (setq buff-file (f-long buff-file))
          (when (treemacs--is-path-in-dir? buff-file old-path)
            (treemacs-without-following
             (with-selected-window window
               (kill-buffer win-buff)
               (let ((new-file (s-replace old-path new-path buff-file)))
                 (find-file-existing new-file)
                 (treemacs--replace-recentf-entry buff-file new-file)))))))))
  ;; then the rest
  (--each (buffer-list)
    (-when-let (buff-file (buffer-file-name it))
      (setq buff-file (f-long buff-file))
      (when (treemacs--is-path-in-dir? buff-file old-path)
        (let ((new-file (s-replace old-path new-path buff-file)))
          (kill-buffer it)
          (find-file-noselect new-file)
          (treemacs--replace-recentf-entry buff-file new-file))))))

(defun treemacs--maybe-filter-dotfiles (dirs)
  "Remove from DIRS directories that shouldn't be reopened.
That is, directories (and their descendants) that are in the reopen cache, but
are not being shown on account of `treemacs-show-hidden-files' being nil."
  (if treemacs-show-hidden-files
      dirs
    (-let [root (treemacs--find-project-for-path (car dirs))]
      (--filter (not (--any (s-matches? treemacs-dotfiles-regex it)
                            (f-split (substring it (length root)))))
                dirs))))

(defun treemacs--get-children-of (parent-btn)
  "Get all buttons exactly one level deeper than PARENT-BTN.
The child buttons are returned in the same order as they appear in the treemacs
buffer."
  (let ((ret)
        (btn (next-button (button-end parent-btn) t)))
    (when (equal (1+ (button-get parent-btn :depth)) (button-get btn :depth))
      (setq ret (cons btn ret))
      (while (setq btn (treemacs--next-neighbour-of btn))
        (push btn ret)))
    (nreverse ret)))

(defun treemacs--init (&optional root)
  "Initialize a treemacs buffer from the current workspace.
Add a project for ROOT if it's non-nil."
  (-let [origin-buffer (current-buffer)]
    (-pcase (treemacs--current-visibility)
      ['visible (treemacs--select-visible-window)]
      ['exists (treemacs--select-not-visible-window)]
      ['none
       (treemacs--setup-buffer)
       (treemacs-mode)
       (treemacs--reset-index)
       (treemacs--reset-project-positions)
       (treemacs-with-writable-buffer
        (-let*- [(projects (treemacs-workspace->projects (treemacs-current-workspace)))
                 (last-index (1- (length projects)))]
          (--each projects
            (treemacs--add-root-element it)
            (unless (= it-index last-index)
              (insert "\n")
              (when treemacs-space-between-root-nodes
                (insert "\n"))))))
       (goto-char 2)])
    (when root (treemacs-add-project-at (treemacs--canonical-path root)))
    (with-no-warnings (setq treemacs--ready-to-follow t))
    (when (or treemacs-follow-after-init (with-no-warnings treemacs-follow-mode))
      (with-current-buffer origin-buffer
        (treemacs--follow)))))

(defun treemacs--on-buffer-kill ()
  "Cleanup to run when a treemacs buffer is killed."
  ;; stop watch must come first since we need a reference to the killed buffer
  ;; to remove it from the filewatch list
  (treemacs--stop-filewatch-for-current-buffer)
  (treemacs--remove-framelocal-buffer)
  (treemacs--tear-down-icon-highlight)
  (unless treemacs--buffer-access
    ;; TODO make local maybe
    (remove-hook 'window-configuration-change-hook #'treemacs--on-window-config-change)))

(defun treemacs--push-button (btn &optional recursive)
  "Execute the appropriate action given the state of the pushed BTN.
Optionally do so in a RECURSIVE fashion."
  (-pcase (button-get btn :state)
    ['root-node-closed (treemacs--expand-root-node btn)]
    ['dir-node-open    (treemacs--collapse-dir-node btn recursive)]
    ['dir-node-closed  (treemacs--expand-dir-node btn :recursive recursive)]
    ['file-node-open   (treemacs--collapse-file-node btn recursive)]
    ['file-node-closed (treemacs--expand-file-node btn recursive)]
    ['tag-node-open    (treemacs--collapse-tag-node btn recursive)]
    ['tag-node-closed  (treemacs--expand-tag-node btn recursive)]
    ['tag-node         (progn (other-window 1) (treemacs--goto-tag btn))]
    [_                 (error "[Treemacs] Cannot push button with unknown state '%s'" (button-get btn :state))]))

(defun treemacs--reopen-node (btn git-info)
  "Reopen file BTN.
GIT-INFO is passed through from the previous branch build."
  (-pcase (button-get btn :state)
    ['dir-node-closed  (treemacs--expand-dir-node btn :git-future git-info)]
    ['file-node-closed (treemacs--expand-file-node btn)]
    ['tag-node-closed  (treemacs--expand-tag-node btn)]
    ['root-node-closed (treemacs--expand-root-node btn)]
    [other             (error "[Treemacs] Cannot reopen button at path %s with state %s"
                              (button-get btn :path) other)]))

(defun treemacs--reopen-at (path git-info)
  "Reopen dirs below PATH.
GIT-INFO is passed through from the previous branch build."
  (treemacs-without-messages
   (dolist (it (-some->>
                path
                (treemacs-get-from-shadow-index)
                (treemacs-shadow-node->children)
                (-reject #'treemacs-shadow-node->closed)
                (-map #'treemacs-shadow-node->key)
                (treemacs--maybe-filter-dotfiles)))
     (treemacs--reopen-node (treemacs-goto-button it) git-info))))

(defun treemacs--nearest-path (btn)
  "Return the path property of the current button (or BTN).
If the property is not set keep looking upward, via the :parent' property.
Useful to e.g. find the path of the file of the currently selected tags entry.
Must be called from treemacs buffer."
  (let* ((path (button-get btn :path)))
    (while (null path)
      (setq btn (button-get btn :parent)
            path (button-get btn :path)))
    path))

(defun treemacs--create-file/dir (is-file?)
  "Interactively create either a file or directory, depending on IS-FILE.

IS-FILE?: Bool"
  (interactive)
  (let ((path-to-create)
        (curr-path (--if-let (treemacs-current-button)
                       (treemacs--nearest-path it)
                     (f-expand "~"))))
    (cl-block body
      (setq path-to-create (read-file-name
                            (if is-file?"Create File: " "Create Directory: ")
                            (f-slash (if (f-dir? curr-path)
                                         curr-path
                                       (f-dirname curr-path)))))
      (when (f-exists? path-to-create)
        (cl-return-from body
          (treemacs-pulse-on-failure "%s already exists."
            (propertize path-to-create 'face 'font-lock-string-face))))
      (treemacs--without-filewatch
       (if is-file?
           (-let [dir (f-dirname path-to-create)]
             (unless (f-exists? dir)
               (make-directory dir t))
             (f-touch path-to-create))
         (make-directory path-to-create t)))
      (-when-let- [project (treemacs--find-project-for-path path-to-create)]
        (treemacs-without-messages (treemacs--do-refresh (current-buffer) project))
        (treemacs-goto-button (treemacs--canonical-path path-to-create) project)
        (recenter))
      (treemacs-pulse-on-success
          "Created %s." (propertize path-to-create 'face 'font-lock-string-face)))))

(defun treemacs-goto-button (path &optional project)
  "Move point to button identified by PATH under PROJECT in the current buffer.
If PROJECT is not given it will be found with `treemacs--find-project-for-path'.
No attempt is made to verify that PATH falls under a project in the workspace.
It is assumed that this check has already been made.

PATH: Filepath
PROJECT `cl-struct-treemacs-project'"
  (unless project (setq project (treemacs--find-project-for-path path)))
  (goto-char (treemacs-project->position project))
  (-let*- [;; go back here if the search fails
           (start (point))
           ;; the path we're moving to minus the project root
           (path-minus-root (->> project (treemacs-project->path) (length) (substring path)))
           ;; the parts of the path that we can try to go to until we arrive at the project root
           (dir-parts (nreverse (s-split (f-path-separator) path-minus-root 'omit-nulls)))
           ;; the path we try to quickly move to because it's already open and thus in the shadow-index
           (goto-path (if dir-parts (treemacs--parent path) path))
           ;; if we try mode than this many times to grab a path location for the shadow index it means
           ;; the file we want to move to is under a *closed* project node
           (counter (length dir-parts))
           ;; manual as in to be expanded manually after we moved to the next closest node we can find
           ;; in the shadow index
           (manual-parts nil)
           (shadow-node nil)]
    ;; try to move as close as possible to the followed file, starting with its immediate parent
    ;; keep moving upwards in the path we move to until reaching the root of the project (counter = 0)
    ;; all the while collecting the parts of the path that beed manual expanding
    (while (and (> counter 0)
                (or (null shadow-node)
                    ;; shadow node might exist, but one of its parents might be null
                    (null (treemacs-shadow-node->position shadow-node))))
      (setq shadow-node (treemacs-get-from-shadow-index goto-path)
            goto-path (treemacs--parent goto-path)
            counter (1- counter))
      (push (pop dir-parts) manual-parts))
    (-let*- [(btn (if (= 0 counter)
                      (treemacs-project->position project)
                    (treemacs-shadow-node->position shadow-node)))
             ;; do the rest manually - at least the actual file to move to is still left in manual-parts
             (search-result (if manual-parts (treemacs--follow-each-dir btn manual-parts) btn))]
      (if (eq 'follow-failed search-result)
          (prog1 nil
            (goto-char start))
        (treemacs--evade-image)
        (hl-line-highlight)
        (set-window-point (get-buffer-window) (point))
        search-result))))

(defun treemacs--on-window-config-change ()
  "Collects all tasks that need to run on a window config change."
  (-when-let (w (treemacs--is-visible?))
    (treemacs-without-following
     (with-selected-window w
       ;; apparently keeping the hook around can lead to a feeback loop together with helms
       ;; auto-resize mode as seen in https://github.com/Alexander-Miller/treemacs/issues/76
       (let (window-configuration-change-hook)
         ;; Reset the treemacs window width to its default - required after window deletions
         (when treemacs--width-is-locked
           (treemacs--set-width treemacs-width))
         ;; Prevent treemacs from being used as other-window
         (when treemacs-is-never-other-window
           (set-window-parameter w 'no-other-window t)))))))

(defun treemacs--set-width (width)
  "Set the width of the treemacs buffer to WIDTH."
  (unless (one-window-p)
    (let ((window-size-fixed)
          (w (max width window-min-width)))
      (cond
       ((> (window-width) w)
        (shrink-window-horizontally  (- (window-width) w)))
       ((< (window-width) w)
        (enlarge-window-horizontally (- w (window-width))))))))

(defun treemacs--filter-files-to-be-shown (files)
  "Filter FILES for those files which treemacs should show.
These are the files which return nil for every function in
`treemacs-ignored-file-predicates' and do not match `treemacs-dotfiles-regex'.
The second test not apply if `treemacs-show-hidden-files' is t."
  (if treemacs-show-hidden-files
      (-filter #'treemacs--reject-ignored-files files)
    (-filter #'treemacs--reject-ignored-and-dotfiles files)))

(defun treemacs--std-ignore-file-predicate (file _)
  "The default predicate to detect ignored files.
Will return t when FILE
1) starts with '.#' (lockfiles)
2) starts with 'flycheck_' (flycheck temp files)
3) ends with '~' (backup files)
4) is surrounded with # (auto save files)
5) is '.' or '..' (default dirs)"
  (s-matches? (rx bol
                  (or (seq (or ".#" "flycheck_") (1+ any))
                      (seq (1+ any) "~")
                      (seq "#" (1+ any) "#")
                      (or "." ".."))
                  eol)
              file))

(defun treemacs--current-visibility ()
  "Return whether the current visibility state of the treemacs buffer.
Valid states are 'visible, 'exists and 'none."
  (cond
   ((treemacs--is-visible?)   'visible)
   ((treemacs-buffer-exists?) 'exists)
   (t 'none)))

(defun treemacs--on-frame-kill (frame)
  "Remove its framelocal buffer when FRAME is killed."
  (-when-let- [b (cdr (assq frame treemacs--buffer-access))]
    (kill-buffer b))
  (setq treemacs--buffer-access
        (assq-delete-all frame treemacs--buffer-access))
  (unless treemacs--buffer-access
    (setq delete-frame-functions
          (delete #'treemacs--on-frame-kill delete-frame-functions))))

(defun treemacs--setup-buffer ()
  "Create and setup a buffer for treemacs in the right position and size."
  (-> (selected-window)
      (frame-root-window)
      (split-window nil treemacs-position)
      (select-window))
  (let ((buf (treemacs--get-framelocal-buffer)))
    (switch-to-buffer buf))
  (treemacs--forget-last-highlight)
  (set-window-dedicated-p (selected-window) t)
  (let ((window-size-fixed))
    (treemacs--set-width treemacs-width)))

(defun treemacs--parent (path)
  "Parent of PATH, or PATH itself if PATH is the root directory."
  (if (string= "/" path)
      path
    (-> path
        (file-name-directory)
        (treemacs--unslash))))

(defun treemacs--evade-image ()
  "The cursor visibly blinks when on top of an icon.
It needs to be moved aside in a way that works for all indent depths and
`treemacs-indentation' settings."
  (beginning-of-line)
  (when (get-text-property (point) 'display)
    (forward-char 1)))

(defun treemacs--read-first-project-path ()
  "Read the first project on init with an empty workspace.
This function is extracted here specifically so that treemacs-projectile can
overwrite it so as to present the project root instead of the current dir as the
first choice."
  (when (treemacs-workspace->is-empty?)
    (read-directory-name "Project root: ")))

(defun treemacs--sort-value-selection ()
  "Interactive selection for a new `treemacs-sorting' value.
Retursns a cons cell of a descriptive string name and the sorting symbol."
  (declare (side-effect-free t))
  (let* ((sort-names '(("Sort Alphabetically Ascending" . alphabetic-asc)
                       ("Sort Alphabetically Descending" . alphabetic-desc)
                       ("Sort Case Insensitive Alphabetically Ascending" . alphabetic-case-insensitive-asc)
                       ("Sort Case Insensitive Alphabetically Descending" . alphabetic-case-insensitive-desc)
                       ("Sort by Size Ascending" . size-asc)
                       ("Sort by Size Descending" . size-desc)
                       ("Sort by Modification Date Ascending" . mod-time-asc)
                       ("Sort by Modification Date Descending" . mod-time-desc)))
         (selected-value (completing-read (format "Sort Method (current is %s)" treemacs-sorting)
                                          (-map #'car sort-names))))
    (--first (s-equals? (car it) selected-value) sort-names)))

(defun treemacs--kill-buffers-after-deletion (path is-file)
  "Clean up after a deleted file or directory.
Just kill the buffer visiting PATH if IS-FILE. Otherwise, go
through the buffer list and kill buffer if PATH is a prefix."
  (if is-file
      (let ((buf (get-file-buffer path)))
        (and buf
             (y-or-n-p (format "Kill buffer of %s, too? "
                               (f-filename path)))
             (kill-buffer buf)))

    ;; Prompt for each buffer visiting a file in directory
    (--each (buffer-list)
      (and
       (treemacs--is-path-in-dir? (buffer-file-name it) path)
       (y-or-n-p (format "Kill buffer %s in %s, too? "
                         (buffer-name it)
                         (f-filename path)))
       (kill-buffer it)))

    ;; Kill all dired buffers in one step
    (when (bound-and-true-p dired-buffers)
      (-when-let (dired-buffers-for-path
                  (->> dired-buffers
                       (--filter (treemacs--is-path-in-dir? (car it) path))
                       (-map #'cdr)))
        (and (y-or-n-p (format "Kill Dired buffers of %s, too? "
                               (f-filename path)))
             (-each dired-buffers-for-path #'kill-buffer))))))

(defun treemacs--do-refresh (buffer project)
  "Execute the refresh process for BUFFER and PROJECT in that buffer.
Specifically extracted with the buffer to refresh being supplied so that
filewatch mode can refresh multiple buffers at once.
Will refresh every project when PROJECT is 'all."
  (with-current-buffer buffer
    (treemacs-save-position
     (progn
       (treemacs--cancel-refresh-timer)
       (run-hook-with-args
        'treemacs-pre-refresh-hook
        project curr-line curr-btn curr-state curr-file curr-tagpath curr-winstart)

       (if (eq 'all project)
           (-each (treemacs-workspace->projects (treemacs-current-workspace)) #'treemacs-project->refresh)
         (treemacs-project->refresh project)))

     (run-hook-with-args
      'treemacs-post-refresh-hook
      project curr-line curr-btn curr-state curr-file curr-tagpath curr-winstart)

     (unless treemacs-silent-refresh
       (treemacs-log "Refresh complete.")))))

(defun treemacs--maybe-recenter ()
  "Potentially recenter after following a file or tag.
The answer depends on the distance between `point' and the window top/bottom
being smaller than `treemacs-follow-recenter-distance'."
  (interactive)
  (-let*- [(current-line (float (count-lines (window-start) (point))))
           (all-lines (float (window-height)))
           (distance-from-top (/ current-line all-lines))
           (distance-from-bottom (- 1.0 distance-from-top))]
    (when (or (> treemacs-follow-recenter-distance distance-from-top)
              (> treemacs-follow-recenter-distance distance-from-bottom))
      (recenter))))

(defun treemacs--setup-peek-buffer (btn &optional goto-tag?)
  "Setup the peek buffer and window for BTN.
Additionally also navigate to BTN's tag if GOTO-TAG is t.

BTN: Button
GOTO-TAG: Bool"
  (-let- [(path (if goto-tag?
                    (treemacs-with-button-buffer btn
                      (treemacs--nearest-path btn))
                  (treemacs-safe-button-get btn :path)))
          (buffer-to-restore (current-buffer))
          (buffer-to-kill nil)]
    (-if-let- [buffer (get-file-buffer path)]
        (switch-to-buffer buffer)
      (find-file path)
      (setq buffer-to-kill (current-buffer)))
    (when goto-tag?
      (treemacs--goto-tag btn))
    (unless treemacs--pre-peek-state
      (setq treemacs--pre-peek-state `(,(selected-window) ,buffer-to-restore ,buffer-to-kill)))
    (add-hook 'post-command-hook #'treemacs--restore-peeked-window)))

(defun treemacs--restore-peeked-window ()
  "Revert the buffer displayed in the peek window before it was used for peeking."
  (unless (memq this-command
                '(treemacs-peek treemacs-next-line-other-window treemacs-previous-line-other-window
                         treemacs-next-page-other-window treemacs-previous-page-other-window))
    (remove-hook 'post-command-hook #'treemacs--restore-peeked-window)
    (treemacs-without-following
      (when treemacs--pre-peek-state
        (-let [(window buffer-to-restore buffer-to-kill) treemacs--pre-peek-state]
          (setq treemacs--pre-peek-state nil)
          (when (buffer-live-p buffer-to-kill)
            (kill-buffer buffer-to-kill))
          (with-selected-window window
            (switch-to-buffer buffer-to-restore)))))))

(provide 'treemacs-impl)

;;; treemacs-impl.el ends here
