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
  treemacs--expand-tags-for-file
  treemacs--collapse-tags-for-file
  treemacs--expand-tag-node
  treemacs--collapse-tag-node
  treemacs--goto-tag
  treemacs--tags-path-of
  treemacs--goto-tag-button-at)

(treemacs-import-functions-from "treemacs"
  treemacs-refresh)

(treemacs-import-functions-from "treemacs-branch-creation"
  treemacs--check-window-system
  treemacs--expand-dir-node
  treemacs--collapse-dir-node
  treemacs--create-branch)

(treemacs-import-functions-from "treemacs-filewatch-mode"
  treemacs--start-watching
  treemacs--stop-filewatch-for-current-buffer
  treemacs--stop-watching
  treemacs--cancel-refresh-timer)

(treemacs-import-functions-from "treemacs-follow-mode"
  treemacs--follow
  treemacs--do-follow)

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
  treemacs--reset-index
  treemacs--on-rename
  treemacs--invalidate-position-cache)

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

(defvar treemacs--defaults-icons nil
  "Stores the default values of the directory and tag icons.
Maps icons' names as symbols to their values, so that they can be queried
via `assq'.")

;;;;;;;;;;;;;;;;;;;
;; Substitutions ;;
;;;;;;;;;;;;;;;;;;;

(defsubst treemacs-current-button ()
  "Get the button in the current line.
Returns nil when point is on the header."
  (if (get-text-property (point-at-bol) 'button)
      (button-at (point-at-bol))
    (let ((p (next-single-property-change (point-at-bol) 'button nil (point-at-eol))))
      (when (and (get-char-property p 'button))
          (copy-marker p t)))))

(defsubst treemacs--unslash (path)
  "Remove the final slash in PATH."
  (declare (pure t) (side-effect-free t))
  (if (eq ?/ (aref path (1- (length path))))
      (substring path 0 -1)
    path))

(defsubst treemacs--get-label-of (btn)
  "Return the text label of BTN."
  (interactive)
  (buffer-substring-no-properties (button-start btn) (button-end btn)))

(defsubst treemacs--is-path-in-dir? (path dir)
  "Is PATH in directory DIR?"
  (s-starts-with? (f-slash dir) path))

(defsubst treemacs--replace-hash-entries (table make-new-key make-new-val)
  "Selectively replace keys and values in a given hash TABLE.
Use MAKE-NEW-KEY and MAKE-NEW-VAL to create a new key from the old - both are
functions that take the key/value as its argument and return the new key/value."
  (ht-each
   (lambda (k v)
     (ht-remove! table k)
     (ht-set table (funcall make-new-key k) (funcall make-new-val v)))
   table))

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

(defsubst treemacs--select-visible ()
  "Switch to treemacs buffer, given that it is currently visible."
  (->> treemacs--buffer-access
       (assoc (selected-frame))
       (cdr)
       (get-buffer-window)
       (select-window)))

(defsubst treemacs--select-not-visible ()
  "Switch to treemacs buffer, given that it not visible."
  (treemacs--setup-buffer))

(defsubst treemacs--unqote (str)
  "Unquote STR if it is wrapped in quotes."
  (declare (pure t) (side-effect-free t))
  (if (s-starts-with? "\"" str)
      (replace-regexp-in-string "\"" "" str)
    str))

(defsubst treemacs--button-symbol-switch (new-sym)
  "Replace icon in current line with NEW-SYM."
  (let* ((b   (next-button (point-at-bol) t))
         (pos (- (button-start b) 2)))
    (save-excursion
      (goto-char pos)
      (delete-char 2)
      (insert new-sym))))

(defsubst treemacs--prop-at-point (prop)
  "Grab property PROP of the button at point.
Returns nil when point is on the header."
  (-when-let (b (treemacs-current-button))
    (button-get b prop)))

(defsubst treemacs--current-root ()
  "Return the current root directory.
Requires and assumes to be called inside the treemacs buffer."
  (treemacs--unslash (f-long default-directory)))

(defsubst treemacs-parent-of (btn)
  "Return the parent path of BTN."
  (--if-let (button-get btn :parent)
      (button-get it :path)
    (treemacs--current-root)))

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

(defsubst treemacs--next-neighbour (btn)
  "Get the next same-level neighbour of BTN, if any."
  (declare (side-effect-free t))
  (-let- [(depth (button-get btn :depth))
          (next (next-button (button-end btn)))]
    (while (and next (< depth (button-get next :depth)))
      (setq next (next-button (button-end next))))
    (when (and next (= depth (button-get next :depth))) next)))

(defsubst treemacs--prev-neighbour (btn)
  "Get the previous same-level neighbour of BTN, if any."
  (declare (side-effect-free t))
  (-let- [(depth (button-get btn :depth))
          (prev (previous-button (button-start btn)))]
    (while (and prev (< depth (button-get prev :depth)))
      (setq prev (previous-button (button-start prev))))
    (when (= depth (button-get prev :depth)) prev)))

(defsubst treemacs--next-non-child-button (btn)
  "Return the next button after BTN that is not a child of BTB."
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
  (-let [btn (treemacs--goto-button-at path)]
    (when (memq (button-get btn :state) '(dir-node-open file-node-open))
      (goto-char (button-start btn))
      (treemacs--push-button btn)
      (goto-char (button-start btn))
      (treemacs--push-button btn))))

;;;;;;;;;;;;;;;
;; Functions ;;
;;;;;;;;;;;;;;;

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
    (let ((root (treemacs--current-root)))
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
      (while (setq btn (treemacs--next-neighbour btn))
        (push btn ret)))
    (nreverse ret)))

(defun treemacs--init (root)
  "Initialize and build treemacs buffer for ROOT."
  (-let [origin-buffer (current-buffer)]
    (if (treemacs--is-visible?)
        (treemacs--select-visible)
      (treemacs--setup-buffer))
    (unless (eq major-mode 'treemacs-mode)
      (treemacs-mode))
    (treemacs--buffer-teardown (treemacs--unslash (f-long root)))
    (treemacs--check-window-system)
    (treemacs--build-tree (treemacs--unslash (f-long root)))
    ;; no warnings since follow mode is known to be defined
    (with-no-warnings (setq treemacs--ready-to-follow t))
    (when (or treemacs-follow-after-init (with-no-warnings treemacs-follow-mode))
      (with-current-buffer origin-buffer
        (treemacs--follow)))))

(defun treemacs--build-tree (root)
  "Build the file tree starting at the given ROOT."
  (treemacs--invalidate-position-cache)
  (treemacs--forget-last-highlight)
  (treemacs--forget-previously-follow-tag-btn)
  (treemacs--stop-filewatch-for-current-buffer)
  (treemacs--cancel-refresh-timer)
  (treemacs-with-writable-buffer
   (delete-region (point-min) (point-max))
   (treemacs--insert-header root)
   (treemacs--create-branch root 0
                            (treemacs--git-status-process-function root)
                            (treemacs--collapsed-dirs-process root))
   (goto-char 0)
   (forward-line 1)
   (treemacs--evade-image)
   ;; watch must start here and not in `treemacs--init': uproot calls build-tree, but not
   ;; init since init runs teardown. we want to run filewatch on the new root, so the watch *must*
   ;; be started here
   ;; same goes for reopening
   (treemacs--start-watching root)))

(defun treemacs--create-header (root)
  "Use ROOT's directory name as treemacs' header."
   (format "*%s*" (f-filename root)))

(defun treemacs--insert-header (root)
  "Insert the header line for the given ROOT."
  (setq default-directory (f-full root))
  (insert (propertize (funcall treemacs-header-function root)
                      'face 'treemacs-header-face)))

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

(defun treemacs--buffer-teardown (root)
  "Cleanup to be run when an existing treemacs buffer is re-initialized at ROOT."
  (treemacs--reset-index root)
  (treemacs--stop-filewatch-for-current-buffer)
  (treemacs--cancel-refresh-timer)
  (treemacs--forget-last-highlight))

(defun treemacs--push-button (btn &optional recursive)
  "Execute the appropriate action given the state of the pushed BTN.
Optionally do so in a RECURSIVE fashion."
  (-pcase (button-get btn :state)
    [`dir-node-open    (treemacs--collapse-dir-node btn recursive)]
    [`dir-node-closed  (treemacs--expand-dir-node btn :recursive recursive)]
    [`file-node-open   (treemacs--collapse-tags-for-file btn recursive)]
    [`file-node-closed (treemacs--expand-tags-for-file btn recursive)]
    [`tag-node-open    (treemacs--collapse-tag-node btn recursive)]
    [`tag-node-closed  (treemacs--expand-tag-node btn recursive)]
    [`tag-node         (progn (other-window 1) (treemacs--goto-tag btn))]
    [_                 (error "[Treemacs] Cannot push button with unknown state '%s'" (button-get btn :state))]))

(defun treemacs--reopen-node (btn git-info)
  "Reopen file BTN.
GIT-INFO is passed through from the previous branch build."
  (-pcase (button-get btn :state)
    [`dir-node-closed  (treemacs--expand-dir-node btn :git-future git-info)]
    [`file-node-closed (treemacs--expand-tags-for-file btn)]
    [`tag-node-closed  (treemacs--expand-tag-node btn :no-add t)]
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
     (treemacs--reopen-node (treemacs--goto-button-at it) git-info))))

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

(defun treemacs--create-file/dir (prompt creation-func)
  "Concrete implementation of file & dir creation.
Use PROMPT to ask for a location and CREATION-FUNC to create a new dir/file.
PROMPT: String
CREATION-FUNC: `f-touch' | `f-mkdir'"
  (interactive)
  (let ((btn (treemacs-current-button))
        (curr-path)
        (location)
        (name))
    (cl-block body
      (if (null btn)
          (f-slash (treemacs--current-root))
        (let ((path (treemacs--nearest-path btn)))
          (setq curr-path (f-slash (if (f-dir? path)
                                       path
                                     (f-dirname path))))))
      (setq location (read-directory-name "Create in: " curr-path))
      (when (not (f-directory? location))
        (cl-return-from body
          (treemacs-log "%s is not a directory."
                         (propertize location 'face 'font-lock-string-face))))
      (setq name (read-string prompt))
      (let ((new-file (f-join location name)))
        (when (f-exists? new-file)
          (cl-return-from body
            (treemacs-log "%s already exists."
                           (propertize  'face 'font-lock-string-face))))
        (treemacs--without-filewatch (funcall creation-func new-file))
        (treemacs-without-messages (treemacs-refresh))
        (treemacs--do-follow (f-long new-file))
        (recenter)
        (treemacs-pulse-on-success)))))

(cl-defun treemacs--uncached-goto-button-at (abs-path &optional (start-from (point-min)))
  "Move point to node identified by ABS-PATH, starting search at START.
Also return that node.
Unlike `treemacs--goto-button-at' this function does not make use of
`treemacs--open-node-position-cache', which means 2 things: 1) It is
considerably slower, and its use should thus be avoided, and 2) It can be used
in times when the node position cache is invalidated, like the reopen phase of
a refresh."
  (-let- [(filename (f-filename abs-path))
          (start (point))
          (result nil)]
    (goto-char start-from)
    (cl-block search
      (save-match-data
        (while (search-forward filename nil t)
          (beginning-of-line)
          (-let*- [(btn (next-button (point) t))
                   (btn-path (button-get btn :path))]
            (if (or (s-equals? abs-path btn-path)
                    ;; loosen matching for collapsed paths
                    (and (button-get btn :collapsed)
                         (treemacs--is-path-in-dir? btn-path abs-path)))
                (progn (treemacs--evade-image)
                       (cl-return-from search (setq result btn)))
              (beginning-of-line 2)))))
      (unless result (goto-char start))
      result)))

(cl-defun treemacs--goto-button-at (abs-path)
  "Move point to button identified by ABS-PATH, also return that button."
  (-let [parent (treemacs--unslash (file-name-directory abs-path))]
    (-if-let- [btn-pos (treemacs-get-position-of parent)]
        (treemacs--uncached-goto-button-at abs-path btn-pos)
      (goto-char (point-min))
      (-let [btn (next-button (point) t)]
        (cl-block search
          (while btn
            (if (string= abs-path (button-get btn :path))
                (progn
                  (goto-char (button-start btn))
                  (cl-return-from search btn))
              (setq btn (next-button (button-end btn))))))))))

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
  "Set the width of the treemacs buffer to WIDTH when it is created."
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
  (treemacs--forget-last-highlight)
  (-> (selected-window)
      (frame-root-window)
      (split-window nil treemacs-position)
      (select-window))
  (let ((buf (treemacs--get-framelocal-buffer)))
    (switch-to-buffer buf)
    (bury-buffer buf))
  (set-window-dedicated-p (selected-window) t)
  (let ((window-size-fixed))
    (treemacs--set-width treemacs-width)))

(defun str-assq-delete-all (key alist)
  "Same as `assq-delete-all', but use `string=' instead of `eq'.
Delete all elements whose car is ‘string=’ to KEY from ALIST."
  (while (and (consp (car alist))
              (string= (car (car alist)) key))
    (setq alist (cdr alist)))
  (let ((tail alist) tail-cdr)
    (while (setq tail-cdr (cdr tail))
      (if (and (consp (car tail-cdr))
               (string= (car (car tail-cdr)) key))
          (setcdr tail (cdr tail-cdr))
        (setq tail tail-cdr))))
  alist)

(defun treemacs--parent (path)
  "Parent of PATH, or PATH itself if PATH is the root directory."
  (if (f-root? path)
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

(defun treemacs--sort-value-selection ()
  "Interactive selection for a new `treemacs-sorting' value.
Retursns a cons cell of a descriptive string name and the sorting symbol."
  (declare (side-effect-free t))
  (let* ((sort-names '(("Sort Alphabetically Ascending" . alphabetic-asc)
                       ("Sort Alphabetically Descending" . alphabetic-desc)
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

(defun treemacs--do-refresh (buffer)
  "Execute the refresh process for BUFFER.
Specifically extracted with the buffer to refresh being supplied so that
filewatch mode can refresh multiple buffers at once."
  (with-current-buffer buffer
    (-let [root (treemacs--current-root)]
      (treemacs-save-position
       (progn
         (treemacs--cancel-refresh-timer)
         (run-hook-with-args
          'treemacs-pre-refresh-hook
          root curr-line curr-btn curr-state curr-file curr-tagpath curr-winstart)
         (treemacs--build-tree root))

       (run-hook-with-args
        'treemacs-post-refresh-hook
        root curr-line curr-btn curr-state curr-file curr-tagpath curr-winstart)
       (unless treemacs-silent-refresh
         (treemacs-log "Refresh complete."))))))

(defun treemacs--maybe-recenter ()
  "Potentially recenter after following a file or tag.
The answer depends on the distance between `point' and the window top/bottom
being smaller than `treemacs-follow-recenter-distance'."
  (interactive);;
  (-let*- [(current-line (float (count-lines (window-start) (point))))
           (all-lines (float (window-height)))
           (distance-from-top (/ current-line all-lines))
           (distance-from-bottom (- 1.0 distance-from-top))]
    (when (or (> treemacs-follow-recenter-distance distance-from-top)
              (> treemacs-follow-recenter-distance distance-from-bottom))
      (recenter))))

(provide 'treemacs-impl)

;;; treemacs-impl.el ends here
