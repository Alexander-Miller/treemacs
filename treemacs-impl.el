;;; treemacs.el --- A tree style file viewer package -*- lexical-binding: t -*-

;; Copyright (C) 2017 Alexander Miller

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

(require 'cl-lib)
(require 'dash)
(require 's)
(require 'f)
(require 'ace-window)
(require 'vc-hooks)
(require 'pfuture)
(require 'treemacs-customization)

(defmacro treemacs--import-functions-from (file &rest functions)
  "Import FILE's FUNCTIONS."
  (declare (indent 1))
  (let ((imports (--map (list 'declare-function it file) functions)))
    `(progn ,@imports)))

(treemacs--import-functions-from "treemacs-tags"
  treemacs--clear-tags-cache
  treemacs--open-tags-for-file
  treemacs--close-tags-for-file
  treemacs--open-tag-node
  treemacs--close-tag-node
  treemacs--close-tag-node
  treemacs--goto-tag
  treemacs--remove-all-tags-under-path-from-cache)

(treemacs--import-functions-from "treemacs"
  treemacs-refresh
  treemacs-visit-node-vertical-split)

(treemacs--import-functions-from "treemacs-branch-creation"
  treemacs--button-open
  treemacs--button-close
  treemacs--check-window-system
  treemacs--create-branch)

(treemacs--import-functions-from "treemacs-filewatch-mode"
  treemacs--start-watching
  treemacs--stop-watching
  treemacs--stop-watching-all
  treemacs--cancel-refresh-timer
  treemacs--cancel-missed-refresh
  treemacs--refresh-catch-up)

(treemacs--import-functions-from "treemacs-follow-mode"
  treemacs--follow
  treemacs--without-following)

(declare-function treemacs-mode "treemacs-mode")
(declare-function projectile-project-root "projectile")

;;;;;;;;;;;;;;;;;;
;; Private vars ;;
;;;;;;;;;;;;;;;;;;

(defvar treemacs--open-dirs-cache '()
  "Cache to keep track of opened subfolders.")

(defconst treemacs--buffer-name "*Treemacs*"
  "Name of the treemacs buffer.")

(defconst treemacs-dir
  (expand-file-name (if load-file-name
                        (file-name-directory load-file-name)
                      default-directory))
  "The directory treemacs.el is stored in.")

(defvar treemacs--in-gui 'unset
  "Indicates whether Emacs is running in a gui or a terminal.")

(defvar treemacs--ready nil
  "Signals to `treemacs-follow-mode' if a follow action may be run.
Must be set to nil when no follow actions should be triggered, e.g. when the
treemacs buffer is being rebuilt or during treemacs' own window selection
functions.")

;;;;;;;;;;;;
;; Macros ;;
;;;;;;;;;;;;

(defmacro treemacs--safe-button-get (button &rest properties)
  "Safely extract BUTTON's PROPERTIES.

Using `button-get' on a button located in a buffer that is not the current
buffer does not work, so this function will run the property extaction from
inside BUTTON's buffer."
  `(with-current-buffer (marker-buffer ,button)
     ,(if (= 1 (length properties))
           `(button-get ,button ,(car properties))
         `(--map (button-get ,button it) ,properties))))

(defmacro treemacs--with-button-buffer (btn &rest body)
  "Use BTN's buffer to execute BODY.
Required for button interactions (like `button-get') that do not work when
called from another buffer than the one the button resides in and
`treemacs--safe-button-get' is not enough."
  `(with-current-buffer (marker-buffer ,btn)
    ,@body))

(defmacro treemacs--log (msg &rest args)
  "Write a log statement given formart string MSG and ARGS."
  `(message
    "%s %s"
    (propertize "[Treemacs]" 'face 'font-lock-keyword-face)
    (format ,msg ,@args)))

(cl-defmacro treemacs--execute-button-action
    (&key save-window split-function window dir-action file-action tag-action no-match-explanation)
  "Infrastructure macro for setting up actions on different button states.
Fetches the currently selected button and verifies it's in the correct state
based on the given state actions.
If it isn't it will log NO-MATCH-EXPLANATION, if it is it selects WINDOW (or
`next-window' if none is given) and splits it with SPLIT-FUNCTION if given.
DIR-ACTION, FILE-ACTION, and TAG-ACTION are inserted into a `pcase' statement
matching the buttons state."
  (let ((valid-states (list)))
    (when dir-action
      (push 'dir-node-open valid-states)
      (push 'dir-node-closed valid-states))
    (when file-action
      (push 'file-node-open valid-states)
      (push 'file-node-closed valid-states))
    (when tag-action
      (push 'tag-node valid-states))
    `(treemacs--without-following
      (let* ((btn (treemacs--current-button))
             (state (button-get btn 'state))
             (current-window (selected-window)))
        (if (not (memq state ',valid-states))
            (treemacs--log "%s" ,no-match-explanation)
          (progn
            (select-window (or ,window (next-window (selected-window) nil nil)))
            ,@(if split-function
                  `((funcall ,split-function)
                    (other-window 1)))
            (pcase state
              ,@(when dir-action
                  `(((or 'dir-node-open 'dir-node-closed)
                     ,dir-action)))
              ,@(when file-action
                  `(((or 'file-node-open 'file-node-closed)
                     ,file-action)))
              ,@(when tag-action
                  `(('tag-node
                     ,tag-action)))
              (_ (error "No match achieved even though button's state %s was part of the set of valid states %s"
                        state ',valid-states)))
            (when ,save-window
              (select-window current-window))))))))

(defmacro treemacs--with-writable-buffer (&rest body)
  "Temporarily turn off read-ony mode to execute BODY."
  `(progn
     (read-only-mode -1)
     (unwind-protect
         (progn ,@body)
       (read-only-mode t))))

(defmacro treemacs--without-messages (&rest body)
  "Temporarily turn off messages to execute BODY."
  `(cl-flet ((message (&rest _) (ignore)))
     ,@body))

;;;;;;;;;;;;;;;;;;;
;; Substitutions ;;
;;;;;;;;;;;;;;;;;;;

(defsubst treemacs--current-button ()
  "Get the button in the current line."
  (next-button (point-at-bol)))

(defsubst treemacs--get-label-of (btn)
  "Return the text label of BTN."
  (interactive)
  (buffer-substring-no-properties (button-start btn) (button-end btn)))

(defsubst treemacs--refresh-on-ui-change ()
  "Refresh the treemacs buffer when the window system has changed."
  (when (treemacs--check-window-system)
    (treemacs-refresh)))

(defsubst treemacs--add-to-cache (parent opened-child)
  "Add to PARENT's open dirs cache an entry for OPENED-CHILD."
  (let ((cache (assoc parent treemacs--open-dirs-cache)))
    (if cache
        (push opened-child (cdr cache))
      (add-to-list 'treemacs--open-dirs-cache `(,parent ,opened-child)))))

(defsubst treemacs--is-visible? ()
  "Inidicates whether the treemacs buffer is currently visible.
Will return the treemacs window if true."
  (get-buffer-window treemacs--buffer-name))

(defsubst treemacs--buffer-exists? ()
  "Indicates whether the treemacs buffer exists even if it is not visible."
  (-contains?
   (-map #'buffer-name (buffer-list))
   treemacs--buffer-name))

(defsubst treemacs--select-visible ()
  "Switch to treemacs buffer, given that it is currently visible."
  (select-window (get-buffer-window treemacs--buffer-name)))

(defsubst treemacs--select-not-visible ()
  "Switch to treemacs buffer, given that it not visible."
  (treemacs--setup-buffer)
  (switch-to-buffer treemacs--buffer-name)
  (treemacs--refresh-catch-up))

(defsubst treemacs--unqote (str)
  "Unquote STR if it is wrapped in quotes."
  (declare (pure t) (side-effect-free t))
  (if (s-starts-with? "\"" str)
      (replace-regexp-in-string "\"" "" str)
    str))

(defsubst treemacs--node-symbol-switch (new-sym)
  "Replace icon in current line with NEW-SYM."
  (let* ((b   (next-button (point-at-bol) t))
         (pos (- (button-start b) 2)))
    (save-excursion
      (goto-char pos)
      (delete-char 2)
      (insert new-sym))))

(defsubst treemacs--parse-git-status (git-future)
  "Parse the git status derived from the output of GIT-FUTURE."
  (when git-future
    (pfuture-await-to-finish git-future)
    (when (equal 0 (process-exit-status (pfuture-process git-future)))
      (let ((git-output (pfuture-result git-future)))
        (unless (s-blank? git-output)
          ;; need the actual git root since git status outputs paths relative to it
          ;; and the output must be valid also for files in dirs being reopened
          (let* ((git-root (vc-call-backend
                            'Git 'root
                            (process-get (pfuture-process git-future) 'default-directory))))
            (let ((status
                   (->> (substring git-output 0 -1)
                        (s-split "\n")
                        (--map (s-split-up-to " " (s-trim it) 1)))))
              (--each status
                (setcdr it (->> (cl-second it) (s-trim-left) (treemacs--unqote) (f-join git-root))))
              status)))))))

(defsubst treemacs--prop-at-point (prop)
  "Grab property PROP of the button at point."
  (save-excursion
    (beginning-of-line)
    (button-get (next-button (point) t) prop)))

(defsubst treemacs--is-path-in-dir? (path dir)
  "Is PATH in directory DIR?"
  (s-starts-with? (f-slash dir) path))

(defsubst treemacs--current-root ()
  "Return the current root directory.
Requires and assumes to be called inside the treemacs buffer."
  (f-long default-directory))

(defsubst treemacs--reject-ignored-files (file)
  "Return t if FILE is *not* an ignored file.
FILE here is a list consisting of an absolute path and file attributes."
  (--none? (funcall it (f-filename file)) treemacs-ignored-file-predicates))

(defsubst treemacs--reject-ignored-and-dotfiles (file)
  "Return t when FILE is neither ignored, nor a dotfile.
FILE here is a list consisting of an absolute path and file attributes."
  (let ((filename (f-filename file)))
    (and (not (s-matches? treemacs-dotfiles-regex filename))
         (--none? (funcall it (f-filename filename)) treemacs-ignored-file-predicates))))

(defsubst treemacs--get-face (path git-info)
  "Return the appropriate face for PATH GIT-INFO."
  ;; for the sake of simplicity we only look at the state in the working tree
  ;; see OUTPUT section `git help status'
  (pcase (-some-> (rassoc path git-info) (car) (substring 0 1))
    ("M" 'treemacs-git-modified-face)
    ("U" 'treemacs-git-conflict-face)
    ("?" 'treemacs-git-untracked-face)
    ("!" 'treemacs-git-ignored-face)
    ("A" 'treemacs-git-added-face)
    (_   'treemacs-git-unmodified-face)))

(defsubst treemacs--file-extension (file)
  "Same as `file-name-extension', but also works with leading periods.

This is something a of workaround to easily allow assigning icons to a FILE with
a name like '.gitignore' without always having to check for both file extensions
and special names like this."
  (let ((filename (f-filename file)))
    (save-match-data
      (if (string-match "\\.[^.]*\\'" filename)
          (substring filename (1+ (match-beginning 0)))
        filename))))

;;;;;;;;;;;;;;;
;; Functions ;;
;;;;;;;;;;;;;;;

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
    (when (equal (1+ (button-get parent-btn 'depth)) (button-get btn 'depth))
      (setq ret (cons btn ret))
      (while (setq btn (button-get btn 'next-node))
        (push btn ret)))
    (nreverse ret)))

(defun treemacs--git-status-process (path &optional recursive)
  "Create a new process future to get the git status under PATH.
Optionally make the git request RECURSIVE."
  (when treemacs-git-integration
    (let* ((default-directory (f-canonical path))
           (future (pfuture-new "git" "status" "--porcelain" "--ignored" (if recursive "-uall" "."))))
      (process-put (pfuture-process future) 'default-directory default-directory)
      future)))

(defun treemacs--init (root)
  "Initialize and build treemacs buffer for ROOT."
  (let ((origin-buffer (current-buffer)))
    (treemacs--buffer-teardown)
    (if (treemacs--is-visible?)
        (treemacs--select-visible)
      (progn
        (treemacs--setup-buffer)
        (switch-to-buffer (get-buffer-create treemacs--buffer-name))
        (bury-buffer treemacs--buffer-name)))
    ;; f-long to expand ~ and remove final slash
    ;; needed for root dirs given by projectile
    (treemacs--build-tree (f-long root))
    ;; do mode activation last - if the treemacs buffer is empty when the major
    ;; mode is activated (this may happen when treemacs is restored from other
    ;; than desktop save mode) treemacs will attempt to restore the previous session
    (treemacs-mode)
    (setq treemacs--ready t)
    ;; no warnings since follow mode is known to be defined
    (when (or treemacs-follow-after-init (with-no-warnings treemacs-follow-mode))
      (with-current-buffer origin-buffer
        (treemacs--follow)))))

(defun treemacs--build-tree (root)
  "Build the file tree starting at the given ROOT."
  (treemacs--check-window-system)
  (treemacs--with-writable-buffer
   (treemacs--delete-all)
   (treemacs--insert-header root)
   (treemacs--create-branch root 0 (treemacs--git-status-process root))
   (goto-char 0)
   (forward-line 1)
   (treemacs--evade-image)
   ;; watch must start here and not init treemacs--init: uproot calls build-tree, but not
   ;; init since init runs teardown. we want to run filewatch on the new root, so the watch *must*
   ;; be started here
   ;; same goes for reopening
   (treemacs--start-watching root)))

(defun treemacs--delete-all ()
  "Delete all content of the buffer."
  (delete-region (point-min) (point-max)))

(defun treemacs--create-header (root)
  "Use ROOT's directory name as treemacs' header."
   (format "*%s*" (f-filename root)))

(defun treemacs--create-header-projectile (root)
  "Try to use the projectile project name for ROOT as treemacs' header.
If not projectile name was found call `treemacs--create-header' for ROOT instead."
  (if (boundp 'projectile-project-name-function)
      (-if-let (project-name (condition-case nil
                                 (projectile-project-root)
                               (error nil)))
          (format "*%s*" (funcall projectile-project-name-function project-name))
        (treemacs--create-header root))
    (user-error "Couldn't create projectile header -'projectile-project-name-function' is not defined. Is projectile loaded?")))

(defun treemacs--insert-header (root)
  "Insert the header line for the given ROOT."
  (setq default-directory (f-full root))
  (insert-button (propertize (funcall treemacs-header-function root)
                             'face 'treemacs-header-face)
                 'face 'treemacs-header-face
                 'abs-path root))

(defun treemacs--buffer-teardown ()
  "Cleanup to be run when the treemacs buffer gets killed."
  (treemacs--stop-watching-all)
  (treemacs--cancel-refresh-timer)
  (treemacs--cancel-missed-refresh)
  (treemacs--clear-tags-cache)
  (setq treemacs--open-dirs-cache nil
        treemacs--ready nil))

(defun treemacs--push-button (btn &optional recursive)
  "Execute the appropriate action given the state of the pushed BTN.
Optionally do so in a RECURSIVE fashion."
  (pcase (button-get btn 'state)
    ('dir-node-open    (treemacs--close-node btn recursive))
    ('dir-node-closed  (treemacs--open-dir-node btn :recursive recursive))
    ('file-node-open   (treemacs--close-tags-for-file btn recursive))
    ('file-node-closed (treemacs--open-tags-for-file btn :recursive recursive))
    ('tag-node-open    (treemacs--close-tag-node btn recursive))
    ('tag-node-closed  (treemacs--open-tag-node btn :recursive recursive))
    ('tag-node         (treemacs--execute-button-action
                        :tag-action (treemacs--goto-tag btn)
                        :no-match-explanation "This button should be a tag"))
    (_                 (error "[Treemacs] Cannot push button with unknown state '%s'" (button-get btn 'state)))))

(defun treemacs--reopen-node (btn)
  "Reopen file BTN."
  (if (null btn)
      ;; the most likely reason for receiving a nil button here is that the undelying file has been deleted,
      ;; so we'll just throw the path out of the cache and assume that all is well
      (treemacs--clear-from-cache (button-get btn 'abs-path))
    (pcase (button-get btn 'state)
      ('dir-node-closed  (treemacs--open-dir-node btn :no-add t))
      ('file-node-closed (treemacs--open-tags-for-file btn :no-add t))
      ('tag-node-closed  (treemacs--open-tag-node btn :no-add t))
      (_                 (error "[Treemacs] Cannot reopen button at path %s with state %s"
                                (button-get btn 'abs-path) (button-get btn 'state))))))

(cl-defun treemacs--open-dir-node (btn &key no-add git-future recursive)
  "Open the node given by BTN.
Do not reopen its previously open children when NO-ADD is given.
Reuse given GIT-FUTURE when this call is RECURSIVE."
  (if (not (f-readable? (button-get btn 'abs-path)))
      (treemacs--log "Directory %s is not readable." (propertize (button-get btn 'abs-path) 'face 'font-lock-string-face))
    (let* ((abs-path (button-get btn 'abs-path))
           (git-future (or git-future (treemacs--git-status-process abs-path recursive))))
      (treemacs--button-open
       :button btn
       :new-state 'dir-node-open
       :new-icon (with-no-warnings treemacs-icon-open)
       :open-action
       (treemacs--create-branch abs-path (1+ (button-get btn 'depth)) git-future btn)
       :post-open-action
       (progn
         (unless no-add (treemacs--add-to-cache (treemacs--parent abs-path) abs-path))
         (treemacs--start-watching abs-path)))
      (when recursive
        (--each (treemacs--get-children-of btn)
          (when (eq 'dir-node-closed (button-get it 'state))
            (goto-char (button-start it))
            (treemacs--open-dir-node
             it :git-future git-future :recursive t)))))))

(defun treemacs--close-node (btn recursive)
  "Close node given by BTN.
Remove all open dir and tag entries under BTN when RECURSIVE."
  (treemacs--button-close
   :button btn
   :new-state 'dir-node-closed
   :new-icon (with-no-warnings treemacs-icon-closed)
   :post-close-action
   (let ((path (button-get btn 'abs-path)))
     (treemacs--stop-watching path)
     (when recursive (treemacs--remove-all-tags-under-path-from-cache path))
     (treemacs--clear-from-cache path recursive))))


(defun treemacs--open-file (&optional window split-func)
  "Visit file of the current node.  Split WINDOW using SPLIT-FUNC.
Do nothing if current node is a directory.
Do not split window if SPLIT-FUNC is nil.
Use `next-window' if WINDOW is nil."
  (let* ((path     (treemacs--prop-at-point 'abs-path))
         (is-file? (f-file? path)))
    (when is-file?
      (select-window (or window (next-window)))
      (when split-func
        (call-interactively split-func)
        (call-interactively 'other-window))
      (find-file path))))

(defun treemacs--reopen-at (path)
  "Reopen dirs below PATH."
  (treemacs--without-messages
   (-some->
    path
    (assoc treemacs--open-dirs-cache)
    (cdr)
    (treemacs--maybe-filter-dotfiles)
    (--each (treemacs--reopen-node (treemacs--goto-button-at it))))))

(defun treemacs--clear-from-cache (path &optional purge)
  "Remove PATH from the open dirs cache.
Also remove any dirs below if PURGE is given."
  (let* ((parent (treemacs--parent path))
         (cache  (assoc parent treemacs--open-dirs-cache))
         (values (cdr cache)))
    (when values
      (if (= 1 (length values))
          (setq treemacs--open-dirs-cache (delete cache treemacs--open-dirs-cache))
        (setcdr cache (delete path values))))
    (when purge
      ;; recursively grab all nodes open below PATH and remove them too
      (-if-let (children
                (->> values
                     (--map (cdr (assoc it treemacs--open-dirs-cache)))
                     (-flatten)))
          (--each children (treemacs--clear-from-cache it t))))))

(defun treemacs--nearest-path (btn)
  "Return the 'abs-path' property of the current button (or BTN).
If the property is not set keep looking upward, via the 'parent' property.
Useful to e.g. find the path of the file of the currently selected tags entry.
Must be called from treemacs buffer."
  (let* ((path (button-get btn 'abs-path)))
    (while (null path)
      (setq btn (button-get btn 'parent)
            path (button-get btn 'abs-path)))
    path))

(cl-defun treemacs--goto-button-at (abs-path &optional (start-from (point-min)))
  "Move point to button identified by ABS-PATH, starting search at START.
Also return that button.
Callers must make sure to save match data"
  (let ((keep-looking t)
        (filename (f-filename abs-path))
        (start (point))
        (ret))
    (goto-char start-from)
    (while (and keep-looking
                (search-forward filename nil t))
      (beginning-of-line)
      (let ((btn (next-button (point) t)))
        (if (s-equals? abs-path (button-get btn 'abs-path))
            (progn (treemacs--evade-image)
                   (setq keep-looking nil
                         ret btn))
          (beginning-of-line 2))))
    (unless ret (goto-char start))
    ret))

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

(defun treemacs--std-ignore-file-predicate (file)
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
   ((treemacs--is-visible?)    'visible)
   ((treemacs--buffer-exists?) 'exists)
   (t 'none)))

(defun treemacs--current-root-btn ()
  "Return the current root button."
  (save-excursion
    (goto-char (point-min))
    (next-button (point) t)))

(defun treemacs--setup-buffer ()
  "Create and setup a buffer for treemacs in the right position and size."
  (select-window
   (display-buffer-in-side-window
    (get-buffer-create treemacs--buffer-name) '((side . left)))
   t)
  (treemacs--set-width treemacs-width)
  (let ((window-size-fixed))
    (set-window-dedicated-p (get-buffer-window) t)))

(defun treemacs--non-child-node (btn)
  "Return the next node after BTN that is not a child of BTB."
  (when btn
    (-if-let (next (button-get btn 'next-node))
        next
      (treemacs--non-child-node (button-get next 'parent)))))

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
    (f-parent path)))

(defun treemacs--evade-image ()
  "The cursor visibly blinks when on top of an icon.
It needs to be moved aside in a way that works for all indent depths and
`treemacs-indentation' settings."
  (beginning-of-line)
  (when (get-text-property (point) 'display)
    (forward-char 1)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Winum Compatibility ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(with-eval-after-load 'winum

  ;; somestimes the compiler asks for the strangest things
  (declare-function treemacs--window-number-zero "treemacs-impl")

  (defun treemacs--window-number-zero ()
    (when (s-equals? (buffer-name) treemacs--buffer-name) 10))

  (when (boundp 'winum-assign-func)
    (setq winum-assign-func #'treemacs--window-number-zero)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Popwin Compatibility ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-eval-after-load 'popwin

  (defadvice popwin:create-popup-window
      (around treemacs--popwin-popup-buffer activate)
    (let ((v? (treemacs--is-visible?))
          (tb (get-buffer treemacs--buffer-name)))
      (when v?
        (with-current-buffer tb
          (setq window-size-fixed nil)))
      ad-do-it
      (when v?
        (with-current-buffer tb
          (setq window-size-fixed 'width)))))

  (defadvice popwin:close-popup-window
      (around treemacs--popwin-close-buffer activate)
    (let ((v? (treemacs--is-visible?))
          (tb (get-buffer treemacs--buffer-name)))
      (when v?
        (with-current-buffer tb
          (setq window-size-fixed nil)))
      ad-do-it
      (when v?
        (with-current-buffer tb
          (setq window-size-fixed 'width))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Golden Ratio Compatibility ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-eval-after-load 'golden-ratio
  (when (bound-and-true-p golden-ratio-exclude-modes)
    (add-to-list 'golden-ratio-exclude-modes 'treemacs-mode)))

(provide 'treemacs-impl)

;;; treemacs-impl.el ends here
