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

;; Not autoloaded, but user-facing functions.

;;; Code:

(require 'hl-line)
(require 'button)
(require 's)
(require 'dash)
(require 'treemacs-core-utils)
(require 'treemacs-filewatch-mode)
(require 'treemacs-rendering)
(require 'treemacs-scope)
(require 'treemacs-follow-mode)
(require 'treemacs-customization)
(require 'treemacs-workspaces)
(require 'treemacs-persistence)
(require 'treemacs-extensions)
(require 'treemacs-logging)

(eval-when-compile
  (require 'cl-lib)
  (require 'treemacs-macros))

(autoload 'ansi-color-apply-on-region "ansi-color")
(autoload 'aw-select "ace-window")

(treemacs-import-functions-from "cfrs"
  cfrs-read)

(treemacs-import-functions-from "treemacs"
  treemacs-find-file
  treemacs-select-window)

(treemacs-import-functions-from "treemacs-tags"
  treemacs--expand-file-node
  treemacs--collapse-file-node
  treemacs--expand-tag-node
  treemacs--collapse-tag-node
  treemacs--goto-tag
  treemacs--visit-or-expand/collapse-tag-node)

(defvar treemacs-valid-button-states
  '(root-node-open
    root-node-closed
    dir-node-open
    dir-node-closed
    file-node-open
    file-node-closed
    tag-node-open
    tag-node-closed
    tag-node)
  "List of all valid values for treemacs buttons' :state property.")

(defun treemacs-next-line (&optional count)
  "Go to next line.
A COUNT argument, moves COUNT lines down."
  (interactive "p")
  ;; Move to EOL - if point is in the middle of a button, forward-button
  ;; just moves to the end of the current button.
  (goto-char (line-end-position))
  ;; Don't show the "No more buttons" message.
  (ignore-errors
    (forward-button count treemacs-wrap-around))
  ;; Move to BOL, since the button might not start at BOL, but parts
  ;; of Treemacs might expect that the point is always at BOL.
  (forward-line 0)
  (treemacs--evade-image))

(defun treemacs-previous-line (&optional count)
  "Go to previous line.
A COUNT argument, moves COUNT lines up."
  (interactive "p")
  ;; Move to the start of line - if point is in the middle of a button,
  ;; backward-button just moves to the start of the current button.
  (forward-line 0)
  ;; Don't show the "No more buttons" message.
  (ignore-errors
    (backward-button count treemacs-wrap-around))
  ;; Move to BOL, since backward-button moves to the end of the button,
  ;; and the button might not start at BOL, but parts of Treemacs might
  ;; expect that the point is always at BOL.
  (forward-line 0)
  (treemacs--evade-image))

(defun treemacs-toggle-node (&optional arg)
  "Expand or close the current node.
If a prefix ARG is provided the open/close process is done recursively.  When
opening directories that means that all sub-directories are opened as well.
When opening files all their tag sections will be opened.
Recursively closing any kind of node means that treemacs will forget about
everything that was expanded below that node.

Since tags cannot be opened or closed a goto definition action will called on
them instead."
  (interactive "P")
  (treemacs-do-for-button-state
   :on-root-node-open   (treemacs--collapse-root-node btn arg)
   :on-root-node-closed (treemacs--expand-root-node btn arg)
   :on-dir-node-open    (treemacs--collapse-dir-node btn arg)
   :on-dir-node-closed  (treemacs--expand-dir-node btn :recursive arg)
   :on-file-node-open   (treemacs--collapse-file-node btn arg)
   :on-file-node-closed (treemacs--expand-file-node btn arg)
   :on-tag-node-open    (treemacs--collapse-tag-node btn arg)
   :on-tag-node-closed  (treemacs--expand-tag-node btn arg)
   :on-tag-node-leaf    (progn (other-window 1) (treemacs--goto-tag btn))
   :on-nil              (treemacs-pulse-on-failure "There is nothing to do here.")))

(defun treemacs-toggle-node-prefer-tag-visit (&optional arg)
  "Same as `treemacs-toggle-node' but will visit a tag node in some conditions.
Tag nodes, despite being expandable sections, will be visited in the following
conditions:

 * Tags belong to a .py file and the tag section's first child element's label
   ends in \" definition*\". This indicates the section is the parent element in
   a nested class/function definition and can be moved to.
 * Tags belong to a .org file and the tag section element possesses a
   'org-imenu-marker text property. This indicates that the section is a
   headline with further org elements below it.

The prefix argument ARG is treated the same way as with `treemacs-toggle-node'."
  (interactive)
  (treemacs-do-for-button-state
   :on-root-node-open   (treemacs--collapse-root-node btn arg)
   :on-root-node-closed (treemacs--expand-root-node btn)
   :on-dir-node-open    (treemacs--collapse-dir-node btn arg)
   :on-dir-node-closed  (treemacs--expand-dir-node btn :recursive arg)
   :on-file-node-open   (treemacs--collapse-file-node btn arg)
   :on-file-node-closed (treemacs--expand-file-node btn arg)
   :on-tag-node-open    (treemacs--visit-or-expand/collapse-tag-node btn arg t)
   :on-tag-node-closed  (treemacs--visit-or-expand/collapse-tag-node btn arg t)
   :on-tag-node-leaf    (progn (other-window 1) (treemacs--goto-tag btn))
   :on-nil              (treemacs-pulse-on-failure "There is nothing to do here.")))

(defun treemacs-TAB-action (&optional arg)
  "Run the appropriate TAB action for the current node.

In the default configuration this usually means to expand or close the content
of the currently selected node.  A potential prefix ARG is passed on to the
executed action, if possible.

This function's exact configuration is stored in `treemacs-TAB-actions-config'."
  (interactive "P")
  (-when-let (state (treemacs--prop-at-point :state))
    (--if-let (cdr (assq state treemacs-TAB-actions-config))
        (progn
          (funcall it arg)
          (treemacs--evade-image))
      (treemacs-pulse-on-failure "No TAB action defined for node of type %s."
        (propertize (format "%s" state) 'face 'font-lock-type-face)))))

(defun treemacs-goto-parent-node (&optional _arg)
  "Select parent of selected node, if possible.

ARG is optional and only available so this function can be used as an action."
  (interactive)
  (--if-let (-some-> (treemacs-current-button) (treemacs-button-get :parent))
      (goto-char it)
    (treemacs-pulse-on-failure "There is no parent to move up to.")))

(defun treemacs-next-neighbour ()
  "Select next node at the same depth as currently selected node, if possible."
  (interactive)
  (or (-some-> (treemacs-current-button)
               (treemacs--next-neighbour-of)
               (goto-char))
      (treemacs-pulse-on-failure)))

(defun treemacs-previous-neighbour ()
  "Select previous node at the same depth as currently selected node, if possible."
  (interactive)
  (or (-some-> (treemacs-current-button)
               (treemacs--prev-non-child-button)
               (goto-char))
      (treemacs-pulse-on-failure)))

(defun treemacs-visit-node-vertical-split (&optional arg)
  "Open current file or tag by vertically splitting `next-window'.
Stay in current window with a prefix argument ARG."
  (interactive "P")
  (treemacs--execute-button-action
   :split-function #'split-window-vertically
   :file-action (find-file (treemacs-safe-button-get btn :path))
   :dir-action (dired (treemacs-safe-button-get btn :path))
   :tag-section-action (treemacs--visit-or-expand/collapse-tag-node btn arg nil)
   :tag-action (treemacs--goto-tag btn)
   :save-window arg
   :no-match-explanation "Node is neither a file, a directory or a tag - nothing to do here."))

(defun treemacs-visit-node-horizontal-split (&optional arg)
  "Open current file or tag by horizontally splitting `next-window'.
Stay in current window with a prefix argument ARG."
  (interactive "P")
  (treemacs--execute-button-action
   :split-function #'split-window-horizontally
   :file-action (find-file (treemacs-safe-button-get btn :path))
   :dir-action (dired (treemacs-safe-button-get btn :path))
   :tag-section-action (treemacs--visit-or-expand/collapse-tag-node btn arg nil)
   :tag-action (treemacs--goto-tag btn)
   :save-window arg
   :no-match-explanation "Node is neither a file, a directory or a tag - nothing to do here."))

(defun treemacs-visit-node-no-split (&optional arg)
  "Open current file or tag within the window the file is already opened in.
If the file/tag is no visible opened in any window use `next-window' instead.
Stay in current window with a prefix argument ARG."
  (interactive "P")
  (treemacs--execute-button-action
   :file-action (find-file (treemacs-safe-button-get btn :path))
   :dir-action (dired (treemacs-safe-button-get btn :path))
   :tag-section-action (treemacs--visit-or-expand/collapse-tag-node btn arg nil)
   :tag-action (treemacs--goto-tag btn)
   :save-window arg
   :ensure-window-split t
   :window  (-some-> btn (treemacs--nearest-path) (get-file-buffer) (get-buffer-window))
   :no-match-explanation "Node is neither a file, a directory or a tag - nothing to do here."))

(defun treemacs-visit-node-ace (&optional arg)
  "Open current file or tag in window selected by `ace-window'.
Stay in current window with a prefix argument ARG."
  (interactive "P")
  (treemacs--execute-button-action
   :window (aw-select "Select window")
   :file-action (find-file (treemacs-safe-button-get btn :path))
   :dir-action (dired (treemacs-safe-button-get btn :path))
   :tag-section-action (treemacs--visit-or-expand/collapse-tag-node btn arg nil)
   :tag-action (treemacs--goto-tag btn)
   :save-window arg
   :ensure-window-split t
   :no-match-explanation "Node is neither a file, a directory or a tag - nothing to do here."))

(defun treemacs-visit-node-in-most-recently-used-window (&optional arg)
  "Open current file or tag in window selected by `get-mru-window'.
Stay in current window with a prefix argument ARG."
  (interactive "P")
  (treemacs--execute-button-action
   :window (get-mru-window (selected-frame) nil :not-selected)
   :file-action (find-file (treemacs-safe-button-get btn :path))
   :dir-action (dired (treemacs-safe-button-get btn :path))
   :tag-section-action (treemacs--visit-or-expand/collapse-tag-node btn arg nil)
   :tag-action (treemacs--goto-tag btn)
   :save-window arg
   :ensure-window-split t
   :no-match-explanation "Node is neither a file, a directory or a tag - nothing to do here."))

(defun treemacs-visit-node-ace-horizontal-split (&optional arg)
  "Open current file by horizontally splitting window selected by `ace-window'.
Stay in current window with a prefix argument ARG."
  (interactive "P")
  (treemacs--execute-button-action
   :split-function #'split-window-horizontally
   :window (aw-select "Select window")
   :file-action (find-file (treemacs-safe-button-get btn :path))
   :dir-action (dired (treemacs-safe-button-get btn :path))
   :tag-section-action (treemacs--visit-or-expand/collapse-tag-node btn arg nil)
   :tag-action (treemacs--goto-tag btn)
   :save-window arg
   :no-match-explanation "Node is neither a file, a directory or a tag - nothing to do here."))

(defun treemacs-visit-node-ace-vertical-split (&optional arg)
  "Open current file by vertically splitting window selected by `ace-window'.
Stay in current window with a prefix argument ARG."
  (interactive "P")
  (treemacs--execute-button-action
   :split-function #'split-window-vertically
   :window (aw-select "Select window")
   :file-action (find-file (treemacs-safe-button-get btn :path))
   :dir-action (dired (treemacs-safe-button-get btn :path))
   :tag-section-action (treemacs--visit-or-expand/collapse-tag-node btn arg nil)
   :tag-action (treemacs--goto-tag btn)
   :save-window arg
   :no-match-explanation "Node is neither a file, a directory or a tag - nothing to do here."))

(defun treemacs-visit-node-default (&optional arg)
  "Run `treemacs-default-visit-action' for the current button.
A potential prefix ARG is passed on to the executed action, if possible."
  (interactive "P")
  (funcall-interactively treemacs-default-visit-action arg))

(defun treemacs-RET-action (&optional arg)
  "Run the appropriate RET action for the current button.

In the default configuration this usually means to open the content of the
currently selected node.  A potential prefix ARG is passed on to the executed
action, if possible.

This function's exact configuration is stored in `treemacs-RET-actions-config'."
  (interactive "P")
  (-when-let (state (treemacs--prop-at-point :state))
    (--if-let (cdr (assq state treemacs-RET-actions-config))
        (progn
          (funcall it arg)
          (treemacs--evade-image))
      (treemacs-pulse-on-failure "No RET action defined for node of type %s."
        (propertize (format "%s" state) 'face 'font-lock-type-face)))))

(defun treemacs-define-RET-action (state action)
  "Define the behaviour of `treemacs-RET-action'.
Determines that a button with a given STATE should lead to the execution of
ACTION.
The list of possible states can be found in `treemacs-valid-button-states'.
ACTION should be one of the `treemacs-visit-node-*' commands."
  (setf treemacs-RET-actions-config (assq-delete-all state treemacs-RET-actions-config))
  (push (cons state action) treemacs-RET-actions-config))

(defun treemacs-define-TAB-action (state action)
  "Define the behaviour of `treemacs-TAB-action'.
Determines that a button with a given STATE should lead to the execution of
ACTION.
The list of possible states can be found in `treemacs-valid-button-states'.
ACTION should be one of the `treemacs-visit-node-*' commands."
  (setf treemacs-TAB-actions-config (assq-delete-all state treemacs-TAB-actions-config))
  (push (cons state action) treemacs-TAB-actions-config))

(defun treemacs-COLLAPSE-action (&optional arg)
  "Run the appropriate COLLAPSE action for the current button.

In the default configuration this usually means to close the content of the
currently selected node.  A potential prefix ARG is passed on to the executed
action, if possible.

This function's exact configuration is stored in `treemacs-COLLAPSE-actions-config'."
  (interactive "P")
  (-when-let (state (treemacs--prop-at-point :state))
    (--if-let (cdr (assq state treemacs-COLLAPSE-actions-config))
      (progn
        (funcall it arg)
        (treemacs--evade-image))
      (treemacs-pulse-on-failure "No COLLAPSE action defined for node of type %s."
        (propertize (format "%s" state) 'face 'font-lock-type-face)))))

(defun treemacs-define-COLLAPSE-action (state action)
  "Define the behaviour of `treemacs-COLLAPSE-action'.
Determines that a button with a given STATE should lead to the execution of
ACTION.
The list of possible states can be found in `treemacs-valid-button-states'.
ACTION should be one of the `treemacs-visit-node-*' commands."
  (setf treemacs-COLLAPSE-actions-config (assq-delete-all state treemacs-COLLAPSE-actions-config))
  (push (cons state action) treemacs-COLLAPSE-actions-config))

(defun treemacs-visit-node-in-external-application ()
  "Open current file according to its mime type in an external application.
Treemacs knows how to open files on linux, windows and macos."
  (interactive)
  ;; code adapted from ranger.el
  (-if-let (path (treemacs--prop-at-point :path))
      (pcase system-type
        ('windows-nt
         (declare-function w32-shell-execute "w32fns.c")
         (w32-shell-execute "open" (replace-regexp-in-string "/" "\\" path t t)))
        ('darwin
         (shell-command (format "open \"%s\"" path)))
        ('gnu/linux
         (let (process-connection-type)
           (start-process
            "" nil "sh" "-c"
            ;; XXX workaround for #633
            (format "xdg-open %s; sleep 1"
                    (shell-quote-argument path)))))
        (_ (treemacs-pulse-on-failure "Don't know how to open files on %s."
             (propertize (symbol-name system-type) 'face 'font-lock-string-face))))
    (treemacs-pulse-on-failure "Nothing to open here.")))

(defun treemacs-quit (&optional arg)
  "Quit treemacs with `bury-buffer'.
With a prefix ARG call `treemacs-kill-buffer' instead."
  (interactive "P")
  (if arg
      (treemacs-kill-buffer)
    (bury-buffer)
    (run-hooks 'treemacs-quit-hook)))

(defun treemacs-kill-buffer ()
  "Kill the treemacs buffer."
  (interactive)
  (when treemacs--in-this-buffer
    ;; teardown logic handled in kill hook
    (if (one-window-p)
        (kill-this-buffer)
      (kill-buffer-and-window))
    (run-hooks 'treemacs-kill-hook)))

(defun treemacs-delete (&optional arg)
  "Delete node at point.
A delete action must always be confirmed.  Directories are deleted recursively.
By default files are deleted by moving them to the trash.  With a prefix ARG
they will instead be wiped irreversibly."
  (interactive "P")
  (treemacs-block
   (treemacs-unless-let (btn (treemacs-current-button))
       (treemacs-pulse-on-failure "Nothing to delete here.")
     (treemacs-error-return-if (not (memq (treemacs-button-get btn :state)
                                          '(file-node-open file-node-closed dir-node-open dir-node-closed)))
       "Only files and directories can be deleted.")
     (treemacs--without-filewatch
      (let* ((delete-by-moving-to-trash (not arg))
             (path (treemacs-button-get btn :path))
             (file-name (propertize (treemacs--filename path) 'face 'font-lock-string-face)))
        (cond
         ((file-symlink-p path)
          (if (yes-or-no-p (format "Remove link '%s -> %s' ? "
                                     file-name
                                     (propertize (file-symlink-p path) 'face 'font-lock-face)))
              (delete-file path delete-by-moving-to-trash)
            (treemacs-return (treemacs-log "Cancelled."))))
         ((file-regular-p path)
          (if (yes-or-no-p (format "Delete '%s' ? " file-name))
              (delete-file path delete-by-moving-to-trash)
            (treemacs-return (treemacs-log "Cancelled."))))
         ((file-directory-p path)
          (if (yes-or-no-p (format "Recursively delete '%s' ? " file-name))
              (delete-directory path t delete-by-moving-to-trash)
            (treemacs-return (treemacs-log "Cancelled."))))
         (t
          (treemacs-error-return
              (treemacs-pulse-on-failure
                  "Item is neither a file, a link or a directory - treemacs does not know how to delete it. (Maybe it no longer exists?)"))))
        (treemacs--on-file-deletion path)
        (treemacs-without-messages
         (treemacs-run-in-every-buffer
          (treemacs-delete-single-node path)))
        (run-hook-with-args 'treemacs-delete-file-functions path)
        (treemacs-log "Deleted %s."
          (propertize path 'face 'font-lock-string-face))))
     (treemacs--evade-image))))

(defun treemacs-create-file ()
  "Create a new file.
Enter first the directory to create the new file in, then the new file's name.
The pre-selection for what directory to create in is based on the \"nearest\"
path to point - the containing directory for tags and files or the directory
itself, using $HOME when there is no path at or near point to grab."
  (interactive)
  (treemacs--create-file/dir t))

(defun treemacs-move-file ()
  "Move file (or directory) at point.
Destination may also be a filename, in which case the moved file will also
be renamed."
  (interactive)
  (treemacs--copy-or-move :move))

(defun treemacs-copy-file ()
  "Copy file (or directory) at point.
Destination may also be a filename, in which case the copied file will also
be renamed."
  (interactive)
  (treemacs--copy-or-move :copy))

(cl-defun treemacs-rename ()
  "Rename the currently selected node.
Buffers visiting the renamed file or visiting a file inside a renamed directory
and windows showing them will be reloaded.  The list of recent files will
likewise be updated."
  (interactive)
  (treemacs-block
   (-let [btn (treemacs-current-button)]
     (treemacs-error-return-if (null btn)
       "Nothing to rename here.")
     (let* ((old-path (treemacs-button-get btn :path))
            (project (treemacs--find-project-for-path old-path))
            (new-path nil)
            (new-name nil)
            (dir nil))
       (treemacs-error-return-if (null old-path)
         "Found nothing to rename here.")
       (treemacs-error-return-if (not (file-exists-p old-path))
         "The file to be renamed does not exist.")
       (setq new-name (treemacs--read-string "New name: " (file-name-nondirectory old-path))
             dir      (treemacs--parent-dir old-path)
             new-path (treemacs-join-path dir new-name))
       (treemacs-error-return-if (file-exists-p new-path)
         "A file named %s already exists."
         (propertize new-name 'face font-lock-string-face))
       (treemacs--without-filewatch (rename-file old-path new-path))
       (treemacs--replace-recentf-entry old-path new-path)
       (-let [treemacs-silent-refresh t]
         (treemacs-run-in-every-buffer
          (treemacs--on-rename old-path new-path treemacs-filewatch-mode)
          (treemacs--do-refresh (current-buffer) project)))
       (treemacs--reload-buffers-after-rename old-path new-path)
       (treemacs-goto-file-node new-path project)
       (run-hook-with-args
        'treemacs-rename-file-functions
        old-path new-path)
       (treemacs-pulse-on-success "Renamed %s to %s."
         (propertize (treemacs--filename old-path) 'face font-lock-string-face)
         (propertize new-name 'face font-lock-string-face))))))

(defun treemacs-create-dir ()
  "Create a new directory.
Enter first the directory to create the new dir in, then the new dir's name.
The pre-selection for what directory to create in is based on the \"nearest\"
path to point - the containing directory for tags and files or the directory
itself, using $HOME when there is no path at or near point to grab."
  (interactive)
  (treemacs--create-file/dir nil))

(defun treemacs-toggle-show-dotfiles ()
  "Toggle the hiding and displaying of dotfiles."
  (interactive)
  (setq treemacs-show-hidden-files (not treemacs-show-hidden-files))
  (treemacs-run-in-every-buffer
   (treemacs--do-refresh (current-buffer) 'all))
  (treemacs-log "Dotfiles will now be %s"
                (if treemacs-show-hidden-files "displayed." "hidden.")))

(defun treemacs-toggle-fixed-width ()
  "Toggle whether the local treemacs buffer should have a fixed width.
See also `treemacs-width.'"
  (interactive)
  (-if-let (buffer (treemacs-get-local-buffer))
      (with-current-buffer buffer
        (setq treemacs--width-is-locked (not treemacs--width-is-locked)
              window-size-fixed (when treemacs--width-is-locked 'width))
        (treemacs-log "Window width has been %s."
          (propertize (if treemacs--width-is-locked "locked" "unlocked")
                      'face 'font-lock-string-face)))
    (treemacs-log-failure "There is no treemacs buffer in the current scope.")))

(defun treemacs-set-width (&optional arg)
  "Select a new value for `treemacs-width'.
With a prefix ARG simply reset the width of the treemacs window."
  (interactive "P")
  (unless arg
    (setq treemacs-width
          (->> treemacs-width
               (format "New Width (current = %s): ")
               (read-number))))
  (treemacs--set-width treemacs-width))

(defun treemacs-copy-absolute-path-at-point ()
  "Copy the absolute path of the node at point."
  (interactive)
  (treemacs-block
   (-let [path (treemacs--prop-at-point :path)]
     (treemacs-error-return-if (null path)
       "There is nothing to copy here")
     (treemacs-error-return-if (not (stringp path))
       "Path at point is not a file.")
     (when (file-directory-p path)
       (setf path (treemacs--add-trailing-slash path)))
     (kill-new path)
     (treemacs-pulse-on-success "Copied absolute path: %s" (propertize path 'face 'font-lock-string-face)))))

(defun treemacs-copy-relative-path-at-point ()
  "Copy the path of the node at point relative to the project root."
  (interactive)
  (treemacs-block
   (let ((path (treemacs--prop-at-point :path))
         (project (treemacs-project-at-point)))
     (treemacs-error-return-if (null path)
       "There is nothing to copy here")
     (treemacs-error-return-if (not (stringp path))
       "Path at point is not a file.")
     (when (file-directory-p path)
       (setf path (treemacs--add-trailing-slash path)))
     (-let [copied (-> path (file-relative-name (treemacs-project->path project)) (kill-new))]
       (treemacs-pulse-on-success "Copied relative path: %s" (propertize copied 'face 'font-lock-string-face))))))

(defun treemacs-copy-project-path-at-point ()
  "Copy the absolute path of the current treemacs root."
  (interactive)
  (treemacs-block
   (-let [project (treemacs-project-at-point)]
     (treemacs-error-return-if (null project)
       "There is nothing to copy here")
     (treemacs-error-return-if (not (stringp (treemacs-project->path project)))
       "Project at point is not a file.")
    (-let [copied (-> project (treemacs-project->path) (kill-new))]
      (treemacs-pulse-on-success "Copied project path: %s" (propertize copied 'face 'font-lock-string-face))))))

(defun treemacs-delete-other-windows ()
  "Same as `delete-other-windows', but will not delete the treemacs window.
If this command is run when the treemacs window is selected `next-window' will
also not be deleted."
  (interactive)
  (save-selected-window
    (-let [w (treemacs-get-local-window)]
      (when (eq w (selected-window))
        (select-window (next-window)))
      (delete-other-windows)
      ;; we still want to call `delete-other-windows' since it contains plenty of nontrivial code
      ;; that we shouldn't prevent from running, so we just restore treemacs instead of preventing
      ;; it from being deleted
      ;; 'no-delete-other-windows could be used instead, but it's only available for emacs 26
      (when w
        (treemacs--select-not-visible-window)))))

(defun treemacs-temp-resort-root (&optional sort-method)
  "Temporarily resort the entire treemacs buffer.
SORT-METHOD is a cons of a string describing the method and the actual sort
value, as returned by `treemacs--sort-value-selection'.  SORT-METHOD will be
provided when this function is called from `treemacs-resort' and will be
interactively read otherwise.  This way this function can be bound directly,
without the need to call `treemacs-resort' with a prefix arg."
  (interactive)
  (-let* (((sort-name . sort-method) (or sort-method (treemacs--sort-value-selection)))
          (treemacs-sorting sort-method))
    (treemacs-without-messages (treemacs-refresh))
    (treemacs-log "Temporarily resorted everything with sort method '%s.'"
                  (propertize sort-name 'face 'font-lock-type-face))))

(defun treemacs-temp-resort-current-dir (&optional sort-method)
  "Temporarily resort the current directory.
SORT-METHOD is a cons of a string describing the method and the actual sort
value, as returned by `treemacs--sort-value-selection'.  SORT-METHOD will be
provided when this function is called from `treemacs-resort' and will be
interactively read otherwise.  This way this function can be bound directly,
without the need to call `treemacs-resort' with a prefix arg."
  (interactive)
  (-let* (((sort-name . sort-method) (or sort-method (treemacs--sort-value-selection)))
          (treemacs-sorting sort-method))
    (-if-let (btn (treemacs-current-button))
        (pcase (treemacs-button-get btn :state)
          ('dir-node-closed
           (treemacs--expand-dir-node btn)
           (treemacs-log "Resorted %s with sort method '%s'."
                         (propertize (treemacs--get-label-of btn) 'face 'font-lock-string-face)
                         (propertize sort-name 'face 'font-lock-type-face)))
          ('dir-node-open
           (treemacs--collapse-dir-node btn)
           (goto-char (treemacs-button-start btn))
           (treemacs--expand-dir-node btn)
           (treemacs-log "Resorted %s with sort method '%s'."
                         (propertize (treemacs--get-label-of btn) 'face 'font-lock-string-face)
                         (propertize sort-name 'face 'font-lock-type-face)))
          ((or 'file-node-open 'file-node-closed 'tag-node-open 'tag-node-closed 'tag-node)
           (let* ((parent (treemacs-button-get btn :parent)))
             (while (and parent
                         (not (-some-> parent (treemacs-button-get :path) (file-directory-p))))
               (setq parent (treemacs-button-get parent :parent)))
             (if parent
                 (let ((line (line-number-at-pos))
                       (window-point (window-point)))
                   (goto-char (treemacs-button-start parent))
                   (treemacs--collapse-dir-node parent)
                   (goto-char (treemacs-button-start btn))
                   (treemacs--expand-dir-node parent)
                   (set-window-point (selected-window) window-point)
                   (with-no-warnings (goto-line line))
                   (treemacs-log "Resorted %s with sort method '%s'."
                                 (propertize (treemacs--get-label-of parent) 'face 'font-lock-string-face)
                                 (propertize sort-name 'face 'font-lock-type-face)))
               ;; a top level file's containing dir is root
               (treemacs-without-messages (treemacs-refresh))
               (treemacs-log "Resorted root directory with sort method '%s'."
                             (propertize sort-name 'face 'font-lock-type-face)))))))))

(defun treemacs-resort (&optional arg)
  "Select a new permanent value for `treemacs-sorting' and refresh.
With a single prefix ARG use the new sort value to *temporarily* resort the
\(closest\) directory at point.
With a double prefix ARG use the new sort value to *temporarily* resort the
entire treemacs view.

Temporary sorting will only stick around until the next refresh, either manual
or automatic via `treemacs-filewatch-mode'.

Instead of calling this with a prefix arg you can also directly call
`treemacs-temp-resort-current-dir' and `treemacs-temp-resort-root'."
  (interactive "P")
  (pcase arg
    ;; Resort current dir only
    (`(4)
     (treemacs-temp-resort-current-dir))
    ;; Temporarily resort everything
    (`(16)
     (treemacs-temp-resort-root))
    ;; Set new permanent value
    (_
     (-let (((sort-name . sort-value) (treemacs--sort-value-selection)))
       (setq treemacs-sorting sort-value)
       (treemacs-without-messages (treemacs-refresh))
       (treemacs-log "Sorting method changed to '%s'."
                     (propertize sort-name 'face 'font-lock-type-face)))))
  (treemacs--evade-image))

(defun treemacs-next-line-other-window (&optional count)
  "Scroll forward COUNT lines in `next-window'."
  (interactive "p")
  (treemacs-without-following
   (with-selected-window (next-window)
     (scroll-up-line count))))

(defun treemacs-previous-line-other-window (&optional count)
  "Scroll backward COUNT lines in `next-window'."
  (interactive "p")
  (treemacs-without-following
   (with-selected-window (next-window)
     (scroll-down-line count))))

(defun treemacs-next-page-other-window (&optional count)
  "Scroll forward COUNT pages in `next-window'.
For slower scrolling see `treemacs-next-line-other-window'"
  (interactive "p")
  (treemacs-without-following
   (with-selected-window (next-window)
     (condition-case _
         (dotimes (_ (or count 1))
           (scroll-up nil))
       (end-of-buffer (goto-char (point-max)))))))

(defun treemacs-previous-page-other-window (&optional count)
  "Scroll backward COUNT pages in `next-window'.
For slower scrolling see `treemacs-previous-line-other-window'"
  (interactive "p")
  (treemacs-without-following
   (with-selected-window (next-window)
     (condition-case _
         (dotimes (_ (or count 1))
           (scroll-down nil))
       (beginning-of-buffer (goto-char (point-min)))))))

(defun treemacs-next-project ()
  "Move to the next project root node."
  (interactive)
  (-let [pos (treemacs--next-project-pos)]
    (if (or (= pos (point))
            (= pos (point-max)))
        (treemacs-pulse-on-failure "There is no next project to move to.")
      (goto-char pos)
      (treemacs--maybe-recenter treemacs-recenter-after-project-jump))))

(defun treemacs-previous-project ()
  "Move to the next project root node."
  (interactive)
  (-let [pos (treemacs--prev-project-pos)]
    (if (or (= pos (point))
            (= pos (point-min)))
        (treemacs-pulse-on-failure "There is no previous project to move to.")
      (goto-char pos)
      (treemacs--maybe-recenter treemacs-recenter-after-project-jump))))

(defun treemacs-rename-project ()
  "Give the project at point a new name."
  (interactive)
  (treemacs-with-writable-buffer
   (treemacs-block
    (treemacs-unless-let (project (treemacs-project-at-point))
        (treemacs-pulse-on-failure "There is no project here.")
      (let* ((old-name (treemacs-project->name project))
             (project-btn (treemacs-project->position project))
             (state (treemacs-button-get project-btn :state))
             (new-name (treemacs--read-string "New name: " (treemacs-project->name project))))
        (treemacs-save-position
         (progn
           (treemacs-return-if (treemacs--is-name-invalid? new-name)
             (treemacs-pulse-on-failure "'%s' is an invalid name."
               (propertize new-name 'face 'font-lock-type-face)))
           (treemacs-return-if (string-equal old-name new-name)
             (treemacs-pulse-on-failure "The new name is the same as the old name."))
           (setf (treemacs-project->name project) new-name)
           (treemacs--forget-last-highlight)
           ;; after renaming, delete and redisplay the project
           (goto-char (treemacs-button-end project-btn))
           (delete-region (point-at-bol) (point-at-eol))
           (treemacs--add-root-element project)
           (when (eq state 'root-node-open)
             (treemacs--collapse-root-node (treemacs-project->position project))
             (treemacs--expand-root-node (treemacs-project->position project))))
         (run-hook-with-args 'treemacs-rename-project-functions project old-name)
         (treemacs-pulse-on-success "Renamed project %s to %s."
           (propertize old-name 'face 'font-lock-type-face)
           (propertize new-name 'face 'font-lock-type-face)))))))
  (treemacs--evade-image))

(defun treemacs-add-project-to-workspace (path &optional name)
  "Add a project at given PATH to the current workspace.
The PATH's directory name will be used as a NAME for a project.  The NAME can
\(or must) be entered manually with either a prefix arg or if a project with the
auto-selected name already exists."
  (interactive "DProject root: ")
  (let* ((default-name (treemacs--filename path))
         (double-name (--first (string= default-name (treemacs-project->name it))
                               (treemacs-workspace->projects (treemacs-current-workspace)))))
    (if (or current-prefix-arg double-name)
        (setf name (treemacs--read-string "Project Name: " (unless double-name (treemacs--filename path))))
      (setf name default-name)))
  (pcase (treemacs-do-add-project-to-workspace path name)
    (`(success ,project)
     (treemacs-pulse-on-success "Added project '%s' to the workspace."
       (propertize (treemacs-project->name project) 'face 'font-lock-type-face)))
    (`(invalid-path ,reason)
     (treemacs-pulse-on-failure (concat "Path '%s' is invalid: %s")
       (propertize path 'face 'font-lock-string-face)
       reason))
    (`(invalid-name ,name)
     (treemacs-pulse-on-failure "Name '%s' is invalid."
       (propertize name 'face 'font-lock-string-face)))
    (`(duplicate-project ,duplicate)
     (goto-char (treemacs-project->position duplicate))
     (treemacs-pulse-on-failure "A project for '%s' already exists. Projects may not overlap."
       (propertize (treemacs-project->path duplicate) 'face 'font-lock-string-face)))
    (`(includes-project ,project)
     (goto-char (treemacs-project->position project))
     (treemacs-pulse-on-failure "Project '%s' is included in '%s'. Projects May not overlap."
       (propertize (treemacs-project->name project) 'face 'font-lock-type-face)
       (propertize path 'face 'font-lock-string-face)))
    (`(duplicate-name ,duplicate)
     (goto-char (treemacs-project->position duplicate))
     (treemacs-pulse-on-failure "A project with the name %s already exists."
       (propertize (treemacs-project->name duplicate) 'face 'font-lock-type-face))))
  nil)
(defalias 'treemacs-add-project #'treemacs-add-project-to-workspace)
(with-no-warnings
  (make-obsolete #'treemacs-add-project #'treemacs-add-project-to-workspace "v2.2.1"))

(defun treemacs-remove-project-from-workspace (&optional arg)
  "Remove the project at point from the current workspace.
With a prefix ARG select project to remove by name."
  (interactive "P")
  (let ((project (treemacs-project-at-point))
        (save-pos))
    (when (or arg (null project))
      (setf project (treemacs--select-project-by-name)
            save-pos (not (equal project (treemacs-project-at-point)))))
    (pcase (if save-pos
               (treemacs-save-position
                (treemacs-do-remove-project-from-workspace project))
             (treemacs-do-remove-project-from-workspace project))
      (`success
       (whitespace-cleanup)
       (treemacs-pulse-on-success "Removed project %s from the workspace."
         (propertize (treemacs-project->name project) 'face 'font-lock-type-face)))
      (`cannot-delete-last-project
       (treemacs-pulse-on-failure "Cannot delete the last project."))
      (`(invalid-project ,reason)
       (treemacs-pulse-on-failure "Cannot delete project: %s"
         (propertize reason 'face 'font-lock-string-face))))))

(defun treemacs-create-workspace ()
  "Create a new workspace."
  (interactive)
  (pcase (treemacs-do-create-workspace)
    (`(success ,workspace)
     (treemacs-pulse-on-success "Workspace %s successfully created."
       (propertize (treemacs-workspace->name workspace) 'face 'font-lock-type-face)))
    (`(invalid-name ,name)
     (treemacs-pulse-on-failure "Name '%s' is invalid."
       (propertize name 'face 'font-lock-string-face)))
    (`(duplicate-name ,duplicate)
     (treemacs-pulse-on-failure "A workspace with the name %s already exists."
       (propertize (treemacs-workspace->name duplicate) 'face 'font-lock-string-face)))))

(defun treemacs-remove-workspace ()
  "Delete a workspace."
  (interactive)
  (pcase (treemacs-do-remove-workspace nil :ask-to-confirm)
    ('only-one-workspace
     (treemacs-pulse-on-failure "You cannot delete the last workspace."))
    (`(workspace-not-found ,name)
     (treemacs-pulse-on-failure "Workspace with name '%s' does not exist"
       (propertize name 'face 'font-lock-type-face)))
    ('user-cancel
     (ignore))
    (`(success ,deleted ,_)
     (treemacs-pulse-on-success "Workspace %s was deleted."
       (propertize (treemacs-workspace->name deleted) 'face 'font-lock-type-face)))))

(defun treemacs-switch-workspace (arg)
  "Select a different workspace for treemacs.

With a prefix ARG clean up buffers after the switch.  A single prefix argument
will delete all file visiting buffers, 2 prefix arguments will clean up all open
buffers (except for treemacs itself and the scratch and messages buffers).

Without a prefix argument `treemacs-workspace-switch-cleanup' will
be followed instead."
  (interactive "P")
  (pcase (treemacs-do-switch-workspace)
    ('only-one-workspace
     (treemacs-pulse-on-failure "There are no other workspaces to select."))
    (`(success ,workspace)
     (treemacs--maybe-clean-buffers-on-workspace-switch
      (pcase arg
        (`(4) 'files)
        (`(16) 'all)
        (_ treemacs-workspace-switch-cleanup)))
     (treemacs-pulse-on-success "Selected workspace %s."
       (propertize (treemacs-workspace->name workspace))))))

(defun treemacs-set-fallback-workspace (&optional arg)
  "Set the current workspace as the default fallback.
With a non-nil prefix ARG choose the fallback instead.

The fallback workspace is the one treemacs will select when it is opened for the
first time and the current file at the time is not part of any of treemacs'
workspaces."
  (interactive "P")
  (treemacs-block
   (-let [fallback (if arg (treemacs--select-workspace-by-name) (treemacs-current-workspace))]
     (treemacs-error-return-if (null fallback)
       "There is no workspace with that name.")
     (setf treemacs--workspaces
           (sort treemacs--workspaces
                 (lambda (ws _) (equal ws fallback))))
     (treemacs--persist)
     (treemacs-pulse-on-success "Selected workspace %s as fallback."
       (propertize (treemacs-workspace->name fallback) 'face 'font-lock-type-face)))))

(defun treemacs-rename-workspace ()
  "Select a workspace to rename."
  (interactive)
  (pcase (treemacs-do-rename-workspace)
    (`(success ,old-name ,workspace)
     (treemacs-pulse-on-success "Workspace %s successfully renamed to %s."
       (propertize old-name 'face 'font-lock-type-face)
       (propertize (treemacs-workspace->name workspace) 'face 'font-lock-type-face)))
    (`(invalid-name ,name)
     (treemacs-pulse-on-failure "Name '%s' is invalid."
       (propertize name 'face 'font-lock-string-face)))))

(defun treemacs-refresh ()
  "Refresh the project at point."
  (interactive)
  (treemacs-unless-let (btn (treemacs-current-button))
      (treemacs-log-failure "There is nothing to refresh.")
    (treemacs-without-recenter
     (treemacs--do-refresh (current-buffer) (treemacs-project-of-node btn)))))

(defun treemacs-collapse-project (&optional arg)
  "Close the project at point.
With a prefix ARG also forget about all the nodes opened in the project."
  (interactive "P")
  (treemacs-unless-let (project (treemacs-project-at-point))
      (treemacs-pulse-on-failure "There is nothing to close here.")
    (-let [btn (treemacs-project->position project)]
      (when (treemacs-is-node-expanded? btn)
        (treemacs--forget-last-highlight)
        (goto-char btn)
        (treemacs--collapse-root-node btn arg)
        (treemacs--maybe-recenter 'on-distance)))
    (treemacs-pulse-on-success "Collapsed current project")))

(defun treemacs-collapse-all-projects (&optional arg)
  "Collapses all projects.
With a prefix ARG also forget about all the nodes opened in the projects."
  (interactive "P")
  (save-excursion
    (treemacs--forget-last-highlight)
    (dolist (project (treemacs-workspace->projects (treemacs-current-workspace)))
      (-when-let (pos (treemacs-project->position project))
        (when (eq 'root-node-open (treemacs-button-get pos :state))
          (goto-char pos)
          (treemacs--collapse-root-node pos arg)))))
  (treemacs--maybe-recenter 'on-distance)
  (treemacs-pulse-on-success "Collapsed all projects"))

(defun treemacs-collapse-other-projects (&optional arg)
  "Collapses all projects except the project at point.
With a prefix ARG also forget about all the nodes opened in the projects."
  (interactive "P")
  (save-excursion
    (-let [curr-project (treemacs-project-at-point)]
      (dolist (project (treemacs-workspace->projects (treemacs-current-workspace)))
        (unless (eq project curr-project)
          (-when-let (pos (treemacs-project->position project))
            (when (eq 'root-node-open (treemacs-button-get pos :state))
              (goto-char pos)
              (treemacs--collapse-root-node pos arg)))))))
  (treemacs--maybe-recenter 'on-distance)
  (treemacs-pulse-on-success "Collapsed all other projects"))

(defun treemacs-peek ()
  "Peek at the content of the node at point.
This will display the file (or tag) at point in `next-window' much like
`treemacs-visit-node-no-split' would.  The difference that the file is not
really (or rather permanently) opened - any command other than `treemacs-peek',
`treemacs-next-line-other-window', `treemacs-previous-line-other-window',
`treemacs-next-page-other-window' or `treemacs-previous-page-other-window' will
cause it to be closed again and the previously shown buffer to be restored.  The
buffer visiting the peeked file will also be killed again, unless it was already
open before being used for peeking."
  (interactive)
  (treemacs--execute-button-action
   :save-window t
   :ensure-window-split t
   :window (-some-> btn (treemacs--nearest-path) (get-file-buffer) (get-buffer-window))
   :no-match-explanation "Only files and tags are peekable."
   :file-action (treemacs--setup-peek-buffer btn)
   :tag-action (treemacs--setup-peek-buffer btn t)))

(defun treemacs-root-up (&optional _)
  "Move treemacs' root one level upward.
Only works with a single project in the workspace."
  (interactive "P")
  (treemacs-block
   (unless (= 1 (length (treemacs-workspace->projects (treemacs-current-workspace))))
     (treemacs-error-return
         "Ad-hoc navigation is only possible when there is but a single project in the workspace."))
   (-let [btn (treemacs-current-button)]
     (unless btn
       (setq btn (previous-button (point))))
     (let* ((project (-> btn (treemacs--nearest-path) (treemacs--find-project-for-path)))
            (old-root (treemacs-project->path project))
            (new-root (treemacs--parent old-root))
            (new-name (pcase new-root
                        ("/" new-root)
                        (_  (file-name-nondirectory new-root))))
            (treemacs--no-messages t)
            (treemacs-pulse-on-success nil))
       (unless (treemacs-is-path old-root :same-as new-root)
         (treemacs-do-remove-project-from-workspace project :ignore-last-project-restriction)
         (treemacs--reset-dom) ;; remove also the previous root's dom entry
         (treemacs-do-add-project-to-workspace new-root new-name)
         (treemacs-goto-file-node old-root))))))

(defun treemacs-root-down (&optional _)
  "Move treemacs' root into the directory at point.
Only works with a single project in the workspace."
  (interactive "P")
  (treemacs-block
   (treemacs-error-return-if (/= 1 (length (treemacs-workspace->projects (treemacs-current-workspace))))
     "Free navigation is only possible when there is but a single project in the workspace.")
   (treemacs-unless-let (btn (treemacs-current-button))
       (treemacs-pulse-on-failure
           "There is no directory to move into here.")
     (pcase (treemacs-button-get btn :state)
       ((or 'dir-node-open 'dir-node-closed)
        (let ((new-root (treemacs-button-get btn :path))
              (treemacs--no-messages t)
              (treemacs-pulse-on-success nil))
          (treemacs-do-remove-project-from-workspace (treemacs-project-at-point) :ignore-last-project-restriction)
          (treemacs--reset-dom) ;; remove also the previous root's dom entry
          (treemacs-do-add-project-to-workspace new-root (file-name-nondirectory new-root))
          (treemacs-goto-file-node new-root)))
       (_
        (treemacs-pulse-on-failure "Button at point is not a directory."))))))

(defun treemacs-show-extensions ()
  "Display a list of all active extensions."
  (interactive)
  (-let [txt (list "#+TITLE: Treemacs Active Extensions\n")]
    (cl-flet ((with-face (txt face) (propertize txt 'font-lock-face face)))
      (pcase-dolist
          (`(,headline . ,name)
           '(("* Directory Extensions" . directory)
             ("* Project Extensions" . project)
             ("* Root Extetensions"  . root)) )
        (let ((top-name (symbol-value (intern (format "treemacs--%s-top-extensions" name))))
              (bottom-name (symbol-value (intern (format "treemacs--%s-bottom-extensions" name)))))
          (push headline txt)
          (pcase-dolist
              (`(,pos-txt . ,pos-val)
               `(("** Top" . ,top-name)
                 ("** Bottom" . ,bottom-name)))
            (push pos-txt txt)
            (if pos-val
                (dolist (ext pos-val)
                  (push (format " - %s\n   with predicate %s\n   defined in %s"
                                (with-face (symbol-name (car ext)) 'font-lock-keyword-face)
                                (with-face (--if-let (cdr ext) (symbol-name it) "None") 'font-lock-function-name-face)
                                (with-face (get (car ext) :defined-in) 'font-lock-string-face))
                        txt))
              (push (with-face " - None" 'font-lock-comment-face) txt))))))
    (-let [buf (get-buffer-create "*Treemacs Extension Overview*")]
      (switch-to-buffer buf)
      (org-mode)
      (erase-buffer)
      (->> txt (nreverse) (--map (concat it "\n")) (apply #'concat) (insert))
      (with-no-warnings (org-reveal))
      (goto-char 0)
      (forward-line))))

(defun treemacs-move-project-up ()
  "Switch position of the project at point and the one above it."
  (interactive)
  (treemacs-block
   (let* ((workspace (treemacs-current-workspace))
          (projects  (treemacs-workspace->projects workspace))
          (project1  (treemacs-project-at-point))
          (index1    (or (treemacs-error-return-if (null project1)
                           "There is nothing to move here.")
                         (-elem-index project1 projects)))
          (index2    (1- index1))
          (project2  (or (treemacs-error-return-if (> 0 index2)
                           "There is no project to switch places with above.")
                         (nth index2 projects)))
          (bounds1  (treemacs--get-bounds-of-project project1))
          (bounds2  (treemacs--get-bounds-of-project project2)))
     (treemacs-with-writable-buffer
      (transpose-regions
       (car bounds1) (cdr bounds1)
       (car bounds2) (cdr bounds2)))
     (setf (nth index1 projects) project2
           (nth index2 projects) project1)
     (treemacs--persist)
     (recenter))))

(defun treemacs-move-project-down ()
  "Switch position of the project at point and the one below it."
  (interactive)
  (treemacs-block
   (let* ((workspace (treemacs-current-workspace))
          (projects  (treemacs-workspace->projects workspace))
          (project1  (treemacs-project-at-point))
          (index1    (or (treemacs-error-return-if (null project1)
                           "There is nothing to move here.")
                         (-elem-index project1 projects)))
          (index2    (1+ index1))
          (project2  (or (treemacs-error-return-if (>= index2 (length projects))
                           "There is no project to switch places with below.")
                         (nth index2 projects)))
          (bounds1  (treemacs--get-bounds-of-project project1))
          (bounds2  (treemacs--get-bounds-of-project project2)))
     (treemacs-with-writable-buffer
      (transpose-regions
       (car bounds1) (cdr bounds1)
       (car bounds2) (cdr bounds2)))
     (setf (nth index1 projects) project2
           (nth index2 projects) project1)
     (treemacs--persist)
     (recenter))))

(defun treemacs-finish-edit ()
  "Finish editing your workspaces and apply the change."
  (interactive)
  (treemacs-block
   (treemacs-error-return-if (not (equal (buffer-name) treemacs--org-edit-buffer-name))
     "This is not a valid treemacs workspace edit buffer")
   (treemacs--org-edit-remove-validation-msg)
   (widen)
   (whitespace-cleanup)
   (-let [lines (treemacs--read-persist-lines (buffer-string))]
     (treemacs-error-return-if (null (buffer-string))
       "The buffer is empty, there is nothing here to save.")
     (pcase (treemacs--validate-persist-lines lines)
       (`(error ,err-line ,err-msg)
        (treemacs--org-edit-display-validation-msg err-msg err-line))
       ('success
        (treemacs--invalidate-buffer-project-cache)
        (write-region
         (apply #'concat (--map (concat it "\n") lines))
         nil
         treemacs-persist-file
         nil :silent)
        (treemacs--restore)
        (-if-let (ws (treemacs--select-workspace-by-name
                      (treemacs-workspace->name (treemacs-current-workspace))))
            (setf (treemacs-current-workspace) ws)
          (treemacs--find-workspace))
        (treemacs--consolidate-projects)
        (if (and (treemacs-get-local-window)
                 (= 2 (length (window-list))))
            (kill-buffer)
          (quit-window)
          (kill-buffer-and-window))
        (run-hooks 'treemacs-workspace-edit-hook)
        (treemacs-log "Edit completed successfully."))))))

(defun treemacs-collapse-parent-node (arg)
  "Close the parent of the node at point.
Prefix ARG will be passed on to the closing function
\(see `treemacs-toggle-node'.\)"
  (interactive "P")
  (-if-let* ((btn (treemacs-current-button))
             (parent (button-get btn :parent)))
      (progn
        (treemacs--forget-last-highlight)
        (goto-char parent)
        (treemacs-toggle-node arg)
        (treemacs--evade-image))
    (treemacs-pulse-on-failure
        (if btn "Already at root." "There is nothing to close here."))))

(defun treemacs-run-shell-command-in-project-root (&optional arg)
  "Run an asynchronous shell command in the root of the current project.
Output will only be saved and displayed if prefix ARG is non-nil.

Every instance of the string `$path' will be replaced with the (properly quoted)
absolute path of the project root."
  (interactive "P")
  (let* ((cmd (read-shell-command "Command: "))
         (name "*Treemacs Shell Command*")
         (node (treemacs-node-at-point))
         (buffer (progn (--when-let (get-buffer name)
                          (kill-buffer it))
                        (get-buffer-create name)))
         (working-dir nil))
    (treemacs-block
     (treemacs-error-return-if (null node)
       (treemacs-pulse-on-failure "There is no project here."))
     (-let [project (treemacs-project-of-node node)]
       (treemacs-error-return-if (treemacs-project->is-unreadable? project)
         (treemacs-pulse-on-failure "Project path is not readable."))
       (setf working-dir (treemacs-project->path project)
             cmd (s-replace "$path" (shell-quote-argument working-dir) cmd))
       (pfuture-callback `(,shell-file-name ,shell-command-switch ,cmd)
         :name name
         :buffer buffer
         :directory working-dir
         :on-success
         (if arg
             (progn
               (pop-to-buffer pfuture-buffer)
               (require 'ansi-color)
               (ansi-color-apply-on-region (point-min) (point-max)))
           (treemacs-log "Shell command completed successfully.")
           (kill-buffer buffer))
         :on-error
         (progn
           (treemacs-log-failure "Shell command failed with exit code %s and output:" (process-exit-status process))
           (message "%s" (pfuture-callback-output))
           (kill-buffer buffer)))))))

(defun treemacs-run-shell-command-for-current-node (&optional arg)
  "Run a shell command on the current node.
Output will only be saved and displayed if prefix ARG is non-nil.

Will use the location of the current node as working directory.  If the current
node is not a file/dir, then the next-closest file node will be used.  If all
nodes are non-files, or if there is no node at point, $HOME will be set as the
working directory.

Every instance of the string `$path' will be replaced with the (properly quoted)
absolute path of the node (if it is present)."
  (interactive "P")
  (let* ((cmd (read-shell-command "Command: "))
         (name "*Treemacs Shell Command*")
         (node (treemacs-node-at-point))
         (buffer (progn (--when-let (get-buffer name)
                          (kill-buffer it))
                        (get-buffer-create name)))
         (working-dir (-some-> node (treemacs-button-get :path))))
    (cond
     ((null node)
      (setf working-dir "~/"))
     ((or (null working-dir) (not (file-exists-p working-dir)))
      (setf working-dir (treemacs--nearest-path node))
      (when (or (null working-dir)
                (not (file-exists-p working-dir)))
        (setf working-dir "~/")))
     (t
      (setf working-dir (treemacs--parent working-dir))))
    (when (and node (treemacs-is-node-file-or-dir? node))
      (setf cmd (s-replace "$path" (shell-quote-argument (treemacs-button-get node :path)) cmd)))
    (pfuture-callback `(,shell-file-name ,shell-command-switch ,cmd)
      :name name
      :buffer buffer
      :directory working-dir
      :on-success
      (if arg
          (progn
            (pop-to-buffer pfuture-buffer)
             (require 'ansi-color)
             (autoload 'ansi-color-apply-on-region "ansi-color")
             (ansi-color-apply-on-region (point-min) (point-max)))
        (treemacs-log "Shell command completed successfully.")
        (kill-buffer buffer))
      :on-error
      (progn
        (treemacs-log-failure "Shell command failed with exit code %s and output:" (process-exit-status process))
        (message "%s" (pfuture-callback-output))
        (kill-buffer buffer)))))

(defun treemacs-narrow-to-current-file ()
  "Close everything except the view on the current file.
This command is best understood as a combination of
`treemacs-collapse-all-projects' followed by `treemacs-find-file'."
  (interactive)
  (treemacs-unless-let (buffer (treemacs-get-local-buffer))
      (treemacs-log-failure "There is no treemacs buffer")
    (let* ((treemacs-pulse-on-success nil)
           (treemacs-pulse-on-failure nil)
           (treemacs--no-messages t))
      (with-current-buffer buffer
        (treemacs-collapse-all-projects :forget-all))
      (treemacs-find-file))))

(defun treemacs-select-scope-type ()
  "Select the scope for treemacs buffers.
The default (and only) option is scoping by frame, which means that every Emacs
frame (and only an Emacs frame) will have its own unique treemacs buffer.
Additional scope types can be enabled by installing the appropriate package.

The following packages offer additional scope types:
 * treemacs-persp
 * treemacs-perspective

To programmatically set the scope type see `treemacs-set-scope-type'."
  (interactive)
  (let* ((selection (completing-read "Select Treemacs Scope: " treemacs-scope-types))
         (new-scope-type (-> selection (intern) (assoc treemacs-scope-types) (cdr))))
    (cond
     ((null new-scope-type)
      (treemacs-log "Nothing selected, type %s remains in effect."
        (propertize selection 'face 'font-lock-type-face)))
     ((eq new-scope-type treemacs--current-scope-type)
      (treemacs-log "New scope type is same as old, nothing has changed."))
     (t
      (treemacs--do-set-scope-type new-scope-type)
      (treemacs-log "Scope of type %s is now in effect."
        (propertize selection 'face 'font-lock-type-face))))))

(defun treemacs-cleanup-litter ()
  "Collapse all nodes matching any of `treemacs-litter-directories'."
  (interactive)
  (-let [litter-list (-map #'regexp-quote treemacs-litter-directories)]
    (treemacs-run-in-every-buffer
     (treemacs-save-position
      (dolist (project (treemacs-workspace->projects workspace))
        (treemacs-walk-reentry-dom (-> project treemacs-project->path treemacs-find-in-dom)
          (lambda (dom-node)
            (-let [path (treemacs-dom-node->key dom-node)]
              (when (and (stringp path)
                         (--any? (string-match-p it path) litter-list))
                (--when-let (treemacs-find-node path project)
                  (goto-char it)
                  (treemacs-toggle-node :purge)))))))))
    (treemacs-pulse-on-success "Cleanup complete.")))

(defun treemacs-icon-catalogue ()
  "Showcase a catalogue of all treemacs themes and their icons."
  (interactive)
  (switch-to-buffer (get-buffer-create "*Treemacs Icons*"))
  (erase-buffer)
  (dolist (theme (nreverse treemacs--themes))
    (insert (format "* Theme %s\n\n" (treemacs-theme->name theme)))
    (insert " |------+------------|\n")
    (insert " | Icon | Extensions |\n")
    (insert " |------+------------|\n")
    (let* ((icons (treemacs-theme->gui-icons theme))
           (rev-icons (make-hash-table :size (ht-size icons) :test 'equal))
           (txt))
      (treemacs--maphash  icons (ext icon)
        (let* ((display (get-text-property 0 'display icon))
               (saved-exts (ht-get rev-icons display)))
          (if saved-exts
              (cl-pushnew ext saved-exts)
            (setf saved-exts (list ext)))
          (ht-set! rev-icons display saved-exts)))
      (treemacs--maphash rev-icons (display exts)
        (push
         (format " | %s | %s |\n"
                 (propertize "x" 'display display)
                 (s-join " " (-map #'prin1-to-string exts)))
         txt))
      (insert (apply #'concat (nreverse txt)))
      (with-no-warnings
        (org-mode)
        (org-table-align))))
  (goto-char 0))

(provide 'treemacs-interface)

;;; treemacs-interface.el ends here
