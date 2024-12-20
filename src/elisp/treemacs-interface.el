;;; treemacs.el --- A tree style file viewer package -*- lexical-binding: t -*-

;; Copyright (C) 2024 Alexander Miller

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
(require 'treemacs-logging)

(eval-when-compile
  (require 'cl-lib)
  (require 'treemacs-macros))

(autoload 'ansi-color-apply-on-region "ansi-color")

(treemacs-import-functions-from "ace-window"
  ace-select-window)

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
   :on-nil              (treemacs-pulse-on-failure "There is nothing to do here.")
   :fallback            (treemacs-TAB-action)))

(defun treemacs-toggle-node-prefer-tag-visit (&optional arg)
  "Same as `treemacs-toggle-node' but will visit a tag node in some conditions.
Tag nodes, despite being expandable sections, will be visited in the following
conditions:

 * Tags belong to a .py file and the tag section's first child element's label
   ends in \" definition*\". This indicates the section is the parent element in
   a nested class/function definition and can be moved to.
 * Tags belong to a .org file and the tag section element possesses a
   \\='org-imenu-marker text property. This indicates that the section is a
   headline with further org elements below it.

The prefix argument ARG is treated the same way as with `treemacs-toggle-node'."
  (interactive)
  (run-hook-with-args
   'treemacs-after-visit-functions
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
    :on-nil              (treemacs-pulse-on-failure "There is nothing to do here."))))

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
Stay in the current window with a single prefix argument ARG, or close the
treemacs window with a double prefix argument."
  (interactive "P")
  (run-hook-with-args
   'treemacs-after-visit-functions
   (treemacs--execute-button-action
    :split-function #'split-window-vertically
    :file-action (find-file (treemacs-safe-button-get btn :path))
    :dir-action (dired (treemacs-safe-button-get btn :path))
    :tag-section-action (treemacs--visit-or-expand/collapse-tag-node btn arg nil)
    :tag-action (treemacs--goto-tag btn)
    :window-arg arg
    :no-match-explanation "Node is neither a file, a directory or a tag - nothing to do here.")))

(defun treemacs-visit-node-horizontal-split (&optional arg)
  "Open current file or tag by horizontally splitting `next-window'.
Stay in the current window with a single prefix argument ARG, or close the
treemacs window with a double prefix argument."
  (interactive "P")
  (run-hook-with-args
   'treemacs-after-visit-functions
   (treemacs--execute-button-action
    :split-function #'split-window-horizontally
    :file-action (find-file (treemacs-safe-button-get btn :path))
    :dir-action (dired (treemacs-safe-button-get btn :path))
    :tag-section-action (treemacs--visit-or-expand/collapse-tag-node btn arg nil)
    :tag-action (treemacs--goto-tag btn)
    :window-arg arg
    :no-match-explanation "Node is neither a file, a directory or a tag - nothing to do here.")))

(defun treemacs-visit-node-close-treemacs (&optional _)
  "Open current node without and close treemacs.
Works just like calling `treemacs-visit-node-no-split' with a double prefix
arg."
  (interactive "P")
  (treemacs-visit-node-no-split '(16)))

(defun treemacs-visit-node-no-split (&optional arg)
  "Open current node without performing any window split or window selection.
The node will be displayed in the window next to treemacs, the exact selection
is determined by `next-window'.  If the node is already opened in some other
window then that window will be selected instead.
Stay in the current window with a single prefix argument ARG, or close the
treemacs window with a double prefix argument."
  (interactive "P")
  (run-hook-with-args
   'treemacs-after-visit-functions
   (treemacs--execute-button-action
    :file-action (find-file (treemacs-safe-button-get btn :path))
    :dir-action (dired (treemacs-safe-button-get btn :path))
    :tag-section-action (treemacs--visit-or-expand/collapse-tag-node btn arg nil)
    :tag-action (treemacs--goto-tag btn)
    :window-arg arg
    :ensure-window-split t
    :window  (-some-> btn (treemacs--nearest-path) (get-file-buffer) (get-buffer-window))
    :no-match-explanation "Node is neither a file, a directory or a tag - nothing to do here.")))

(defun treemacs-visit-node-ace (&optional arg)
  "Open current file or tag in window selected by `ace-window'.
Stay in the current window with a single prefix argument ARG, or close the
treemacs window with a double prefix argument."
  (interactive "P")
  (run-hook-with-args
   'treemacs-after-visit-functions
   (treemacs--execute-button-action
    :window (ace-select-window)
    :file-action (find-file (treemacs-safe-button-get btn :path))
    :dir-action (dired (treemacs-safe-button-get btn :path))
    :tag-section-action (treemacs--visit-or-expand/collapse-tag-node btn arg nil)
    :tag-action (treemacs--goto-tag btn)
    :window-arg arg
    :ensure-window-split t
    :no-match-explanation "Node is neither a file, a directory or a tag - nothing to do here.")))

(defun treemacs-visit-node-in-most-recently-used-window (&optional arg)
  "Open current file or tag in window selected by `get-mru-window'.
Stay in the current window with a single prefix argument ARG, or close the
treemacs window with a double prefix argument."
  (interactive "P")
  (run-hook-with-args
   'treemacs-after-visit-functions
   (treemacs--execute-button-action
    :window (get-mru-window (selected-frame) nil :not-selected)
    :file-action (find-file (treemacs-safe-button-get btn :path))
    :dir-action (dired (treemacs-safe-button-get btn :path))
    :tag-section-action (treemacs--visit-or-expand/collapse-tag-node btn arg nil)
    :tag-action (treemacs--goto-tag btn)
    :window-arg arg
    :ensure-window-split t
    :no-match-explanation "Node is neither a file, a directory or a tag - nothing to do here.")))

(defun treemacs-visit-node-ace-horizontal-split (&optional arg)
  "Open current file by horizontally splitting window selected by `ace-window'.
Stay in the current window with a single prefix argument ARG, or close the
treemacs window with a double prefix argument."
  (interactive "P")
  (run-hook-with-args
   'treemacs-after-visit-functions
   (treemacs--execute-button-action
    :split-function #'split-window-horizontally
    :window (ace-select-window)
    :file-action (find-file (treemacs-safe-button-get btn :path))
    :dir-action (dired (treemacs-safe-button-get btn :path))
    :tag-section-action (treemacs--visit-or-expand/collapse-tag-node btn arg nil)
    :tag-action (treemacs--goto-tag btn)
    :window-arg arg
    :no-match-explanation "Node is neither a file, a directory or a tag - nothing to do here.")))

(defun treemacs-visit-node-ace-vertical-split (&optional arg)
  "Open current file by vertically splitting window selected by `ace-window'.
Stay in the current window with a single prefix argument ARG, or close the
treemacs window with a double prefix argument."
  (interactive "P")
  (run-hook-with-args
   'treemacs-after-visit-functions
   (treemacs--execute-button-action
    :split-function #'split-window-vertically
    :window (ace-select-window)
    :file-action (find-file (treemacs-safe-button-get btn :path))
    :dir-action (dired (treemacs-safe-button-get btn :path))
    :tag-section-action (treemacs--visit-or-expand/collapse-tag-node btn arg nil)
    :tag-action (treemacs--goto-tag btn)
    :window-arg arg
    :no-match-explanation "Node is neither a file, a directory or a tag - nothing to do here.")))

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

This function's exact configuration is stored in
`treemacs-COLLAPSE-actions-config'."
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

(defun treemacs-toggle-show-dotfiles ()
  "Toggle the hiding and displaying of dotfiles.

For toggling the display of git-ignored files see
`treemacs-hide-gitignored-files-mode'."
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

  (defun treemacs-increase-width (&optional arg)
    "Increase the value for `treemacs-width' with `treemacs-width-increment'.
With a prefix ARG add the increment value multiple times."
    (interactive "P")
    (let* ((treemacs-window (treemacs-get-local-window))
            (multiplier (if (numberp arg) arg 1))
            (old-width (window-body-width treemacs-window))
            (new-width (+ old-width (* multiplier treemacs-width-increment))))
      (setq treemacs-width new-width)
      (treemacs--set-width new-width)
      (let ((current-size (window-body-width treemacs-window)))
        (when (not (eq current-size new-width))
          (setq treemacs-width old-width)
          (treemacs--set-width old-width)
          (treemacs-pulse-on-failure "Could not increase window width!")))))

  (defun treemacs-decrease-width (&optional arg)
    "Decrease the value for `treemacs-width' with `treemacs-width-increment'.
With a prefix ARG substract the increment value multiple times."
    (interactive "P")
    (let* ((treemacs-window (treemacs-get-local-window))
            (multiplier (if (numberp arg) arg 1))
            (old-width (window-body-width treemacs-window))
            (new-width (- old-width (* multiplier treemacs-width-increment))))
      (setq treemacs-width new-width)
      (treemacs--set-width new-width)
      (let ((current-size (window-body-width treemacs-window)))
        (when (not (eq current-size new-width))
          (setq treemacs-width old-width)
          (treemacs--set-width old-width)
          (treemacs-pulse-on-failure "Could not decrease window width!")))))

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
     (-let [copied (-> path (file-relative-name (treemacs-project->path project)))]
       (kill-new copied)
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
    (-let [copied (-> project (treemacs-project->path))]
      (kill-new copied)
      (treemacs-pulse-on-success "Copied project path: %s" (propertize copied 'face 'font-lock-string-face))))))

(defun treemacs-copy-filename-at-point ()
  "Copy the filename of the node at point."
  (interactive)
  (treemacs-block
   (-let [path (treemacs--prop-at-point :path)]
     (treemacs-error-return-if (null path)
       "There is nothing to copy here")
     (treemacs-error-return-if (not (stringp path))
       "Path at point is not a file.")
     (let ((filename (file-name-nondirectory path)))
       (kill-new filename)
       (treemacs-pulse-on-success "Copied filename: %s" (propertize filename 'face 'font-lock-string-face))))))

(defun treemacs-paste-dir-at-point-to-minibuffer ()
  "Paste the directory at point into the minibuffer.
This is used by the \"Paste here\" mouse menu button, which assumes that we are
running `treemacs--copy-or-move', so that pasting this path into the minibuffer
allows us to copy/move the previously-selected file into the path at point."
  (interactive)
  (treemacs-block
   (treemacs-error-return-if (not (active-minibuffer-window))
     "Minibuffer is not active")
   (let* ((path-at-point (treemacs--prop-at-point :path))
          (dir (if (file-directory-p path-at-point)
                   path-at-point
                 (file-name-directory path-at-point))))
     (select-window (active-minibuffer-window))
     (delete-region (minibuffer-prompt-end) (point-max))
     (insert dir))
   (message "Copied from treemacs")))

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
      (when (and w (not (equal 'visible (treemacs-current-visibility))))
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
           ;; after renaming, delete and redisplay the project
           (goto-char (treemacs-button-end project-btn))
           (delete-region (line-beginning-position) (line-end-position))
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
     (treemacs-pulse-on-failure "Project '%s' is included in '%s'. Projects may not overlap."
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
                (treemacs-do-remove-project-from-workspace project nil :ask))
             (treemacs-do-remove-project-from-workspace project nil :ask))
      (`success
       (whitespace-cleanup)
       (treemacs-pulse-on-success "Removed project %s from the workspace."
         (propertize (treemacs-project->name project) 'face 'font-lock-type-face)))
      (`user-cancel
       (ignore))
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
    (-let [project (treemacs-project-of-node btn)]
      (treemacs-without-recenter
       (treemacs--do-refresh (current-buffer) project))
      (run-hook-with-args 'treemacs-post-project-refresh-functions project))))

(defun treemacs-collapse-project (&optional arg)
  "Close the project at point.
With a prefix ARG also forget about all the nodes opened in the project."
  (interactive "P")
  (treemacs-unless-let (project (treemacs-project-at-point))
      (treemacs-pulse-on-failure "There is nothing to close here.")
    (-let [btn (treemacs-project->position project)]
      (when (treemacs-is-node-expanded? btn)
        (goto-char btn)
        (treemacs--collapse-root-node btn arg)
        (treemacs--maybe-recenter 'on-distance)))
    (treemacs-pulse-on-success "Collapsed current project")))

(defun treemacs-collapse-all-projects (&optional arg)
  "Collapses all projects.
With a prefix ARG remember which nodes were expanded."
  (interactive "P")
  (-when-let (buffer (treemacs-get-local-buffer))
    (with-current-buffer buffer
      (save-excursion
        (dolist (project (treemacs-workspace->projects (treemacs-current-workspace)))
          (-when-let (pos (treemacs-project->position project))
            (when (eq 'root-node-open (treemacs-button-get pos :state))
              (goto-char pos)
              (treemacs--collapse-root-node pos (not arg))))))
      (treemacs--maybe-recenter 'on-distance)
      (treemacs-pulse-on-success "Collapsed all projects"))))

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
        (-if-let (ws (treemacs--find-workspace-by-name
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
        (when treemacs-hide-gitignored-files-mode
          (treemacs--prefetch-gitignore-cache 'all))
        (treemacs-log "Edit completed successfully."))))))

(defun treemacs-collapse-parent-node (arg)
  "Close the parent of the node at point.
Prefix ARG will be passed on to the closing function
\(see `treemacs-toggle-node'.\)"
  (interactive "P")
  (-if-let* ((btn (treemacs-current-button))
             (parent (button-get btn :parent)))
      (progn
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

(defun treemacs-fit-window-width ()
  "Make treemacs wide enough to display its entire content.

Specifically this will increase (or reduce) the width of the treemacs window to
that of the longest line, counting all lines, not just the ones that are
visible."
  (interactive)
  (let ((longest 0)
        (depth 0))
    (save-excursion
      (goto-char (point-min))
      (while (= 0 (forward-line 1))
        (-let [new-len (- (line-end-position) (line-beginning-position))]
          (when (> new-len longest)
            (setf longest new-len
                  depth (treemacs--prop-at-point :depth))))))
    (let* ((icon-px-diff (* depth (- treemacs--icon-size (frame-char-width))))
           (icon-offset (% icon-px-diff (frame-char-width)))
           (new-width (+ longest icon-offset)))
      (setf treemacs-width new-width)
      (treemacs--set-width new-width)
      (treemacs-pulse-on-success "Width set to %s"
        (propertize (format "%s" new-width) 'face 'font-lock-string-face)))))

(defun treemacs-extra-wide-toggle ()
  "Expand the treemacs window to an extr-wide state (or turn it back).

Specifically this will toggle treemacs' width between
`treemacs-wide-toggle-width' and the normal `treemacs-width'."
  (interactive)
  (if (get 'treemacs-extra-wide-toggle :toggle-on)
      (progn
        (treemacs--set-width treemacs-width)
        (put 'treemacs-extra-wide-toggle :toggle-on nil)
        (treemacs-log "Switched to normal width display"))
    (treemacs--set-width treemacs-wide-toggle-width)
    (put 'treemacs-extra-wide-toggle :toggle-on t)
    (treemacs-log "Switched to extra width display")))

(defun treemacs-next-workspace (&optional arg)
  "Switch to the next workspace.
With a prefix ARG switch to the previous workspace instead."
  (interactive)
  (treemacs-block
   (treemacs-error-return-if (= 1 (length treemacs--workspaces))
     "There is only 1 workspace.")
   (let* ((ws (treemacs-current-workspace))
          (ws-count (length treemacs--workspaces))
          (idx (--find-index (eq it ws) treemacs--workspaces))
          (new-idx (% (+ ws-count (if arg (1- idx) (1+ idx))) ws-count))
          (new-ws (nth new-idx treemacs--workspaces)))
     (treemacs-do-switch-workspace new-ws)
     (treemacs-pulse-on-success "Switched to workspace '%s'"
       (propertize (treemacs-workspace->name new-ws)
                   'face 'font-lock-string-face)))))

(defun treemacs-create-workspace-from-project (&optional arg)
  "Create (and switch to) a workspace containing only the current project.

By default uses the project at point in the treemacs buffer.  If there is no
treemacs buffer, then the project of the current file is used instead.  With a
prefix ARG it is also possible to interactively select the project."
  (interactive "P")
  (treemacs-block
    (-let [project nil]
      (if (eq t treemacs--in-this-buffer)
          (setf project (treemacs-project-of-node (treemacs-current-button)))
        (setf project (treemacs--find-project-for-buffer (buffer-file-name (current-buffer))))
        (treemacs-select-window))
      (when (or arg (null project))
        (setf project (treemacs--select-project-by-name))
        (treemacs-return-if (null project)))
      (let* ((ws-name (treemacs-project->name project))
             (new-ws (treemacs--find-workspace-by-name ws-name)))
        (if new-ws
            (setf (treemacs-workspace->projects new-ws) (list project))
          (-let [ws-create-result (treemacs-do-create-workspace ws-name)]
            (treemacs-error-return-if (not (equal 'success (car ws-create-result)))
              "Something went wrong when creating a new workspace: %s" ws-create-result)
            (setf new-ws (cdr ws-create-result))
            (setf (treemacs-workspace->projects new-ws) (list project))
            (treemacs--persist)))
        (treemacs-do-switch-workspace new-ws)
        (treemacs-pulse-on-success "Switched to project workspace '%s'"
          (propertize ws-name 'face 'font-lock-type-face))))))

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
