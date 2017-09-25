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
;;; Not autoloaded, but user-facing functions.

;;; Code:

(require 'f)
(require 's)
(require 'dash)
(require 'cl-lib)
(require 'treemacs-impl)
(require 'treemacs-filewatch-mode)
(require 'treemacs-follow-mode)
(require 'treemacs-customization)

(treemacs--import-functions-from "treemacs"
  treemacs-refresh
  treemacs-toggle)

(defconst treemacs-valid-button-states
  '(dir-node-open
    dir-node-closed
    file-node-open
    file-node-closed
    tag-node-open
    tag-node-closed
    tag-node)
  "List of all valid values for treemacs buttons' 'state' property.")

(defun treemacs-next-line ()
  "Goto next line."
  (interactive)
  (forward-line 1)
  (treemacs--evade-image))

(defun treemacs-previous-line ()
  "Goto previous line."
  (interactive)
  (forward-line -1)
  (treemacs--evade-image))

(defun treemacs-push-button (&optional arg)
  "Push the button in the current line.
For directories, files and tag sections expand/close the button.
For tags go to the tag definition via `treemacs-visit-node-no-split'.

With a prefix ARG expanding and closing of nodes is recursive."
  (interactive "P")
  (save-excursion
    (treemacs--push-button (treemacs--current-button) arg))
  (treemacs--evade-image))

(defun treemacs-click-mouse1 (event)
  "Do the same as `treemacs-push-button' when mouse1 clicking on an icon.
Clicking anywhere other than an icon does nothing.
Must be bound to a mouse click, or EVENT will not be supplied."
  (interactive "e")
  ;; save-excursion does not work here, presumably because point was already moved by
  ;; the click before this function even runs
  (let ((p (point)))
    (when (and (eq 'mouse-1 (elt event 0))
               (elt (elt event 1) 7)) ;; image object that's clicked on
      (forward-button 1)
      (treemacs-push-button)
      ;; for whatever reason a call to `treemacs--evade-image' here results in point
      ;; jumping to the next line when a node is closed
      (goto-char (1+ p)))))

(defun treemacs-uproot ()
  "Switch treemacs' root directory to current root's parent, if possible."
  (interactive)
  (let* ((root      (treemacs--current-root))
         (new-root  (treemacs--parent root)))
    (unless (s-equals? root new-root)
      (treemacs--build-tree new-root)
      (treemacs--goto-button-at root)
      (treemacs--evade-image))))

(defun treemacs-goto-parent-node ()
  "Select parent of selected node, if possible."
  (interactive)
  (beginning-of-line)
  (-some-> (next-button (point)) (button-get 'parent) (button-start) (goto-char))
  (treemacs--evade-image))

(defun treemacs-next-neighbour ()
  "Select next node at the same depth as currently selected node, if possible."
  (interactive)
  (beginning-of-line)
  (-some-> (next-button (point))
           (button-get 'next-node)
           (button-start)
           (goto-char)))

(defun treemacs-previous-neighbour ()
  "Select previous node at the same depth as currently selected node, if possible."
  (interactive)
  (beginning-of-line)
  (-some-> (next-button (point))
           (button-get 'prev-node)
           (button-start)
           (goto-char)))

(defun treemacs-change-root ()
  "Use currently selected directory as new root. Do nothing for files."
  (interactive)
  (let ((btn (treemacs--current-button)))
    (pcase (button-get btn 'state)
      ((or 'dir-node-open 'dir-node-closed)
       (treemacs--build-tree (button-get btn 'abs-path)))
      (_
       (treemacs--log "Button in current line is not a directory.")))))

(defun treemacs-visit-node-vertical-split (&optional arg)
  "Open current file or tag by vertically splitting `next-window'.
Stay in current window with a prefix argument ARG."
  (interactive "P")
  (treemacs--execute-button-action
   :split-function #'split-window-vertically
   :file-action (find-file (treemacs--safe-button-get btn 'abs-path))
   :dir-action (dired (treemacs--safe-button-get btn 'abs-path))
   :tag-action (treemacs--goto-tag btn)
   :save-window arg
   :no-match-explanation "Node is neither a file, a directory or a tag - nothing to do here."))

(defun treemacs-visit-node-horizontal-split (&optional arg)
  "Open current file or tag by horizontally splitting `next-window'.
Stay in current window with a prefix argument ARG."
  (interactive "P")
  (treemacs--execute-button-action
   :split-function #'split-window-horizontally
   :file-action (find-file (treemacs--safe-button-get btn 'abs-path))
   :dir-action (dired (treemacs--safe-button-get btn 'abs-path))
   :tag-action (treemacs--goto-tag btn)
   :save-window arg
   :no-match-explanation "Node is neither a file, a directory or a tag - nothing to do here."))

(defun treemacs-visit-node-no-split (&optional arg)
  "Open current file or tag within `next-window'.
Stay in current window with a prefix argument ARG."
  (interactive "P")
  (treemacs--execute-button-action
   :file-action (find-file (treemacs--safe-button-get btn 'abs-path))
   :dir-action (dired (treemacs--safe-button-get btn 'abs-path))
   :tag-action (treemacs--goto-tag btn)
   :save-window arg
   :no-match-explanation "Node is neither a file, a directory or a tag - nothing to do here."))

(defun treemacs-visit-node-ace (&optional arg)
  "Open current file or tag in window selected by `ace-window'.
Stay in current window with a prefix argument ARG."
  (interactive "P")
  (treemacs--execute-button-action
   :window (aw-select "Select window")
   :file-action (find-file (treemacs--safe-button-get btn 'abs-path))
   :dir-action (dired (treemacs--safe-button-get btn 'abs-path))
   :tag-action (treemacs--goto-tag btn)
   :save-window arg
   :no-match-explanation "Node is neither a file, a directory or a tag - nothing to do here."))

(defun treemacs-visit-node-ace-horizontal-split (&optional arg)
  "Open current file by horizontally splitting window selected by `ace-window'.
Stay in current window with a prefix argument ARG."
  (interactive "P")
  (treemacs--execute-button-action
   :split-function #'split-window-horizontally
   :window (aw-select "Select window")
   :file-action (find-file (treemacs--safe-button-get btn 'abs-path))
   :dir-action (dired (treemacs--safe-button-get btn 'abs-path))
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
   :file-action (find-file (treemacs--safe-button-get btn 'abs-path))
   :dir-action (dired (treemacs--safe-button-get btn 'abs-path))
   :tag-action (treemacs--goto-tag btn)
   :save-window arg
   :no-match-explanation "Node is neither a file, a directory or a tag - nothing to do here."))

(defun treemacs-visit-node-default-action (&optional arg)
  "Run the action defined in `treemacs-default-actions' for the current button.
Pass on prefix ARG to the action."
  (interactive "P")
  (-when-let (state (treemacs--prop-at-point 'state))
    (funcall (alist-get state treemacs-default-actions) arg)))

(defun treemacs-define-default-action (state action)
  "Define the behaviour of `treemacs-visit-default-action'.
Determines that a button with state STATE should lead to the execution of
ACTION.

First deleted the previous entry with key STATE from `treemacs-default-actions'
and then inserts the new touple."
  (assq-delete-all state treemacs-default-actions)
  (push (cons state action) treemacs-default-actions))

(defun treemacs-xdg-open ()
  "Open current file, using the `xdg-open' shell command."
  (interactive)
  (-when-let (path (treemacs--prop-at-point 'abs-path))
    (when (f-exists? path)
      (call-process-shell-command (format "xdg-open \"%s\" &" path)))))

(defun treemacs-kill-buffer ()
  "Kill the treemacs buffer."
  (interactive)
  (when (eq 'treemacs-mode major-mode)
    (treemacs--buffer-teardown)
    (if (one-window-p)
        (kill-this-buffer)
      (kill-buffer-and-window))))

(defun treemacs-delete ()
  "Delete node at point.
A delete action must always be confirmed. Directories are deleted recursively."
  (interactive)
  (-if-let (btn (treemacs--current-button))
      (if (not (memq (button-get btn 'state) '(file-node-open file-node-closed dir-node-open dir-node-closed)))
          (treemacs--log "Only files and directories can be deleted.")
        (let* ((path      (button-get btn 'abs-path))
               (file-name (f-filename path)))
          (when
              (cond
               ((f-file? path)
                (when (y-or-n-p (format "Delete %s ? " file-name))
                  (f-delete path)
                  (treemacs--kill-buffers-after-deletion path t)
                  t))
               ((f-directory? path)
                (when (y-or-n-p (format "Recursively delete %s ? " file-name))
                  (f-delete path t)
                  (treemacs--kill-buffers-after-deletion path nil)
                  t)))
            (treemacs--clear-from-cache btn t)
            (treemacs--remove-all-tags-under-path-from-cache path)
            (treemacs--without-messages (treemacs-refresh))))))
  (treemacs--evade-image))

(defun treemacs-create-file (dir filename)
  "In directory DIR create file called FILENAME."
  (interactive "DDirectory: \nMFilename: ")
  (let ((created-path (f-join dir filename)))
    (f-touch created-path)
    (treemacs--without-messages (treemacs-refresh))
    (treemacs--do-follow created-path)))

(defun treemacs-rename ()
  "Rename the currently selected node.
Buffers visiting the renamed file or visiting a file inside a renamed directory
and windows showing them will be reloaded. The list of recent files will
likewise be updated."
  (interactive)
  (treemacs--log
   (catch 'exit
     (let* ((btn (treemacs--current-button))
            (old-path (button-get btn 'abs-path))
            (new-path)
            (new-name)
            (dir))
       (unless old-path
         (throw 'exit "Found nothing to rename here."))
       (unless (file-exists-p old-path)
         (throw 'exit "The file to be renamed does not exist."))
       (setq new-name (read-string "New name: ")
             dir      (f-dirname old-path)
             new-path (f-join dir new-name))
       (when (get-buffer new-name)
         (throw 'exit (format "A buffer named %s already exists."
                              (propertize new-name 'face font-lock-string-face))))
       (when (file-exists-p new-name)
         (throw 'exit (format "A file named %s already exists."
                              (propertize new-name 'face font-lock-string-face))))
       (rename-file old-path new-path)
       (treemacs--replace-recentf-entry old-path new-path)
       (treemacs--update-caches-after-rename old-path new-path)
       (treemacs--reload-buffers-after-rename old-path new-path)
       (treemacs-refresh)
       (treemacs--goto-button-at new-path)
       (throw 'exit (format "Renamed %s to %s."
                            (propertize (f-filename old-path) 'face font-lock-string-face)
                            (propertize new-name 'face font-lock-string-face)))))))

(defun treemacs-create-dir (dir dirname)
  "In directory DIR create directory called DIRNAME."
  (interactive "DCreate in: \nMDirname: ")
  (let ((created-path (f-join dir dirname)))
    (f-mkdir created-path)
    (treemacs--without-messages (treemacs-refresh))
    (treemacs--do-follow created-path)))

(defun treemacs-toggle-show-dotfiles ()
  "Toggle the hiding and displaying of dotfiles."
  (interactive)
  (setq treemacs-show-hidden-files (not treemacs-show-hidden-files))
  (treemacs-refresh)
  (treemacs--log (concat "Dotfiles will now be "
                         (if treemacs-show-hidden-files
                             "displayed." "hidden."))))

(defun treemacs-toggle-fixed-width ()
  "Toggle whether the treemacs buffer should have a fixed width.
See also `treemacs-width.'"
  (interactive)
  (setq treemacs--width-is-locked (not treemacs--width-is-locked))
  (treemacs--log "Window width has been %s."
                 (propertize (if treemacs--width-is-locked "locked" "unlocked")
                             'face 'font-lock-string-face)))

(defun treemacs-reset-width (&optional arg)
  "Reset the width of the treemacs window to `treemacs-buffer-width'.
If a prefix argument ARG is provided read a new value for
`treemacs-buffer-width' first."
  (interactive "P")
  (let ((window-size-fixed nil))
    (when arg
      (setq treemacs-width
            (->> treemacs-width
                 (format "New Width (current = %s): ")
                 (read-number))))
    (treemacs--set-width treemacs-width)))

(cl-defun treemacs-find-file (&optional (path (buffer-file-name (current-buffer))))
  "Find and move point to PATH (or the current file) in the treemacs buffer.
Most likley to be useful when `treemacs-follow-mode' is not active.

Expand folders if needed. If PATH is not under the current root ask to change
the root. If no treemacs buffer exists create it. Do nothing if PATH is not
given and the current buffer is not editing a file."
  (interactive)
  (when (and path (f-exists? path))
    (save-selected-window
      (treemacs--without-following
       (let* ((visibility (treemacs--current-visibility))
              (path       (f-long path))
              (is-dir?    (f-directory? path)))
         ;; get up a visible treemacs buffer, regardless of its current state
         (if (eq visibility 'none)
             ;; finding a file without an existing treemacs is just an init
             ;; nothing else to do here
             (treemacs--init (if is-dir? path (f-dirname path)))
           (progn
             (cl-case visibility
               (exists  (treemacs-toggle))
               (visible (treemacs--select-visible))
               (t       (error "Unkown treemacs buffer visibility '%s'" visibility)))
             ;; find the file given whether is a directory and if it can be found below
             ;; the current root or not
             (let ((root (treemacs--current-root)))
               (if (treemacs--is-path-in-dir? path root)
                   (treemacs--do-follow path root)
                 (when (or treemacs-change-root-without-asking
                           (y-or-n-p "Change the treemacs root to find the file? "))
                   (if is-dir?
                       (treemacs--init path)
                     (progn
                       (treemacs--init (f-dirname path))
                       (treemacs--goto-button-at path)
                       (hl-line-mode -1)
                       (hl-line-mode t)))))))))))))

(defun treemacs-yank-path-at-point ()
  "Copy the absolute path of the node at point."
  (interactive)
  (let ((yank (-> (treemacs--prop-at-point 'abs-path)
                  (f-full)
                  (kill-new))))
    (treemacs--log "Yanked path: %s" (propertize yank 'face 'font-lock-string-face))))

(defun treemacs-yank-root ()
  "Copy the absolute path of the current treemacs root."
  (interactive)
  (let ((yank (-> default-directory
                  (f-full)
                  (kill-new))))
    (treemacs--log "Yanked root: %s" (propertize yank 'face 'font-lock-string-face))))

(defun treemacs-delete-other-windows ()
  "Same as `delete-other-windows', but will not delete the treemacs window."
  (interactive)
  (let ((w (selected-window)))
    (--each (window-list (selected-frame))
      (unless (or (eq it w)
                  (-> it (window-buffer) (buffer-name) (string-equal treemacs--buffer-name)))
        (delete-window it)))))

(defun treemacs-select-window ()
  "Select the treemacs window if it is visible.
Call `treemacs-toggle' if it is not."
  (interactive)
  (-if-let (w (treemacs--is-visible?))
      (select-window w t)
    (treemacs-toggle)))

(provide 'treemacs-interface)

;;; treemacs-interface.el ends here
