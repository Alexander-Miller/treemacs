;;; treemacs.el --- A tree style file explorer package -*- lexical-binding: t -*-

;; Copyright (C) 2017 Alexander Miller

;; Author: Alexander Miller <alexanderm@web.de>
;; Package-Requires: ((emacs "25.1") (cl-lib "0.5") (dash "2.11.0") (s "1.10.0") (f "0.11.0") (ace-window "0.9.0") (pfuture "1.1"))
;; Homepage: https://github.com/Alexander-Miller/treemacs
;; Version: 1.8.1

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

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 's)
(require 'f)
(require 'treemacs-customization)
(require 'treemacs-faces)
(require 'treemacs-visuals)
(require 'treemacs-branch-creation)
(require 'treemacs-impl)
(require 'treemacs-follow-mode)
(require 'treemacs-filewatch-mode)
(require 'treemacs-mode)
(require 'treemacs-persist)
(require 'treemacs-tags)

(declare-function projectile-project-p "projectile")
(declare-function projectile-project-root "projectile")

(defconst treemacs-version "1.8.1")

;;;###autoload
(defun treemacs-toggle ()
  "If a treemacs buffer exists and is visible hide it.
If a treemacs buffer exists, but is not visible bring it to the foreground
and select it.
If no treemacs buffer exists call `treemacs'."
  (interactive)
  (cond
   ((treemacs--is-visible?)
    (treemacs--select-visible)
    (treemacs--refresh-on-ui-change)
    (if (one-window-p)
        (switch-to-buffer (other-buffer))
      (bury-buffer)))
   ((treemacs--buffer-exists?)
    (treemacs--select-not-visible)
    (treemacs--refresh-on-ui-change))
   (t
    (treemacs))))

;;;###autoload
(defun treemacs (&optional arg)
  "Open treemacs with current buffer's directory as root.
If the current buffer's `default-directory' is nil, use $HOME as fallback.
If a prefix argument ARG is given manually select the root directory."
  (interactive "P")
  (treemacs--init (cond
                   (arg (read-directory-name "Treemacs root: "))
                   (default-directory default-directory)
                   (t (getenv "HOME")))))

;;;###autoload
(defun treemacs-projectile (&optional arg)
  "Open treemacs for the current projectile project.
If not in a project do nothing. If a prefix argument ARG is given select
the project from among `projectile-known-projects'."
  (interactive "P")
  (if (boundp 'projectile-known-projects)
      (cond
       (arg
        (treemacs--init (completing-read "Project: " projectile-known-projects)))
       ((projectile-project-p)
        (treemacs--init (projectile-project-root)))
       (t (treemacs--log "You're not in a project.")))
    (user-error "Could't initialize at project root - 'projectile-known-projects' is not defined. Is projectile loaded?")))

;;;###autoload
(defun treemacs-next-line ()
  "Goto next line."
  (interactive)
  (forward-line 1)
  (treemacs--evade-image))

;;;###autoload
(defun treemacs-previous-line ()
  "Goto previous line."
  (interactive)
  (forward-line -1)
  (treemacs--evade-image))

;;;###autoload
(defun treemacs-push-button ()
  "Push the button in the current line.
For directories, files and tag sections expand/close the button.
For tags go to the tag definition via `treemacs-visit-node-no-split'."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (forward-button 1)
    (push-button))
  (treemacs--evade-image))

;;;###autoload
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
      (push-button)
      ;; for whatever reason a call to `treemacs--evade-image' here results in point
      ;; jumping to the next line when a node is closed
      (goto-char (1+ p)))))

;;;###autoload
(defun treemacs-uproot ()
  "Switch treemacs' root directory to current root's parent, if possible."
  (interactive)
  (let* ((root      (treemacs--current-root))
         (new-root  (treemacs--parent root)))
    (unless (s-equals? root new-root)
      (treemacs--stop-watching-all)
      (treemacs--build-tree new-root)
      (treemacs--goto-button-at root)
      (treemacs--evade-image))))

;;;###autoload
(defun treemacs-goto-parent-node ()
  "Select parent of selected node, if possible."
  (interactive)
  (beginning-of-line)
  (-some-> (next-button (point)) (button-get 'parent) (button-start) (goto-char))
  (treemacs--evade-image))

;;;###autoload
(defun treemacs-next-neighbour ()
  "Select next node at the same depth as currently selected node, if possible."
  (interactive)
  (beginning-of-line)
  (-some-> (next-button (point))
           (button-get 'next-node)
           (button-start)
           (goto-char)))

;;;###autoload
(defun treemacs-previous-neighbour ()
  "Select previous node at the same depth as currently selected node, if possible."
  (interactive)
  (beginning-of-line)
  (-some-> (next-button (point))
           (button-get 'prev-node)
           (button-start)
           (goto-char)))

;;;###autoload
(defun treemacs-refresh ()
  "Refresh and rebuild treemacs buffer."
  (interactive)
  (-if-let (treemacs-buffer (get-buffer treemacs--buffer-name))
      (treemacs--without-following
       (with-selected-window (get-buffer-window treemacs-buffer)
         (treemacs--cancel-refresh-timer)
         (let* ((curr-line    (line-number-at-pos))
                (curr-btn     (progn (beginning-of-line) (next-button (point) t)))
                (curr-state   (button-get curr-btn 'state))
                (curr-file    (treemacs--nearest-path curr-btn))
                (curr-tagpath (treemacs--tags-path-of curr-btn))
                (win-start    (window-start (get-buffer-window)))
                (root-btn     (treemacs--current-root-btn))
                (root         (button-get root-btn 'abs-path)))
           (treemacs--build-tree root)
           ;; move point to the same file it was with before the refresh if the file
           ;; still exists and is visible, stay in the same line otherwise
           (pcase curr-state
             ((or 'dir-node-open 'dir-node-closed 'file-node-open 'file-node-closed)
              (if (and (f-exists? curr-file)
                       (or treemacs-show-hidden-files
                           (not (s-matches? treemacs-dotfiles-regex (f-filename curr-file)))))
                  (treemacs--goto-button-at curr-file)
                ;; not pretty, but there can still be some off by one jitter when
                ;; using forwald-line
                (treemacs--without-messages (with-no-warnings (goto-line curr-line)))))
             ((or 'tag-node-open 'tag-node-closed 'tag-node)
              (treemacs--goto-tag-button-at curr-tagpath curr-file win-start))
             (_ (treemacs--log "Refresh doesn't yet know how to deal with '%s'" curr-state)))
           (treemacs--evade-image)
           (set-window-start (get-buffer-window) win-start)
           ;; needs to be turned on again when refresh is called from outside the
           ;; treemacs window, otherwise it looks like the selection disappears
           (hl-line-mode t)
           (treemacs--log "Refresh complete."))))
    (treemacs--log "There is nothing to refresh.")))

;;;###autoload
(defun treemacs-change-root ()
  "Use currently selected directory as new root. Do nothing for files."
  (interactive)
  (let ((btn (treemacs--current-button)))
    (pcase (button-get btn 'state)
      ((or 'dir-node-open 'dir-node-closed)
       (treemacs--stop-watching-all)
       (treemacs--build-tree (button-get btn 'abs-path)))
      (_
       (treemacs--log "Button in current line is not a directory.")))))

;;;###autoload
(defun treemacs-visit-node-vertical-split ()
  "Open current file or tag by vertically splitting `next-window'."
  (interactive)
  (treemacs--execute-button-action
   :split-function #'split-window-vertically
   :file-action (find-file (button-get btn 'abs-path))
   :tag-action (treemacs--goto-tag btn)
   :no-match-explanation "This action only works for files and tags."))

;;;###autoload
(defun treemacs-visit-node-horizontal-split ()
  "Open current file or tag by horizontally splitting `next-window'."
  (interactive)
  (treemacs--execute-button-action
   :split-function #'split-window-horizontally
   :file-action (find-file (button-get btn 'abs-path))
   :tag-action (treemacs--goto-tag btn)
   :no-match-explanation "This action only works for files and tags."))

;;;###autoload
(defun treemacs-visit-node-no-split ()
  "Open current file or tag within `next-window'."
  (interactive)
  (treemacs--execute-button-action
   :file-action (find-file (button-get btn 'abs-path))
   :tag-action (treemacs--goto-tag btn)
   :no-match-explanation "This action only works for files and tags."))

;;;###autoload
(defun treemacs-visit-node-ace ()
  "Open current file or tag in window selected by `ace-window'."
  (interactive)
  (treemacs--execute-button-action
   :window (aw-select "Select window")
   :file-action (find-file (button-get btn 'abs-path))
   :tag-action (treemacs--goto-tag btn)
   :no-match-explanation "This action only works for files and tags."))

;;;###autoload
(defun treemacs-visit-node-ace-horizontal-split ()
  "Open current file by horizontally splitting window selected by `ace-window'."
  (interactive)
  (treemacs--execute-button-action
   :split-function #'split-window-horizontally
   :window (aw-select "Select window")
   :file-action (find-file (button-get btn 'abs-path))
   :tag-action (treemacs--goto-tag btn)
   :no-match-explanation "This action only works for files and tags."))

;;;###autoload
(defun treemacs-visit-node-ace-vertical-split ()
  "Open current file by vertically splitting window selected by `ace-window'."
  (interactive)
  (treemacs--execute-button-action
   :split-function #'split-window-vertically
   :window (aw-select "Select window")
   :file-action (find-file (button-get btn 'abs-path))
   :tag-action (treemacs--goto-tag btn)
   :no-match-explanation "This action only works for files and tags."))

;;;###autoload
(defun treemacs-xdg-open ()
  "Open current file, using the `xdg-open' shell command."
  (interactive)
  (-when-let (path (treemacs--prop-at-point 'abs-path))
    (when (f-exists? path)
      (call-process-shell-command (format "xdg-open \"%s\" &" path)))))

;;;###autoload
(defun treemacs-kill-buffer ()
  "Kill the treemacs buffer."
  (interactive)
  (when (s-equals? treemacs--buffer-name (buffer-name))
    (kill-this-buffer)
    (when (not (one-window-p))
      (delete-window))))

;;;###autoload
(defun treemacs-delete ()
  "Delete node at point.
A delete action must always be confirmed. Directories are deleted recursively."
  (interactive)
  (beginning-of-line)
  (-if-let (btn (next-button (point)))
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
                (treemacs--clear-from-cache path t)
                (treemacs--kill-buffers-after-deletion path nil)
                t)))
          (treemacs--without-messages (treemacs-refresh)))))
  (treemacs--evade-image))

;;;###autoload
(defun treemacs-create-file (dir filename)
  "In directory DIR create file called FILENAME."
  (interactive "DDirectory: \nMFilename: ")
  (let ((created-path (f-join dir filename)))
    (f-touch created-path)
    (treemacs--without-messages (treemacs-refresh))
    (treemacs--do-follow created-path)))

;;;###autoload
(defun treemacs-create-dir (dir dirname)
  "In directory DIR create directory called DIRNAME."
  (interactive "DCreate in: \nMDirname: ")
  (let ((created-path (f-join dir dirname)))
    (f-mkdir created-path)
    (treemacs--without-messages (treemacs-refresh))
    (treemacs--do-follow created-path)))

;;;###autoload
(defun treemacs-toggle-show-dotfiles ()
  "Toggle the hiding and displaying of dotfiles."
  (interactive)
  (setq treemacs-show-hidden-files (not treemacs-show-hidden-files))
  (treemacs-refresh)
  (treemacs--log (concat "Dotfiles will now be "
                         (if treemacs-show-hidden-files
                             "displayed." "hidden."))))

;;;###autoload
(defun treemacs-toggle-fixed-width ()
  "Toggle whether the treemacs buffer should have a fixed width.
See also `treemacs-width.'"
  (interactive)
  (if window-size-fixed
      (setq window-size-fixed nil)
    (setq window-size-fixed 'width))
  (treemacs--log "Treemacs buffer width has been %s."
                 (if window-size-fixed "locked" "unlocked")))

;;;###autoload
(defun treemacs-reset-width (&optional arg)
  "Reset the width of the treemacs buffer to `treemacs-buffer-width'.
If a prefix argument ARG is provided read a new value for
`treemacs-buffer-width' first."
  (interactive "P")
  (let ((window-size-fixed nil))
    (when arg (setq treemacs-width (read-number "New Width: ")))
    (treemacs--set-width treemacs-width)))

;;;###autoload
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

;;;###autoload
(defun treemacs-yank-path-at-point ()
  "Copy the absolute path of the node at point."
  (interactive)
  (let ((yank (-> (treemacs--prop-at-point 'abs-path)
                  (f-full)
                  (kill-new))))
    (treemacs--log "Yanked path: %s" (propertize yank 'face 'font-lock-string-face))))

;;;###autoload
(defun treemacs-yank-root ()
  "Copy the absolute path of the current treemacs root."
  (interactive)
  (let ((yank (-> default-directory
                  (f-full)
                  (kill-new))))
    (treemacs--log "Yanked root: %s" (propertize yank 'face 'font-lock-string-face))))

;;;###autoload
(defun treemacs-delete-other-windows ()
  "Same as `delete-other-windows', but will not delete the treemacs window."
  (interactive)
  (let ((w (selected-window)))
    (--each (window-list (selected-frame))
      (unless (or (eq it w)
                  (-> it (window-buffer) (buffer-name) (string-equal treemacs--buffer-name)))
        (delete-window it)))))

;;;###autoload
(defun treemacs-select-window ()
  "Select the treemacs window if it is visible.
Call `treemacs-toggle' if it is not."
  (interactive)
  (-if-let (w (treemacs--is-visible?))
      (select-window w t)
    (treemacs-toggle)))


(provide 'treemacs)

;;; treemacs.el ends here
