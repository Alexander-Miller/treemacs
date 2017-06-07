;;; treemacs.el --- A tree style file explorer package

;; Copyright (C) 2017 Alexander Miller

;; Author: Alexander Miller <alexanderm@web.de>
;; Homepage: https://github.com/Alexander-Miller/treemacs

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
;;; Autoloaded, user-facing functions.

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 's)
(require 'f)
(require 'treemacs-customization)
(require 'treemacs-impl)
(require 'treemacs-follow-mode)
(require 'treemacs-filewatch)

(declare-function projectile-project-p "projectile")
(declare-function projectile-project-root "projectile")

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
      (delete-window)))
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
  (cond
   ((and arg (bound-and-true-p projectile-known-projects))
    ;; no warnings since `projectile-known-projects' is known here
    (treemacs--init (completing-read "Project: " (with-no-warnings projectile-known-projects))))
   ((projectile-project-p)
    (treemacs--init (projectile-project-root)))
   (t (message "You're not in a project."))))

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
  "Open/close directory. Open file with `treemacs-visit-file-vertical-split'."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (forward-button 1)
    (call-interactively #'push-button))
  (treemacs--evade-image))

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
  (and (-some->
        (next-button (point))
        (button-get 'parent)
        (button-start)
        (goto-char))
       (treemacs--evade-image)))

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
         (let* ((curr-line (line-number-at-pos))
                (curr-path (treemacs--prop-at-point 'abs-path))
                (win-start (window-start (get-buffer-window)))
                (root-btn  (treemacs--current-root-btn))
                (root      (button-get root-btn 'abs-path)))
           (treemacs--build-tree root)
           ;; move point to the same file it was with before the refresh if the file
           ;; still exists and is visible, stay in the same line otherwise
           (if (and (f-exists? curr-path)
                    (or treemacs-show-hidden-files
                        (not (s-matches? treemacs-dotfiles-regex (f-filename curr-path)))))
               (treemacs--goto-button-at curr-path)
             ;; not pretty, but there can still be some off by one jitter when
             ;; using forwald-line
             (treemacs--without-messages (with-no-warnings (goto-line curr-line))))
           (treemacs--evade-image)
           (set-window-start (get-buffer-window) win-start)
           ;; needs to be turned on again when refresh is called from outside the
           ;; treemacs window, otherwise it looks like the selection disappears
           (hl-line-mode t)
           (message "Treemacs buffer refreshed."))))
    (message "Treemacs buffer does not exist.")))

;;;###autoload
(defun treemacs-change-root ()
  "Use currently selected directory as new root. Do nothing for files."
  (interactive)
  (beginning-of-line)
  (let* ((point     (point))
         (btn       (next-button point))
         (state     (button-get btn 'state))
         (new-root  (button-get btn 'abs-path)))
    (if (not (eq 'file state))
        (progn
          (treemacs--stop-watching-all)
          (treemacs--build-tree new-root))
      (goto-char point))))

;;;###autoload
(defun treemacs-visit-file-vertical-split ()
  "Open current file by vertically splitting `next-window'.
Do nothing for directories."
  (interactive)
  (treemacs--without-following
   (treemacs--open-file nil #'split-window-vertically)))

;;;###autoload
(defun treemacs-visit-file-horizontal-split ()
  "Open current file by horizontally splitting `next-window'.
Do nothing for directories."
  (interactive)
  (treemacs--without-following
   (treemacs--open-file nil #'split-window-horizontally)))

;;;###autoload
(defun treemacs-visit-file-no-split ()
  "Open current file within `next-window'.
Do nothing for directories."
  (interactive)
  (treemacs--without-following
   (treemacs--open-file)))

;;;###autoload
(defun treemacs-visit-file-ace ()
  "Open current file in window selected by `ace-window'.
Do nothing for directories."
  (interactive)
  (treemacs--without-following
   (treemacs--open-file
    (aw-select "Select buffer"))))

;;;###autoload
(defun treemacs-visit-file-ace-horizontal-split ()
  "Open current file by horizontally splitting window selected by `ace-window'.
Do nothing for directories."
  (interactive)
  (save-excursion
    (treemacs--without-following
     (treemacs--open-file
      (aw-select "Select buffer") #'split-window-horizontally))))

;;;###autoload
(defun treemacs-visit-file-ace-vertical-split ()
  "Open current file by vertically splitting window selected by `ace-window'.
Do nothing for directories."
  (interactive)
  (save-excursion
    (treemacs--without-following
     (treemacs--open-file
      (aw-select "Select buffer") #'split-window-vertically))))

;;;###autoload
(defun treemacs-xdg-open ()
  "Open current file, using the `xdg-open' shell command.
Do nothing for directories."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((abs-path (button-get (next-button (point)) 'abs-path)))
      (when (f-file? abs-path)
        (call-process-shell-command (format "xdg-open \"%s\" &" abs-path))))))

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
  (message (concat "Dotfiles will now be "
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
  (message "Treemacs buffer width has been %s."
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
  (-> (treemacs--prop-at-point 'abs-path)
      (f-full)
      (kill-new)))

;;;###autoload
(defun treemacs-yank-root ()
  "Copy the absolute path of the current treemacs root."
  (interactive)
  (-> default-directory
      (f-full)
      (kill-new)))

(provide 'treemacs-core)

;;; treemacs-core.el ends here
