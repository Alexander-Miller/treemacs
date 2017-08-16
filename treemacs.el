;;; treemacs.el --- A tree style file explorer package -*- lexical-binding: t -*-

;; Copyright (C) 2017 Alexander Miller

;; Author: Alexander Miller <alexanderm@web.de>
;; Package-Requires: ((emacs "25.1") (cl-lib "0.5") (dash "2.11.0") (s "1.10.0") (f "0.11.0") (ace-window "0.9.0") (pfuture "1.1"))
;; Homepage: https://github.com/Alexander-Miller/treemacs
;; Version: 1.9

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
(require 'treemacs-interface)
(require 'treemacs-persist)
(require 'treemacs-tags)

(declare-function projectile-project-p "projectile")
(declare-function projectile-project-root "projectile")

(defconst treemacs-version "1.9")

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


(provide 'treemacs)

;;; treemacs.el ends here
