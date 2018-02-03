;;; treemacs-projectile.el --- Projectile integration for treemacs -*- lexical-binding: t -*-

;; Copyright (C) 2018 Alexander Miller

;; Author: Alexander Miller <alexanderm@web.de>
;; Package-Requires: ((projectile "0.14.0") (treemacs "0"))
;; Package-Version: 0
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
;;; Projectile integration for treemacs

;;; Code:

(require 'treemacs)
(require 'projectile)

;;;###autoload
(defun treemacs-projectile (&optional arg)
  "Open treemacs for the current projectile project.
If not in a project do nothing. If a prefix argument ARG is given select
the project from among `projectile-known-projects'."
  (interactive "P")
  (cond
   (arg
    (treemacs--init (completing-read "Project: " projectile-known-projects)))
   ((projectile-project-p)
    (treemacs--init (projectile-project-root)))
   (t (treemacs-log "You're not in a project."))))

;;;###autoload
(defun treemacs-projectile-toggle ()
  "If a treemacs buffer exists and is visible hide it.
If a treemacs buffer exists, but is not visible bring it to the foreground
and select it.
If no treemacs buffer exists call `treemacs-projectile'."
  (interactive)
  (pcase (treemacs--current-visibility)
    ('visible
     (treemacs--select-visible)
     (if (one-window-p)
         (switch-to-buffer (other-buffer))
       (bury-buffer)))
    ('exists
     (treemacs--select-not-visible))
    ('none
     (treemacs-projectile))
    (_ (error "[Treemacs] Invalid visibility value: %s" (treemacs--current-visibility)))))

(defun treemacs-projectile-create-header (root)
  "Try to use the projectile project name for ROOT as treemacs' header.
If not projectile name was found call `treemacs--create-header' for ROOT instead."
  (-if-let (project-name (condition-case nil
                             (projectile-project-root)
                           (error nil)))
      (format "*%s*" (funcall projectile-project-name-function project-name))
    (treemacs--create-header root)))

(provide 'treemacs-projectile)

;;; treemacs-projectile.el ends here
