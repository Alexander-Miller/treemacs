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
  "Add the current projectile project to the treemacs workspace.
If not in a project do nothing. With a prefix ARG select a project from
`projectile-known-projects'."
  (interactive "P")
  (cond
   (arg
    (treemacs--init (completing-read "Project: " projectile-known-projects)))
   ((projectile-project-p)
    (treemacs--init (projectile-project-root)))
   (t (treemacs-log "You are not in a project."))))

(provide 'treemacs-projectile)

;;; treemacs-projectile.el ends here
