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
(defun treemacs-projectile ()
  "Add of one `projectile-known-projects' to the treemacs workspace."
  (interactive)
  (if (and (bound-and-true-p projectile-known-projects)
           (listp projectile-known-projects))
      (treemacs--init (completing-read "Project: " projectile-known-projects))
    (treemacs-pulse-on-failure "It looks like projectile does not know any projects.")))

(define-key treemacs-mode-map (kbd "C-p p") #'treemacs-projectile)

(provide 'treemacs-projectile)

;;; treemacs-projectile.el ends here
