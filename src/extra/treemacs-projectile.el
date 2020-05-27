;;; treemacs-projectile.el --- Projectile integration for treemacs -*- lexical-binding: t -*-

;; Copyright (C) 2020 Alexander Miller

;; Author: Alexander Miller <alexanderm@web.de>
;; Package-Requires: ((emacs "25.2") (projectile "0.14.0") (treemacs "0.0"))
;; Version: 0
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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;; Projectile integration for treemacs

;;; Code:

(require 'treemacs)
(require 'projectile)

(eval-when-compile
  (require 'treemacs-macros))

;;;###autoload
(defun treemacs-projectile (&optional arg)
  "Add one of `projectile-known-projects' to the treemacs workspace.
With a prefix ARG was for the name of the project instead of using the name of
the project's root directory."
  (interactive)
  (if (and (bound-and-true-p projectile-known-projects)
           (listp projectile-known-projects)
           projectile-known-projects)
      (let* ((projects (--reject (treemacs-is-path (treemacs--canonical-path it) :in-workspace (treemacs-current-workspace))
                                 (-map #'treemacs--unslash projectile-known-projects)))
             (path (completing-read "Project: " projects))
             (name (unless arg (treemacs--filename path))))
        (if (treemacs-workspace->is-empty?)
            (treemacs--init path name)
          (save-selected-window
            (treemacs-select-window)
            ;; not casing the full error list since some are excluded
            (pcase (treemacs-do-add-project-to-workspace path name)
              (`(success ,project)
               (treemacs-pulse-on-success "Added project %s to the workspace."
                 (propertize (treemacs-project->name project) 'face 'font-lock-type-face)))
              (`(duplicate-name ,duplicate)
               (goto-char (treemacs-project->position duplicate))
               (treemacs-pulse-on-failure "A project with the name %s already exists."
                 (propertize (treemacs-project->name duplicate) 'face 'font-lock-type-face)))))))
    (treemacs-pulse-on-failure "It looks like projectile does not know any projects.")))

(define-key treemacs-project-map (kbd "p") #'treemacs-projectile)

(defun treemacs--read-first-project-path ()
  "Overwrites the original definition from `treemacs-core-utils'.
This version will read a directory based on the current project root instead of
the current dir."
  (when (treemacs-workspace->is-empty?)
    (file-truename
     (read-directory-name "Project root: "
                          (condition-case _
                              (projectile-project-root)
                            (error nil))))))

(defun treemacs--projectile-current-user-project-function ()
  "Get the current projectile project root."
  (declare (side-effect-free t))
  (-some-> (projectile-project-root) (file-truename) (treemacs--canonical-path)))

(defun treemacs-projectile--add-file-to-projectile-cache (path)
  "Add created file PATH to projectile's cache."
  (let ((file-buffer (get-file-buffer path))
        (kill? nil))
    (unless file-buffer
      (setf file-buffer (find-file-noselect path)
            kill? t))
    (with-current-buffer file-buffer
      (projectile-find-file-hook-function))
    (when kill? (kill-buffer file-buffer))))

(add-to-list 'treemacs--find-user-project-functions #'treemacs--projectile-current-user-project-function)
(add-hook 'treemacs-create-file-functions #'treemacs-projectile--add-file-to-projectile-cache)

(provide 'treemacs-projectile)

;;; treemacs-projectile.el ends here
