;;; treemacs.el --- A tree style file viewer package -*- lexical-binding: t -*-

;; Copyright (C) 2018 Alexander Miller

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
;;; General persistence and desktop-save-mode integration.

;;; Code:

(require 'f)
(require 'treemacs-workspaces)
(require 'treemacs-customization)

(defun treemacs--persist ()
  "Persist treemacs' state in `treemacs-persist-file'."
  (unless noninteractive
    (unless (f-exists? treemacs-persist-file)
      (f-mkdir (f-dirname treemacs-persist-file))
      (f-touch treemacs-persist-file))
    (condition-case e
        (-> treemacs-current-workspace
            (list)
            (pp-to-string)
            (f-write 'utf-8 treemacs-persist-file))
      (error (treemacs-log "Error '%s' when persisting workspace." e)))))

(defun treemacs--restore ()
  "Restore treemacs' state from `treemacs-persist-file'."
  (unless noninteractive
    (condition-case e
        (when (f-exists? treemacs-persist-file)
          (-when-let- [workspace (-> treemacs-persist-file (f-read 'utf-8) (read) (car))]
            (dolist (project (treemacs-workspace->projects workspace))
              (unless (-> project (treemacs-project->path) (f-exists?))
                (treemacs-log (format "Project at %s does not exist and was removed from the workspace."
                               (propertize (treemacs-project->path project) 'face 'font-lock-string-face)))
                (setf (treemacs-workspace->projects workspace)
                      (delete project (treemacs-workspace->projects workspace)))))
            (setq treemacs-current-workspace workspace)))
      (error (treemacs-log "Error '%s' when loading the persisted workspace." e)))))

(add-hook 'kill-emacs-hook #'treemacs--persist)

(unless (or noninteractive (featurep 'treemacs))
  (treemacs--restore))

(provide 'treemacs-persistence)

;;; treemacs-persistence.el ends here
