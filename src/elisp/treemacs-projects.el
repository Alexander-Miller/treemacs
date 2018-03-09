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
;;; TODO

;;; Code:

(require 'dash)
(require 'ht)
(require 'treemacs-impl)
(require 'treemacs-structure)
(eval-and-compile
  (require 'cl-lib)
  (require 'treemacs-macros))

(treemacs-import-functions-from "treemacs-branch-creation"
  treemacs--add-root-element)

(-defstruct treemacs-project name path)

(-defstruct treemacs-workspace name projects)

(defvar-local treemacs--project-positions (make-hash-table :test #'equal :size 20))

(defvar treemacs-current-workspace (make-treemacs-workspace :name "Default Workspace"))

(defsubst treemacs-current-workspace ()
  "Get the current workspace."
  treemacs-current-workspace)

(defsubst treemacs--add-project-to-current-workspace (project)
  "Add PROJECT to the current workspace."
  (setf (treemacs-workspace->projects treemacs-current-workspace)
        (push project (treemacs-workspace->projects treemacs-current-workspace))))

(defsubst treemacs--remove-project-from-current-workspace (project)
  "Add PROJECT to the current workspace."
  (setf (treemacs-workspace->projects treemacs-current-workspace)
        (delete project (treemacs-workspace->projects treemacs-current-workspace))))

(defsubst treemacs--set-project-position (project position)
  "Insert PROJECT's POSITION into `treemacs--project-positions'."
  (ht-set! treemacs--project-positions project position))

(defsubst treemacs-project->position (project)
  "Return the position of PROJECT in the current buffer."
  (ht-get treemacs--project-positions project))

(defsubst treemacs-project->is-expanded? (project)
  "Return non-nil if PROJECT is expanded in the current buffer."
  (eq 'root-node-open (button-get (treemacs-project->position project) :state)))

(defsubst treemacs-project->is-last? (project)
  "Return t when PROJECT's root node is the last in the view."
  (-> project
      (treemacs-project->position)
      (button-end)
      (next-single-property-change :project)
      (null)))

(defun treemacs-add-project-at (path)
  "Add project at PATH to the current workspace."
  ;; TODO validate path
  (setq path (treemacs--unslash (f-long path)))
  (-let*- [(name (read-string "Project Name: " (f-filename path)))
           (project (make-treemacs-project :name name :path path))
           (empty-workspace? (-> treemacs-current-workspace (treemacs-workspace->projects) (null)))]
    (treemacs--add-project-to-current-workspace project)
    (treemacs-run-in-every-buffer
     (treemacs-with-writable-buffer
      (if empty-workspace?
          (progn
            (goto-char (point-min))
            (treemacs--reset-index))
        (goto-char (point-max))
        (if (treemacs-current-button)
            (insert "\n\n")
          (insert "\n")))
      (treemacs--add-root-element project)
      (treemacs--add-project-to-current-workspace project)
      (treemacs--insert-shadow-node (make-treemacs-shadow-node
                              :key path :position (treemacs-project->position project)))))))

(defsubst treemacs-project-at-point ()
  "Get the `cl-struct-treemacs-project' for the (nearest) project at point."
  (-let [btn (treemacs-current-button)]
    ;; if we're not on a button now look for the next best or previous best button
    (unless btn
      (--if-let (next-button (point))
          (setq btn it)
        (--if-let (previous-button (point))
            (setq btn it)
          (error "[Treemacs] It looks like the treemacs buffer is empty. That shouldn't happen!"))))
    (-let [project (button-get btn :project)]
      (while (not project)
        (setq btn (button-get btn :parent)
              project (button-get btn :project)))
      project)))

(provide 'treemacs-projects)

;;; treemacs-projects.el ends here
