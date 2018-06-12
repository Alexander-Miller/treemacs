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
(require 'treemacs-visuals)
(require 'treemacs-structure)
(eval-and-compile
  (require 'cl-lib)
  (require 'treemacs-macros))

(treemacs-import-functions-from "treemacs-branch-creation"
  treemacs--collapse-root-node
  treemacs--expand-root-node
  treemacs--add-root-element)

(-defstruct treemacs-project name path)

(-defstruct treemacs-workspace name projects)

(defvar-local treemacs--project-positions nil)

(defvar-local treemacs--project-of-buffer nil
  "The `cl-struct-treemacs-project' that the current buffer falls under, if any.")

(defvar treemacs-current-workspace (make-treemacs-workspace :name "Default Workspace"))

(defun treemacs--find-project-for-buffer ()
  "In the current workspace find the project current buffer's file falls under."
  (unless treemacs--project-of-buffer
    (when (buffer-file-name)
      (setq treemacs--project-of-buffer
            (--first (treemacs--is-path-in-dir? (buffer-file-name) (treemacs-project->path it))
                     (treemacs-workspace->projects treemacs-current-workspace)))))
  treemacs--project-of-buffer)

(defsubst treemacs--find-project-for-path (path)
  "Return the project for PATH in the current workspace."
  (--first (treemacs--is-path-in-dir? path (treemacs-project->path it))
           (treemacs-workspace->projects treemacs-current-workspace)))

(defsubst treemacs-current-workspace ()
  "Get the current workspace."
  treemacs-current-workspace)

(defsubst treemacs-workspace->is-empty? ()
  "Return t when there are no projects in the current workspace."
  (null (treemacs-workspace->projects treemacs-current-workspace)))

(defsubst treemacs--add-project-to-current-workspace (project)
  "Add PROJECT to the current workspace."
  (setf (treemacs-workspace->projects treemacs-current-workspace)
        ;; reversing around to get the order right - new project goes to the *bottom* of the list
        (-let [reversed (nreverse (treemacs-workspace->projects treemacs-current-workspace))]
          (nreverse (push project reversed)))))

(defsubst treemacs--remove-project-from-current-workspace (project)
  "Remove PROJECT from the current workspace."
  (setf (treemacs-workspace->projects treemacs-current-workspace)
        (delete project (treemacs-workspace->projects treemacs-current-workspace)))
  ;; also reset the cached buffers' projects
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (equal treemacs--project-of-buffer project)
        (setq treemacs--project-of-buffer nil)))))

(defsubst treemacs--reset-project-positions ()
  "Reset `treemacs--project-positions'."
  (setq treemacs--project-positions (make-hash-table :test #'equal :size 20)))

(defsubst treemacs--set-project-position (project position)
  "Insert PROJECT's POSITION into `treemacs--project-positions'."
  (ht-set! treemacs--project-positions project position))

(defsubst treemacs-project->position (project)
  "Return the position of PROJECT in the current buffer."
  (ht-get treemacs--project-positions project))

(defsubst treemacs-project->is-expanded? (project)
  "Return non-nil if PROJECT is expanded in the current buffer."
  (eq 'root-node-open (button-get (treemacs-project->position project) :state)))

(defsubst treemacs-project->refresh (project)
  "Refresh PROJECT in the current buffer."
  (when (treemacs-project->is-expanded? project)
    (-let [root-btn (treemacs-project->position project)]
      (goto-char root-btn)
      (treemacs--forget-last-highlight)
      (treemacs--collapse-root-node root-btn)
      (treemacs--expand-root-node root-btn))))

(defsubst treemacs-project->is-last? (project)
  "Return t when PROJECT's root node is the last in the view."
  (-> project
      (treemacs-project->position)
      (button-end)
      (next-single-property-change :project)
      (null)))

(defun treemacs-add-project-at (path &optional name)
  "Add project at PATH to the current workspace.
NAME is provided during ad-hoc navigation only."
  (--if-let (treemacs--find-project-for-path path)
      (progn
        (goto-char (treemacs-project->position it))
        (treemacs-pulse-on-success
            (format "Project for %s already exists."
                    (propertize path 'face 'font-lock-string-face))))
    (-let*- [(name (or name (read-string "Project Name: " (f-filename path))))
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
              (progn
                (insert "\n")
                (when treemacs-space-between-root-nodes (insert "\n")))
            (insert "\n")))
        (treemacs--add-root-element project)
        (treemacs--insert-shadow-node (make-treemacs-shadow-node
                                :key path :position (treemacs-project->position project)))))
      (treemacs-pulse-on-success "Added project %s to the workspace."
        (propertize name 'face 'font-lock-type-face)))))

(defsubst treemacs-project-at-point ()
  "Get the `cl-struct-treemacs-project' for the (nearest) project at point.
Return nil when `treemacs-current-button' is nil."
  (-when-let- [btn (treemacs-current-button)]
    (-let [project (button-get btn :project)]
      (while (not project)
        (setq btn (button-get btn :parent)
              project (button-get btn :project)))
      project)))

(provide 'treemacs-workspaces)

;;; treemacs-workspaces.el ends here
