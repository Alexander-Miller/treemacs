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
  (require 'treemacs-macros))

(treemacs-import-functions-from "treemacs-branch-creation"
  treemacs--collapse-root-node
  treemacs--expand-root-node
  treemacs--add-root-element)

(treemacs-import-functions-from "treemacs-interface"
  treemacs-previous-project
  treemacs-next-project)

(treemacs-import-functions-from "treemacs-persistence"
  treemacs--persist)

(treemacs--defstruct treemacs-project name path)

(treemacs--defstruct treemacs-workspace name projects)

(defvar treemacs--workspaces (list (make-treemacs-workspace :name "Default Workspace")))

(defvar-local treemacs--project-positions nil)

(defvar-local treemacs--project-of-buffer nil
  "The project that the current buffer falls under, if any.");; TODO invalidate when?

(defsubst treemacs-workspaces ()
  "Return the list of all workspaces in treemacs."
  treemacs--workspaces)

(defsubst treemacs-current-workspace ()
  "Get the current workspace.
Workspaces are local to frames and are therefore stored as frame parameters and
not buffer-local values.
This function can be used with `setf'."
  (frame-parameter (selected-frame) 'treemacs-workspace))
(gv-define-setter treemacs-current-workspace (val) `(set-frame-parameter (selected-frame) 'treemacs-workspace ,val))

(defsubst treemacs--find-workspace ()
  "Find the right workspace for the current (uninitialized) treemacs buffer."
  (setf (treemacs-current-workspace) (car treemacs--workspaces)))

(defun treemacs--find-project-for-buffer ()
  "In the current workspace find the project current buffer's file falls under."
  (unless treemacs--project-of-buffer
    (when (buffer-file-name)
      (setq treemacs--project-of-buffer
            (--first (treemacs--is-path-in-dir? (buffer-file-name) (treemacs-project->path it))
                     (treemacs-workspace->projects (treemacs-current-workspace))))))
  treemacs--project-of-buffer)

(defsubst treemacs--find-project-for-path (path)
  "Return the project for PATH in the current workspace."
  (--first (treemacs--is-path-in-dir? path (treemacs-project->path it))
           (treemacs-workspace->projects (treemacs-current-workspace))))

(defsubst treemacs-workspace->is-empty? ()
  "Return t when there are no projects in the current workspace."
  (null (treemacs-workspace->projects (treemacs-current-workspace))))

(defsubst treemacs--add-project-to-current-workspace (project)
  "Add PROJECT to the current workspace."
  (setf (treemacs-workspace->projects (treemacs-current-workspace))
        ;; reversing around to get the order right - new project goes to the *bottom* of the list
        (-let [reversed (nreverse (treemacs-workspace->projects (treemacs-current-workspace)))]
          (nreverse (push project reversed)))))

(defsubst treemacs--remove-project-from-current-workspace (project)
  "Remove PROJECT from the current workspace."
  (setf (treemacs-workspace->projects (treemacs-current-workspace))
        (delete project (treemacs-workspace->projects (treemacs-current-workspace))))
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

(defsubst treemacs-project->refresh! (project)
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

(defun treemacs-do-create-workspace ()
  "Create a new workspace.
Return values may be as follows:

* If a workspace for the given name already exists:
  - the symbol `duplicate-name'
  - the workspace with the duplicate name
* If the given name is invalid:
  - the symbol `invalid-name'
  - the name
* If everything went well:
  - the symbol `success'
  - the created workspace"
  (cl-block body
    (-let [name (read-string "Workspace name: ")]
      (when (treemacs--is-name-invalid? name)
        (cl-return-from body
          `(invalid-name ,name)))
      (-when-let (ws (--first (string= name (treemacs-workspace->name it))
                              treemacs--workspaces))
        (cl-return-from body
          `(duplicate-name ,ws)))
      (-let [workspace (make-treemacs-workspace :name name)]
        (add-to-list 'treemacs--workspaces workspace :append)
        (treemacs--persist)
        `(success ,workspace)))))

(defun treemacs-do-remove-workspace (&optional ask-to-confirm)
  "Delete a workspace.
Ask the user to confirm the deletion when ASK-TO-CONFIRM is t (it will be when
this is called from `treemacs-remove-workspace').
Return values may be as follows:

* If only a single workspace remains:
  - the symbol `only-one-workspace'
* If the user cancel the deletion:
  - the symbol `user-cancel'
* If everything went well:
  - the symbol `success'
  - the deleted workspace
  - the list of the remaining workspaces"
  (cl-block body
    (when (= 1 (length treemacs--workspaces))
      (cl-return-from body 'only-one-workspace))
    (let* ((names->workspaces (--map (cons (treemacs-workspace->name it) it) treemacs--workspaces))
           (name (completing-read "Delete: " names->workspaces nil t))
           (to-delete (cdr (assoc name names->workspaces))))
      (when (and ask-to-confirm
                 (not (yes-or-no-p (format "Delete workspace %s and all its projects?"
                                           (propertize (treemacs-workspace->name to-delete)
                                                       'face 'font-lock-type-face)))))
        (cl-return-from body 'user-cancel))
      ;; TODO re-render
      (setq treemacs--workspaces (delete to-delete treemacs--workspaces))
      (treemacs--persist)
      `(success ,to-delete ,treemacs--workspaces))))

(defun treemacs-do-add-project-to-workspace (path &optional name)
  "Add project at PATH to the current workspace.
NAME is provided during ad-hoc navigation only.
Return values may be as follows:

* If the project for the given path already exists:
  - the symbol `duplicate-project'
  - the project the PATH falls into
* If a project for the given name already exists:
  - the symbol `duplicate-name'
  - the project with the duplicate name
* If the given name is invalid:
  - the symbol `invalid-name'
  - the name
* If everything went well:
  - the symbol `success'
  - the created project

PATH: Filepath
NAME: String"
  (cl-block body
    (setq path (treemacs--canonical-path path))
    (-when-let (project (treemacs--find-project-for-path path))
        (cl-return-from body
          `(duplicate-project ,project)))
    (let* ((name (or name (read-string "Project Name: " (treemacs--filename path))))
           (project (make-treemacs-project :name name :path path))
           (empty-workspace? (-> (treemacs-current-workspace) (treemacs-workspace->projects) (null))))
      (when (treemacs--is-name-invalid? name)
        (cl-return-from body
          `(invalid-name ,name)))
      (-when-let (double (--first (string= name (treemacs-project->name it))
                                  (treemacs-workspace->projects (treemacs-current-workspace))))
        (cl-return-from body
          `(duplicate-name ,double)))
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
      (treemacs--persist)
      `(success ,project))))
(defalias 'treemacs-add-project-at #'treemacs-do-add-project-to-workspace)
(with-no-warnings
  (make-obsolete #'treemacs-add-project-at #'treemacs-do-add-project-to-workspace "v.2.2.1"))

(defun treemacs-do-remove-project-from-workspace (project)
  "Add the given PROJECT to the current workspace.

PROJECT: Project Struct"
  (treemacs-run-in-every-buffer
   (treemacs-with-writable-buffer
    (-let [project-btn (treemacs-project->position project)]
      (goto-char project-btn)
      (when (treemacs-project->is-expanded? project)
        (treemacs--collapse-root-node project-btn t)))
    (kill-whole-line)
    (-let [is-last? (treemacs-project->is-last? project)]
      (if is-last?
          (treemacs-previous-project)
        (kill-whole-line)
        (treemacs-next-project)))
    (treemacs--forget-last-highlight)
    (delete-trailing-whitespace)
    (treemacs--remove-project-from-current-workspace project)
    (--when-let (treemacs-get-local-window)
      (with-selected-window it
        (recenter)))
    (treemacs--evade-image)
    (hl-line-highlight)))
  (treemacs--persist))

(defun treemacs-do-switch-workspace ()
  "Switch to a new workspace.
Return values may be as follows:

* If there are no workspaces to switch to:
  - the symbol `only-one-workspace'
* If everything went well:
  - the symbol `success'
  - the selected workspace"
  (cl-block body
    (when (= 1 (length treemacs--workspaces))
      (cl-return-from body
        'only-one-workspace))
    (let* ((workspaces (->> treemacs--workspaces
                            (--reject (eq it (treemacs-current-workspace)))
                            (--map (cons (treemacs-workspace->name it) it))))
           (name (completing-read "Switch to: " workspaces nil t))
           (selected (cdr (--first (string= (car it) name) workspaces))))
      (setf (treemacs-current-workspace) selected)
      (cl-return-from body
        `(success ,selected)))))

(defun treemacs--is-name-invalid? (name)
  "Validate the NAME of a project or workspace.
Returns t when the name is invalid.

NAME: String"
  (or (string= "" name)
      (not (s-matches? (rx (1+ (or space (syntax word) (syntax symbol) (syntax punctuation)))) name))))

(defsubst treemacs-project-at-point ()
  "Get the project for the (nearest) project at point.
Return nil when `treemacs-current-button' is nil."
  (-when-let (btn (treemacs-current-button))
    (-let [project (button-get btn :project)]
      (while (not project)
        (setq btn (button-get btn :parent)
              project (button-get btn :project)))
      project)))

(provide 'treemacs-workspaces)

;;; treemacs-workspaces.el ends here
