;;; treemacs.el --- A tree style file viewer package -*- lexical-binding: t -*-

;; Copyright (C) 2024 Alexander Miller

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

;; Everything about creating, (re)moving, (re)naming and otherwise
;; editing projects and workspaces.

;;; Code:

(require 'dash)
(require 'ht)
(require 'treemacs-core-utils)
(require 'treemacs-dom)
(require 'treemacs-scope)
(require 'treemacs-customization)

(eval-when-compile
  (require 'cl-lib)
  (require 'inline)
  (require 'treemacs-macros))

(eval-when-compile
  (cl-declaim (optimize (speed 3) (safety 0))))

(treemacs-import-functions-from "treemacs"
  treemacs-select-window)

(treemacs-import-functions-from "treemacs-rendering"
  treemacs--projects-end
  treemacs--collapse-root-node
  treemacs--expand-root-node
  treemacs--add-root-element
  treemacs--render-projects
  treemacs--insert-root-separator
  treemacs--root-face)

(treemacs-import-functions-from "treemacs-interface"
  treemacs-previous-project
  treemacs-next-project)

(treemacs-import-functions-from "treemacs-persistence"
  treemacs--persist
  treemacs--maybe-load-workspaces)

(treemacs-import-functions-from "treemacs-visuals"
  treemacs-pulse-on-failure)

(treemacs-import-functions-from "treemacs-async"
  treemacs--prefetch-gitignore-cache)

(cl-defstruct (treemacs-project
               (:conc-name treemacs-project->)
               (:constructor treemacs-project->create!))
  name
  path
  path-status
  is-disabled?)

(cl-defstruct (treemacs-workspace
               (:conc-name treemacs-workspace->)
               (:constructor treemacs-workspace->create!))
  name
  projects
  is-disabled?)

(defvar treemacs--workspaces (list (treemacs-workspace->create! :name "Default")))
(defvar treemacs--disabled-workspaces (list))

(defvar treemacs--find-user-project-functions
  (list #'treemacs--current-builtin-project-function
        #'treemacs--current-directory-project-function)
  "List of functions to find the user project for the current buffer.")

(defvar-local treemacs--org-err-ov nil
  "The overlay that will display validations when org-editing.")

(defvar-local treemacs--project-of-buffer nil
  "The project that the current buffer falls under, if any.")

(defvar treemacs-override-workspace nil
  "Used to override the return value of `treemacs-current-workspace'.
Used by `treemacs-run-in-every-buffer' to make sure all workspace-related
functions can be used since make functions (like `treemacs-find-file-node')
rely on the current buffer and workspace being aligned.")

(define-inline treemacs--invalidate-buffer-project-cache ()
  "Set all buffers' `treemacs--project-of-buffer' to nil.
To be called whenever a project or workspace changes."
  (inline-quote
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (setf treemacs--project-of-buffer nil)))))

(defun treemacs--current-builtin-project-function ()
  "Find the current project.el project."
  (declare (side-effect-free t))
  (-when-let (project (project-current))
    (if (fboundp 'project-root)
        (-> project (project-root) (file-truename) (treemacs-canonical-path))
      (-> project (cdr) (file-truename) (treemacs-canonical-path)))))

(defun treemacs--current-directory-project-function ()
  "Find the current working directory."
  (declare (side-effect-free t))
  (-some-> default-directory (treemacs--canonical-path)))

(define-inline treemacs-workspaces ()
  "Return the list of all workspaces in treemacs."
  (declare (side-effect-free t))
  (inline-quote treemacs--workspaces))

(define-inline treemacs-disabled-workspaces ()
  "Return the list of all workspaces in treemacs that are disabled."
  (declare (side-effect-free t))
  (inline-quote treemacs--disabled-workspaces))

(defun treemacs-current-workspace ()
  "Get the current workspace.
The return value can be overridden by let-binding `treemacs-override-workspace'.
This will happen when using `treemacs-run-in-every-buffer' to make sure that
this function returns the right workspace for the iterated-over buffers.

If no workspace is assigned to the current scope the persisted workspaces will
be loaded and a workspace will be found based on the `current-buffer'.

This function can be used with `setf'."
  (or treemacs-override-workspace
      (-if-let (shelf (treemacs-current-scope-shelf))
          (treemacs-scope-shelf->workspace shelf)
        (treemacs--maybe-load-workspaces)
        (let* ((workspace (treemacs--find-workspace (buffer-file-name (current-buffer))))
               (new-shelf (treemacs-scope-shelf->create! :workspace workspace)))
          (setf (treemacs-current-scope-shelf) new-shelf)
          (run-hook-with-args treemacs-workspace-first-found-functions
                              workspace (treemacs-current-scope))
          workspace))))

(gv-define-setter treemacs-current-workspace (val)
  `(let ((shelf (treemacs-current-scope-shelf)))
     (unless shelf
       (setf shelf (treemacs-scope-shelf->create!))
       (push (cons (treemacs-current-scope) shelf) treemacs--scope-storage))
     (setf (treemacs-scope-shelf->workspace shelf) ,val)))

(define-inline treemacs--find-workspace (&optional path)
  "Find the right workspace the given PATH.

PATH: String"
  (declare (side-effect-free t))
  (inline-letevals (path)
    (inline-quote
     (let ((ws-for-path (--first (treemacs-is-path ,path :in-workspace it)
                                 treemacs--workspaces)))
       (setf (treemacs-current-workspace)
             (pcase-exhaustive treemacs-find-workspace-method
               ('find-for-file-or-pick-first
                (or ws-for-path (car treemacs--workspaces)))
               ('find-for-file-or-manually-select
                (or ws-for-path (treemacs--select-workspace-by-name)))
               ('always-ask
                (treemacs--select-workspace-by-name))))))))

;; TODO(2020/11/25): NAME
(define-inline treemacs--find-project-for-buffer (&optional buffer-file)
  "In the current workspace find the project current buffer's file falls under.
Optionally supply the BUFFER-FILE in case it is not available by calling the
function `buffer-file-name' (like in Dired).

FILE: Filepath"
  (inline-letevals (buffer-file)
    (inline-quote
     (progn
       (unless treemacs--project-of-buffer
         (let ((path (or ,buffer-file (buffer-file-name))))
           (when path (setf treemacs--project-of-buffer (treemacs-is-path path :in-workspace)))))
       treemacs--project-of-buffer))))

(define-inline treemacs--find-project-for-path (path)
  "Return the project for PATH in the current workspace."
  (declare (side-effect-free t))
  (inline-letevals (path)
    (inline-quote (treemacs-is-path ,path :in-workspace))))

(define-inline treemacs-workspace->is-empty? ()
  "Return t when there are no projects in the current workspace."
  (declare (side-effect-free t))
  (inline-quote
   (null (treemacs-workspace->projects (treemacs-current-workspace)))))

(define-inline treemacs--add-project-to-current-workspace (project)
  "Add PROJECT to the current workspace."
  (inline-letevals (project)
    (inline-quote
     (setf (treemacs-workspace->projects (treemacs-current-workspace))
           ;; reversing around to get the order right - new project goes to the *bottom* of the list
           (-let [reversed (nreverse (treemacs-workspace->projects (treemacs-current-workspace)))]
             (nreverse (push ,project reversed)))))))

(define-inline treemacs--remove-project-from-current-workspace (project)
  "Remove PROJECT from the current workspace."
  (inline-letevals (project)
    (inline-quote
     (progn
       (setf (treemacs-workspace->projects (treemacs-current-workspace))
             (delete ,project (treemacs-workspace->projects (treemacs-current-workspace))))
       ;; also reset the cached buffers' projects
       (dolist (buffer (buffer-list))
         (with-current-buffer buffer
           (when (equal treemacs--project-of-buffer ,project)
             (setq treemacs--project-of-buffer nil))))))))

(define-inline treemacs--next-project-pos ()
  "Get the position of the next project.
Will return `point-max' if there is no next project."
  (declare (side-effect-free t))
  (inline-quote (next-single-char-property-change (line-end-position) :project)))

(define-inline treemacs--prev-project-pos ()
  "Get the position of the next project.
Will return `point-min' if there is no next project."
  (declare (side-effect-free t))
  (inline-quote (previous-single-char-property-change (line-beginning-position) :project)))

(define-inline treemacs-project->key (self)
  "Get the hash table key of SELF.
SELF may be a project struct or a root key of a top level extension."
  (declare (side-effect-free t))
  (inline-letevals (self)
    (inline-quote
     ;; Top-level extensions are added to the project positions their root-key,
     ;; not a real project.
     (if (treemacs-project-p ,self)
         (treemacs-project->path ,self)
       ,self))))

(define-inline treemacs-project->position (self)
  "Return the position of project SELF in the current buffer."
  (declare (side-effect-free t))
  (inline-letevals (self)
    (inline-quote
     (treemacs-dom-node->position
      (treemacs-find-in-dom (treemacs-project->path ,self))))))

(define-inline treemacs-project->is-expanded? (self)
  "Return non-nil if project SELF is expanded in the current buffer."
  (declare (side-effect-free t))
  (inline-letevals (self)
    (inline-quote
     (memq (-> ,self (treemacs-project->position) (treemacs-button-get :state))
           treemacs--open-node-states))))

(defun treemacs-project->refresh-path-status! (self)
  "Refresh the path status of project SELF in the current buffer.
Does not preserve the current position in the buffer."
  (let ((old-path-status (treemacs-project->path-status self))
        (new-path-status (treemacs--get-path-status (treemacs-project->path self))))
    (unless (eq old-path-status new-path-status)
      (setf (treemacs-project->path-status self) new-path-status)
      ;; When the path transforms from unreadable or disconnected to readable,
      ;; update the :symlink status on its button.
      (let ((pos (treemacs-project->position self))
            (path (treemacs-project->path self)))
        (when (treemacs-project->is-readable? self)
          (treemacs-button-put pos :symlink (file-symlink-p path)))
        (treemacs-button-put pos 'face (treemacs--root-face self))))))

;; TODO(2021/08/17): -> rendering
(defun treemacs-project->refresh! (self)
  "Refresh project SELF in the current buffer.
Does not preserve the current position in the buffer."
  (treemacs-project->refresh-path-status! self)
  (when (treemacs-project->is-expanded? self)
    (let ((root-btn (treemacs-project->position self)))
      (goto-char root-btn)
      (funcall (alist-get (treemacs-button-get root-btn :state)
                          treemacs-TAB-actions-config))
      (unless (treemacs-project->is-unreadable? self)
        (funcall (alist-get (treemacs-button-get root-btn :state)
                            treemacs-TAB-actions-config))))))

(define-inline treemacs-project->is-last? (self)
  "Return t when root node of project SELF is the last in the view."
  (declare (side-effect-free t))
  (inline-letevals (self)
    (inline-quote
     (-> ,self
         (treemacs-project->position)
         (treemacs-button-end)
         (next-single-property-change :project)
         (null)))))

(defun treemacs-do-create-workspace (&optional name)
  "Create a new workspace with optional NAME.
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
  (treemacs-block
   (-let [name (or name (treemacs--read-string "Workspace name: "))]
     (treemacs-return-if (treemacs--is-name-invalid? name)
       `(invalid-name ,name))
     (-when-let (ws (--first (string= name (treemacs-workspace->name it))
                             treemacs--workspaces))
       (treemacs-return `(duplicate-name ,ws)))
     (-let [workspace (treemacs-workspace->create! :name name)]
       (add-to-list 'treemacs--workspaces workspace :append)
       (treemacs--persist)
       (run-hook-with-args 'treemacs-create-workspace-functions workspace)
       `(success ,workspace)))))

(defun treemacs-do-remove-workspace (&optional workspace ask-to-confirm)
  "Delete a WORKSPACE.
Ask the user to confirm the deletion when ASK-TO-CONFIRM is t (it will be when
this is called from `treemacs-remove-workspace').

If no WORKSPACE name is given it will be selected interactively.

Return values may be as follows:

* If only a single workspace remains:
  - the symbol `only-one-workspace'
* If the user cancels the deletion:
  - the symbol `user-cancel'
* If the workspace cannot be found:
  - the symbol `workspace-not-found'
* If everything went well:
  - the symbol `success'
  - the deleted workspace
  - the list of the remaining workspaces"
  (treemacs-block
   (treemacs-return-if (= 1 (length treemacs--workspaces))
     'only-one-workspace)
   (let* ((name (or workspace
                    (completing-read "Delete: " (-map #'treemacs-workspace->name treemacs--workspaces) nil t)))
          (to-delete (treemacs-find-workspace-by-name name)))
     (treemacs-return-if
         (and ask-to-confirm
              (not (yes-or-no-p (format "Delete workspace %s and all its projects?"
                                        (propertize (treemacs-workspace->name to-delete)
                                                    'face 'font-lock-type-face)))))
       'user-cancel)
     (treemacs-return-if (null to-delete)
       `(workspace-not-found ,name))
     (setq treemacs--workspaces (delete to-delete treemacs--workspaces))
     (treemacs--persist)
     (treemacs--invalidate-buffer-project-cache)
     (treemacs-run-in-every-buffer
      (let ((current-ws (treemacs-current-workspace)))
        (when (eq current-ws to-delete)
          (treemacs--rerender-after-workspace-change))))
     (run-hook-with-args 'treemacs-delete-workspace-functions to-delete)
     `(success ,to-delete ,treemacs--workspaces))))

(defun treemacs--rerender-after-workspace-change ()
  "Redraw treemacs after the current workspace was changed or deleted."
  (let* ((treemacs-buffer (treemacs-get-local-buffer))
         (in-treemacs? (eq (current-buffer) treemacs-buffer)))
    (pcase (treemacs-current-visibility)
      ('none
       (ignore))
      ('exists
       (kill-buffer treemacs-buffer)
       (save-selected-window (treemacs-select-window))
       (delete-window (treemacs-get-local-window)))
      ('visible
       (kill-buffer treemacs-buffer)
       (if in-treemacs?
           (treemacs-select-window)
         (save-selected-window (treemacs-select-window)))))))

(defun treemacs--get-path-status (path)
  "Get the status of PATH.

Returns either

* `local-readable' when PATH is a local readable file or directory,
* `local-unreadable' when PATH is a local unreadable file or directory,
* `remote-readable' when PATH is a remote readable file or directory,
* `remote-unreadable' when PATH is a remote unreadable file or directory,
* `remote-disconnected' when PATH is remote, but the connection is down, or
* `extension' when PATH is not a string."
  (declare (side-effect-free t))
  (cond
   ((not (stringp path)) 'extension)
   ((not (file-remote-p path))
    (if (file-readable-p path) 'local-readable 'local-unreadable))
   ((not (file-remote-p path nil t)) 'remote-disconnected)
   ((file-readable-p path) 'remote-readable)
   (t 'remote-unreadable)))

(define-inline treemacs-project->is-unreadable? (self)
  "Return non-nil if the project SELF is definitely unreadable.

If `path-status' of the project is `remote-disconnected', the return value will
be nil even though the path might still be unreadable.  Does not verify the
readability, the cached path-state is used.  Extension projects will count as
readable."
  (declare (side-effect-free t))
  (inline-quote (memq (treemacs-project->path-status ,self)
                      '(local-unreadable remote-unreadable))))

(define-inline treemacs-project->is-readable? (self)
  "Return t if the project SELF is definitely readable for file operations.

Does not verify the readability - the cached state is used."
  (declare (side-effect-free t))
  (inline-quote (memq (treemacs-project->path-status ,self)
                      '(local-readable remote-readable))))

(define-inline treemacs-project->is-remote? (self)
  "Return t if the project SELF is remote."
  (declare (side-effect-free t))
  (inline-letevals (self)
    (inline-quote (memq (treemacs-project->path-status ,self)
                        '(remote-disconnected remote-readable remote-unreadable)))))

(define-inline treemacs-project->is-local? (self)
  "Return t if the project SELF is local.  Returns nil for extensions."
  (declare (side-effect-free t))
  (inline-letevals (self)
    (inline-quote (memq (treemacs-project->path-status ,self)
                        '(local-readable local-unreadable)))))

(define-inline treemacs-project->is-local-and-readable? (self)
  "Return t if the project SELF is local and readable."
  (declare (side-effect-free t))
  (inline-quote (eq (treemacs-project->path-status ,self) 'local-readable)))

(defun treemacs-do-add-project-to-workspace (path name)
  "Add project at PATH to the current workspace.
NAME is provided during ad-hoc navigation only.
Return values may be as follows:

* If the given path is invalid (is nil or does not exist)
  - the symbol `invalid-path'
  - a string describing the problem
* If the project for the given path already exists:
  - the symbol `duplicate-project'
  - the project the PATH falls into
* If a project under given path already exists:
  - the symbol `includes-project'
  - the project the PATH contains
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
  (treemacs-block
   (treemacs-return-if (null path)
     `(invalid-path "Path is nil."))
   (let ((path-status (treemacs--get-path-status path))
         (added-in-workspace (treemacs-current-workspace)))
     (treemacs-return-if (not (file-readable-p path))
       `(invalid-path "Path is not readable does not exist."))
     (setq path (-> path (file-truename) (treemacs-canonical-path)))
     (-when-let (project (treemacs--find-project-for-path path))
       (treemacs-return `(duplicate-project ,project)))
     (treemacs-return-if (treemacs--is-name-invalid? name)
       `(invalid-name ,name))
     (-when-let (project (--first (treemacs-is-path (treemacs-project->path it) :in path)
                                  (treemacs-workspace->projects (treemacs-current-workspace))))
       (treemacs-return `(includes-project ,project)))
     (let ((project (treemacs-project->create! :name name :path path :path-status path-status)))
       (-when-let (double (--first (string= name (treemacs-project->name it))
                                   (treemacs-workspace->projects (treemacs-current-workspace))))
         (treemacs-return `(duplicate-name ,double)))
       (treemacs--add-project-to-current-workspace project)
       (treemacs--invalidate-buffer-project-cache)
       (treemacs-run-in-every-buffer
        (when (eq added-in-workspace workspace)
          (treemacs-with-writable-buffer
           (goto-char (treemacs--projects-end))
           (cond
            ;; Inserting the first and only button - no need to add spacing
            ((not (treemacs-current-button)))
            ;; Inserting before a button. This happens when only bottom extensions exist.
            ((bolp)
             (save-excursion (treemacs--insert-root-separator))
             ;; Unlock the marker - when the marker is at the beginning of the buffer,
             ;; expanding/collapsing extension nodes would move the marker and it was thus locked.
             (set-marker-insertion-type (treemacs--projects-end) t))
            ;; Inserting after a button (the standard case)
            ;; We should already be at EOL, but play it safe.
            (t
             (end-of-line)
             (treemacs--insert-root-separator)))
           (treemacs--add-root-element project)
           (treemacs-dom-node->insert-into-dom!
            (treemacs-dom-node->create! :key path :position (treemacs-project->position project)))
           (when treemacs-expand-added-projects
             (treemacs--expand-root-node (treemacs-project->position project))))))
       (treemacs--persist)
       (treemacs--invalidate-buffer-project-cache)
       (when (with-no-warnings treemacs-hide-gitignored-files-mode)
         (treemacs--prefetch-gitignore-cache path))
       (run-hook-with-args 'treemacs-create-project-functions project)
       `(success ,project)))))

(defalias 'treemacs-add-project-at #'treemacs-do-add-project-to-workspace)
(with-no-warnings
  (make-obsolete #'treemacs-add-project-at #'treemacs-do-add-project-to-workspace "v.2.2.1"))

(defun treemacs-do-remove-project-from-workspace
    (project &optional ignore-last-project-restriction ask-to-confirm)
  "Remove the given PROJECT from the current workspace.

PROJECT may either be a `treemacs-project' instance or a string path.  In the
latter case the project containing the path will be selected.

When IGNORE-LAST-PROJECT-RESTRICTION is non-nil removing the last project will
not count as an error.  This is meant to be used in non-interactive code, where
another project is immediately added afterwards, as leaving the project list
empty is generally a bad idea.

Ask the user to confirm the deletion when ASK-TO-CONFIRM is t (it will be when
this is called from `treemacs-remove-project-from-workspace').

Return values may be as follows:

* If the given path is invalid (is nil or does not exist):
  - the symbol `invalid-project'
  - a string describing the problem
* If the user cancels the deletion:
  - the symbol `user-cancel'
* If there is only one project:
  - the symbol `cannot-delete-last-project'
* If everything went well:
  - the symbol `success'"
  (treemacs-block
   (unless ignore-last-project-restriction
     (treemacs-return-if (>= 1 (length (treemacs-workspace->projects (treemacs-current-workspace))))
       'cannot-delete-last-project))
   (treemacs-return-if (null project)
     `(invalid-project "Project is nil"))
   ;; when used from outside treemacs it is much easier to supply a path string than to
   ;; look up the project instance
   (when (stringp project)
     (-let [found-project (treemacs-is-path (treemacs-canonical-path project) :in-workspace)]
       (treemacs-return-if (null found-project)
         `(invalid-project ,(format "Given path '%s' is not in the workspace" project)))
       (setf project found-project)))
   (treemacs-return-if
       (and ask-to-confirm
            (not (yes-or-no-p (format "Remove project %s from the current workspace?"
                                      (propertize (treemacs-project->name project)
                                                  'face 'font-lock-type-face)))))
     'user-cancel)
   (treemacs-run-in-every-buffer
    (treemacs-with-writable-buffer
     (let* ((project-pos (goto-char (treemacs-project->position project)))
            (prev-project-pos (move-marker (make-marker) (treemacs--prev-project-pos)))
            (next-project-pos (move-marker (make-marker) (treemacs--next-project-pos))))
       (when (treemacs-project->is-expanded? project)
         (treemacs--collapse-root-node project-pos t))
       (treemacs--remove-project-from-current-workspace project)
       (treemacs--invalidate-buffer-project-cache)
       (let ((previous-button (previous-button project-pos))
             (next-button (next-button project-pos)))
         (cond
          ;; Previous button exists. Delete from the end of the current line to
          ;; the end of the previous button's line. If the `treemacs--projects-end'
          ;; is at the EOL of the  it will move to EOL of the previous button.
          (previous-button
           (delete-region (treemacs-button-end previous-button) (line-end-position))
           (when next-button (forward-button 1)))
          ;; Previous project does not exist, but a next button exists. Delete from
          ;; BOL to the start of the next buttons line.
          (next-button
           (when (> next-button (treemacs--projects-end))
             ;; The first item after the deletion will be bottom extensions. Project
             ;; end will be at its BOL, making it move upon expand/collapse. Lock the marker.
             (set-marker-insertion-type (treemacs--projects-end) nil))
           (delete-region (line-beginning-position) (progn (goto-char next-button) (forward-line 0) (point))))

          ;; Neither the previous nor the next button exists. Simply delete the
          ;; current line.
          (t
           (delete-region (line-beginning-position) (line-end-position)))))
       (if (equal (point-min) prev-project-pos)
           (goto-char next-project-pos)
         (goto-char prev-project-pos)))
     (treemacs--invalidate-buffer-project-cache)
     (--when-let (treemacs-get-local-window)
       (with-selected-window it
         (recenter)))
     (treemacs--evade-image)
     (hl-line-highlight)))
   (run-hook-with-args 'treemacs-delete-project-functions project)
   (treemacs--persist)
   'success))

(defun treemacs-do-switch-workspace (&optional workspace)
  "Switch to a new WORKSPACE.
Workspace may either be a workspace name, a workspace object, or be left out.
In the latter case the workspace to switch to will be selected interactively.
Return values may be as follows:

* If there are no workspaces to switch to:
  - the symbol `only-one-workspace'
* If the given workspace could not be found (if WORKSPACE was a name string)
  - the symbol `workspace-not-found'
  - the given workspace name
* If everything went well:
  - the symbol `success'
  - the selected workspace"
  (treemacs--maybe-load-workspaces)
  (treemacs-block
   (treemacs-return-if (= 1 (length treemacs--workspaces))
     'only-one-workspace)
   (let (new-workspace)
     (cond
      ((treemacs-workspace-p workspace)
       (setf new-workspace workspace))
      ((stringp workspace)
       (setf new-workspace (treemacs-find-workspace-by-name workspace))
       (treemacs-return-if (null new-workspace)
         `(workspace-not-found ,workspace)))
      ((null workspace)
       (let* ((workspaces (->> treemacs--workspaces
                               (--reject (eq it (treemacs-current-workspace)))
                               (--map (cons (treemacs-workspace->name it) it))))
              (name (completing-read "Switch to: " workspaces nil :require-match)))
         (setf new-workspace (cdr (--first (string= (car it) name) workspaces))))))
     (setf (treemacs-current-workspace) new-workspace)
     (treemacs--invalidate-buffer-project-cache)
     (treemacs--rerender-after-workspace-change)
     (when (with-no-warnings treemacs-hide-gitignored-files-mode)
       (treemacs--prefetch-gitignore-cache 'all))
     (run-hooks 'treemacs-switch-workspace-hook)
     (treemacs-return
      `(success ,new-workspace)))))

(defun treemacs-do-rename-workspace (&optional workspace new-name)
  "Rename a workspace.

Takes either a WORKSPACE and a NEW-NAME as arguments or reads them
interactively.

Return values may be as follows:

* If the given name is invalid:
  - the symbol `invalid-name'
  - the name
* If everything went well:
  - the symbol `success'
  - the old-name
  - the renamed workspace"
  (treemacs-block
   (let ((old-name))
     (unless workspace
       (let* ((current-ws (treemacs-current-workspace))
              (old-name (treemacs-workspace->name current-ws))
              (name-map (-> (--map (cons (treemacs-workspace->name it) it)  treemacs--workspaces)
                            (sort (lambda (n _) (string= (car n) old-name)))))
              (str-to-rename (completing-read "Rename: " name-map)))
         (setf workspace (cdr (assoc str-to-rename name-map)))))
     (setf old-name (treemacs-workspace->name workspace))
     (unless new-name
       (setf new-name (treemacs--read-string "New name: ")))
     (treemacs-return-if (treemacs--is-name-invalid? new-name)
       `(invalid-name ,new-name))
     (setf (treemacs-workspace->name workspace) new-name)
     (treemacs--persist)
     (run-hook-with-args 'treemacs-rename-workspace-functions workspace old-name)
     `(success ,old-name ,workspace))))

(defun treemacs--is-name-invalid? (name)
  "Validate the NAME of a project or workspace.
Returns t when the name is invalid.

NAME: String"
  (declare (pure t) (side-effect-free t))
  (or (s-blank-str? name)
      (s-contains? "\n" name)
      (not (s-matches? (rx (1+ (or space (syntax word) (syntax symbol) (syntax punctuation)))) name))))

(define-inline treemacs-project-at-point ()
  "Get the project for the (nearest) project at point.
Return nil when `treemacs-current-button' is nil."
  (declare (side-effect-free t))
  (inline-quote
   (-when-let (btn (treemacs-current-button))
     (treemacs-project-of-node btn))))

(defun treemacs--get-bounds-of-project (project)
  "Get the bounds a PROJECT in the current buffer.
Returns a cons cell of buffer positions at the very start and end of the
PROJECT, excluding newlines.

PROJECT: Project Struct"
  (interactive)
  (save-excursion
    (goto-char (treemacs-project->position project))
    (let* ((start (line-beginning-position))
           (next  (treemacs--next-non-child-button (treemacs-project->position project)))
           (end   (if next
                      (-> next (treemacs-button-start) (previous-button) (treemacs-button-end))
                    ;; final position minus the final newline
                    (1- (point-max)))))
      (cons start end))))

(defun treemacs--consolidate-projects ()
  "Correct treemacs buffers' content after the workspace was edited."
  (treemacs--invalidate-buffer-project-cache)
  (treemacs-run-in-every-buffer
   (let* ((current-file (--when-let (treemacs-current-button) (treemacs--nearest-path it)))
          (current-workspace (treemacs-current-workspace))
          ;; gather both the projects actually in the workspace ...
          (projects-in-workspace (treemacs-workspace->projects current-workspace))
          (projects-in-buffer)
          (expanded-projects-in-buffer))
     (goto-char 0)
     ;; ... as well as the projects currently shown in the buffer
     (unless (s-blank? (buffer-string))
       (push (treemacs-project-at-point) projects-in-buffer)
       (let (next-pos)
         (while (/= (point-max)
                    (setq next-pos (treemacs--next-project-pos)))
           (goto-char next-pos)
           (unless (treemacs-button-get (treemacs-current-button) :custom)
             (push (treemacs-project-at-point) projects-in-buffer)))))
     ;; remember which ones are expanded, close them so the dom position can be rebuilt
     (dolist (project-in-buffer projects-in-buffer)
       (-let [project-btn (treemacs-project->position project-in-buffer)]
         (when (eq 'root-node-open (treemacs-button-get project-btn :state))
           (push project-in-buffer expanded-projects-in-buffer)
           (goto-char project-btn)
           (treemacs--collapse-root-node project-btn))))
     ;; figure out which ones have been deleted and and remove them from the dom
     (dolist (project-in-buffer projects-in-buffer)
       (unless (member project-in-buffer projects-in-workspace)
         (treemacs-on-collapse (treemacs-project->path project-in-buffer) :purge)
         (ht-remove! treemacs-dom (treemacs-project->path project-in-buffer))
         (setf projects-in-buffer (delete project-in-buffer projects-in-buffer))))
     (treemacs-with-writable-buffer
      (treemacs--reset-dom)
      ;; delete everything's that's visible and render it again - the order of projects could
      ;; have been changed
      (erase-buffer)
      (treemacs--render-projects projects-in-workspace)
      (goto-char 0)
      ;; re-expand the projects that were expanded before the consolidation
      (let (next-pos)
        (-let [btn (treemacs-current-button)]
          (when (member (treemacs-button-get btn :project) expanded-projects-in-buffer)
            (treemacs--expand-root-node btn)))
        (while (/= (point-max)
                   (setq next-pos (treemacs--next-project-pos)))
          (goto-char next-pos)
          (-let [btn (treemacs-current-button)]
            (when (member (treemacs-button-get btn :project) expanded-projects-in-buffer)
              (treemacs--expand-root-node btn))))))
     ;; go back to the previous position
     (if (and current-file
              (treemacs-is-path current-file :in-workspace))
         (treemacs-goto-file-node current-file)
       (goto-char 0)
       (treemacs--evade-image))
     (hl-line-highlight))))

(defun treemacs--org-edit-display-validation-msg (message line)
  "Display an inline validation MESSAGE in LINE when org-editing."
  (save-excursion
    (pcase line
      (:start
       (goto-char 0)
       (forward-line (if treemacs-show-edit-workspace-help 4 2)))
      (_
       (goto-char 0)
       (search-forward-regexp (rx-to-string `(seq bol ,line eol)))))
    (setf treemacs--org-err-ov (make-overlay (line-end-position) (line-end-position)))
    (overlay-put treemacs--org-err-ov 'after-string
                 (concat (propertize " ← " 'face 'error) message))
    (add-hook 'after-change-functions #'treemacs--org-edit-remove-validation-msg nil :local)))

(defun treemacs--org-edit-remove-validation-msg (&rest _)
  "Remove the validation message overlay."
  (when (and treemacs--org-err-ov
             (overlayp treemacs--org-err-ov))
    (delete-overlay treemacs--org-err-ov))
  (remove-hook 'after-change-functions #'treemacs--org-edit-remove-validation-msg :local))

(defun treemacs--find-current-user-project ()
  "Find current project by calling `treemacs--find-user-project-functions'."
  (declare (side-effect-free t))
  (treemacs-block
   (dolist (fun treemacs--find-user-project-functions)
     (--when-let (funcall fun)
       (treemacs-return it)))))

(defun treemacs--find-workspace-by-name (name)
  "Find a workspace with the given NAME.
Returns nil when there is no match."
  (treemacs--maybe-load-workspaces)
  (--first (string= name (treemacs-workspace->name it))
           treemacs--workspaces))

(defun treemacs--select-workspace-by-name ()
  "Interactively select the workspace.
Selection is based on the list of names of all workspaces and still happens
when there is only one workspace."
  (treemacs--maybe-load-workspaces)
  (let (name)
    (while (or (null name) (string= "" name))
      (setf name (completing-read
                  "Workspace: "
                  (->> treemacs--workspaces
                       (--map (cons (treemacs-workspace->name it) it)))
                  nil :require-match)))
    (--first (string= name (treemacs-workspace->name it))
             treemacs--workspaces)))

(defun treemacs--maybe-clean-buffers-on-workspace-switch (which)
  "Delete buffers depending on the value of WHICH.

 - When it is nil do nothing.
 - When it is `files' delete all buffers visiting files.
 - When it is `all' delete all buffers

In any case treemacs itself, and the scratch and messages buffers will be left
alive."
  (when which
    (let* ((scratch (get-buffer-create "*scratch*"))
           (messages (get-buffer "*Messages*"))
           (no-delete-test
            (pcase which
              ('files (lambda (b) (null (buffer-file-name b))))
              ('all (lambda (_) nil)))))
      (dolist (buffer (buffer-list))
        (unless (or (eq t (buffer-local-value 'treemacs--in-this-buffer buffer))
                    (eq buffer scratch)
                    (eq buffer messages)
                    (funcall no-delete-test buffer))
          (kill-buffer buffer))))))

(defun treemacs-find-workspace-by-name (name)
  "Find a workspace with the given NAME.
The check is case-sensitive.  nil is returned when no workspace is found."
  (declare (side-effect-free t))
  (--first (string= name (treemacs-workspace->name it))
           treemacs--workspaces))

(defun treemacs-find-workspace-by-path (path)
  "Find a workspace with a project containing the given PATH.
nil is returned when no workspace is found."
  (declare (side-effect-free t))
  (--first (treemacs-is-path path :in-workspace it)
           treemacs--workspaces))

(defun treemacs-find-workspace-where (predicate)
  "Find a workspace matching the given PREDICATE.
Predicate should be a function that takes a `treemacs-workspace' as its single
argument.  nil is returned when no workspace is found."
  (--first (funcall predicate it) treemacs--workspaces))

(provide 'treemacs-workspaces)

;;; treemacs-workspaces.el ends here
