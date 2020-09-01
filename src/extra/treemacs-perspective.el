;;; treemacs-perspective.el --- Perspective integration for treemacs -*- lexical-binding: t -*-

;; Copyright (C) 2020 Alexander Miller

;; Author: Alexander Miller <alexanderm@web.de>
;;   Jason Dufair <jase@dufair.org>
;; Package-Requires: ((emacs "25.2") (treemacs "0.0") (perspective "2.8") (dash "2.11.0"))
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
;;; Integration of perspective.el into treemacs' buffer scoping framework.

;;; Code:

(require 'treemacs)
(require 'perspective)
(require 'eieio)
(require 'dash)

(eval-when-compile
  (require 'treemacs-macros)
  (require 'cl-lib))

;; remove base compatibility hook
(remove-hook 'perspective-activated-functions #'treemacs--remove-treemacs-window-in-new-frames)

(defclass treemacs-perspective-scope (treemacs-scope) () :abstract t)
(add-to-list 'treemacs-scope-types (cons 'Perspectives 'treemacs-perspective-scope))

(cl-defmethod treemacs-scope->current-scope ((_ (subclass treemacs-perspective-scope)))
  "Get the current perspective as scope.
Returns the symbol `none' if no perspective is active."
  (or (persp-curr) 'none))

(cl-defmethod treemacs-scope->current-scope-name ((_ (subclass treemacs-perspective-scope)) perspective)
  "Return the name of the given PERSPECTIVE.
Will return \"No Perspective\" if no perspective is active."
  (if (eq 'none perspective)
      "No Perspective"
    (format "Perspective %s" (persp-name perspective))))

(cl-defmethod treemacs-scope->setup ((_ (subclass treemacs-perspective-scope)))
  "Perspective-scope setup."
  (add-hook 'persp-switch-hook #'treemacs-perspective--on-perspective-switch)
  (add-hook 'persp-killed-hook #'treemacs--on-scope-kill)
  (treemacs-perspective--ensure-workspace-exists))

(cl-defmethod treemacs-scope->cleanup ((_ (subclass treemacs-perspective-scope)))
  "Perspective-scope tear-down."
  (remove-hook 'persp-switch-hook #'treemacs-perspective--on-perspective-switch)
  (remove-hook 'persp-killed-hook #'treemacs--on-scope-kill))

(defun treemacs-perspective--on-perspective-switch (&rest _)
  "Hook running after the perspective was switched.
Will select a workspace for the now active perspective, creating it if necessary."
  ;; runnig with a timer ensures that any other post-processing is finished after a perspective
  ;; was run since commands like `spacemacs/helm-persp-switch-project' first create a perspective
  ;; and only afterwards select the file to display
  (run-with-timer
   0.1 nil
   (lambda ()
     (treemacs-without-following
      (treemacs-perspective--ensure-workspace-exists)
      (treemacs--change-buffer-on-scope-change)))))

(defun treemacs-perspective--ensure-workspace-exists ()
  "Make sure a workspace exists for the given PERSPECTIVE-NAME.
Matching happens by name.  If no workspace can be found it will be created."
  (let* ((perspective-name (treemacs-scope->current-scope-name
                      (treemacs-current-scope-type) (treemacs-current-scope)))
         (workspace (or (treemacs--select-workspace-by-name perspective-name)
                        (treemacs-perspective--create-workspace perspective-name))))
    (setf (treemacs-current-workspace) workspace)
    (treemacs--invalidate-buffer-project-cache)
    (run-hooks 'treemacs-switch-workspace-hook)
    workspace))

(defun treemacs-perspective--create-workspace (name)
  "Create a new workspace for the given perspective NAME.
Projects will be found as per `treemacs--find-user-project-functions'.  If that
does not return anything the projects of the fallback workspace will be copied."
  (treemacs-block
   (let* ((ws-result (treemacs-do-create-workspace name))
          (ws-status (car ws-result))
          (ws (cadr ws-result))
          (root-path (treemacs--find-current-user-project))
          (project-list))
     (unless (eq ws-status 'success)
       (treemacs-log "Failed to create workspace for perspective: %s, using fallback instead." ws)
       (treemacs-return (car treemacs--workspaces)))
     (if root-path
         (setf project-list
               (list (treemacs-project->create!
                      :name (treemacs--filename root-path)
                      :path root-path
                      :path-status (treemacs--get-path-status root-path))))
       (-let [fallback-workspace (car treemacs--workspaces)]
         ;; copy the projects instead of reusing them so we don't accidentially rename
         ;; a project in 2 workspaces
         (dolist (project (treemacs-workspace->projects fallback-workspace))
           (push (treemacs-project->create!
                  :name (treemacs-project->name project)
                  :path (treemacs-project->path project)
                  :path-status (treemacs-project->path-status project))
                 project-list))))
     (setf (treemacs-workspace->projects ws) project-list)
     (treemacs-return ws))))

(provide 'treemacs-perspective)

;;; treemacs-perspective.el ends here
