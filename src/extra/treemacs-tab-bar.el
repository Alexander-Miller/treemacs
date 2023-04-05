;;; treemacs-tab-bar.el --- Tab bar integration for treemacs -*- lexical-binding: t -*-

;; Copyright (C) 2023 Alexander Miller

;; Author: Alexander Miller <alexanderm@web.de>
;;   Jason Dufair <jase@dufair.org>
;;   Aaron Jensen <aaronjensen@gmail.com>
;; Package-Requires: ((emacs "27.1") (treemacs "0.0") (dash "2.11.0"))
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
;;; Integration of tab-bar-mode into treemacs' buffer scoping framework.

;;; Code:

(require 'dash)
(require 'tab-bar)
(require 'treemacs)

(eval-when-compile
  (require 'treemacs-macros)
  (require 'cl-lib))

(defclass treemacs-tab-bar-scope (treemacs-scope) () :abstract t)
(add-to-list 'treemacs-scope-types (cons 'Tabs 'treemacs-tab-bar-scope))

(cl-defmethod treemacs-scope->current-scope ((_ (subclass treemacs-tab-bar-scope)))
  "Get the current tab as scope.
Return symbol `none' unless variable `tab-bar-mode' is non-nil."
(if tab-bar-mode
      (cdr (assq 'name (tab-bar-get-buffer-tab nil)))
    'none))

(cl-defmethod treemacs-scope->current-scope-name ((_ (subclass treemacs-tab-bar-scope)) tab)
  "Return the name of the given TAB.
Will return \"No Tab\" if no tab is active."
  (if (eq 'none tab)
      "No Tab"
    (format "Tab %s" tab)))

(cl-defmethod treemacs-scope->setup ((_ (subclass treemacs-tab-bar-scope)))
  "Tabs-scope setup."
  (when (fboundp 'tab-bar-tabs-set)
    (advice-add #'tab-bar-tabs-set :after #'treemacs-tab-bar--on-tab-switch))
  (advice-add #'tab-bar-select-tab :after #'treemacs-tab-bar--on-tab-switch)
  (add-to-list 'tab-bar-tab-post-open-functions #'treemacs-tab-bar--on-tab-switch)
  (add-to-list 'tab-bar-tab-pre-close-functions #'treemacs-tab-bar--on-tab-close)
  (treemacs-tab-bar--ensure-workspace-exists))

(cl-defmethod treemacs-scope->cleanup ((_ (subclass treemacs-tab-bar-scope)))
  "Tabs-scope tear-down."
  (when (fboundp 'tab-bar-tabs-set)
    (advice-remove #'tab-bar-tabs-set #'treemacs-tab-bar--on-tab-switch))
  (advice-remove #'tab-bar-select-tab #'treemacs-tab-bar--on-tab-switch)
  (setq tab-bar-tab-post-open-functions
        (delete #'treemacs-tab-bar--on-tab-switch tab-bar-tab-post-open-functions))
  (setq tab-bar-tab-pre-close-functions
        (delete #'treemacs-tab-bar--on-tab-close tab-bar-tab-pre-close-functions)))

(defun treemacs-tab-bar--on-tab-close (tab &rest _)
  "Cleanup hook to run when a TAB is closed."
  (treemacs--on-scope-kill (cdr (assq 'name tab))))

(defun treemacs-tab-bar--on-tab-switch (&rest _)
  "Hook running after the tab was switched.
Will select a workspace for the now active tab, creating it if
necessary."
  (treemacs-without-following
   (treemacs-tab-bar--ensure-workspace-exists)
   (treemacs--change-buffer-on-scope-change)))

(defun treemacs-tab-bar--ensure-workspace-exists ()
  "Make sure a workspace exists for the given TAB-NAME.
Matching happens by name.  If no workspace can be found it will be created."
  (let* ((tab-name (treemacs-scope->current-scope-name
                      (treemacs-current-scope-type) (treemacs-current-scope)))
         (workspace (or (treemacs--find-workspace-by-name tab-name)
                        (treemacs-tab-bar--create-workspace tab-name))))
    (setf (treemacs-current-workspace) workspace)
    (treemacs--invalidate-buffer-project-cache)
    (run-hooks 'treemacs-switch-workspace-hook)
    workspace))

(defun treemacs-tab-bar--create-workspace (name)
  "Create a new workspace for the given tab NAME.
Projects will be found as per `treemacs--find-user-project-functions'.  If that
does not return anything the projects of the fallback workspace will be copied."
  (treemacs-block
   (let* ((ws-result (treemacs-do-create-workspace name))
          (ws-status (car ws-result))
          (ws (cadr ws-result))
          (root-path (treemacs--find-current-user-project))
          (project-list))
     (unless (eq ws-status 'success)
       (treemacs-log "Failed to create workspace for tab: %s, using fallback instead." ws)
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
     (setf (treemacs-workspace->projects ws) (nreverse project-list))
     (treemacs-return ws))))

(provide 'treemacs-tab-bar)

;;; treemacs-tab-bar.el ends here
