;;; treemacs.el --- A tree style file viewer package -*- lexical-binding: t -*-

;; Copyright (C) 2020 Alexander Miller

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
;;; Varations of header-line-format treemacs can use.

;;; Code:

(require 'dash)
(require 'treemacs-faces)
(require 'treemacs-interface)

(eval-when-compile
  (require 'cl-lib))

(cl-macrolet
    ((make-local-map
      (&rest body)
      `(-doto (make-sparse-keymap)
         (define-key [header-line mouse-1]
           (lambda (event)
             (interactive "e")
             ,@body)))))

  (defconst treemacs-header-close-button
    (propertize
     "(❌)"
     'local-map (make-local-map (delete-window (posn-window (event-start event))))
     'face 'treemacs-header-button-face)
    "Header button to close the treemacs window.")

  (defconst treemacs-header-projects-button
    (propertize
     "(P)"
     'local-map
     (make-local-map
      (let* ((menu
              (easy-menu-create-menu
               nil
               `(["Add Project"            treemacs-add-project]
                 ["Add Projectile Project" treemacs-projectile :visible (featurep 'treemacs-projectile)]
                 ["Remove Project"         treemacs-remove-project-from-workspace])))
             (choice (x-popup-menu event menu)))
        (when choice (call-interactively (lookup-key menu (apply 'vector choice))))))
     'face 'treemacs-header-button-face)
    "Header button to open a project administration menu.")

  (defconst treemacs-header-workspace-button
    (propertize
     "(W)"
     'local-map
     (make-local-map
      (let* ((menu
              (easy-menu-create-menu
               nil
               `(["Edit Workspaces"       treemacs-edit-workspaces]
                 ["Create Workspace"      treemacs-create-workspace]
                 ["Remove Worspace"       treemacs-remove-workspace]
                 ["Rename Workspace"      treemacs-rename-workspace]
                 ["Switch Worspaces"      treemacs-switch-workspace]
                 ["Set Fallback Worspace" treemacs-set-fallback-workspace])))
             (choice (x-popup-menu event menu)))
        (when choice (call-interactively (lookup-key menu (apply 'vector choice))))) )
     'face 'treemacs-header-button-face)
    "Header button to open a workspace administration menu.")

  (defconst treemacs-header-toggles-button
    (propertize
     "(T)"
     'local-map
     (make-local-map
      (let* ((menu
              (easy-menu-create-menu
               nil
               `([,(format "Dotfile Visibility (Currently %s)"
                           (if treemacs-show-hidden-files "Enabled" "Disabled"))
                  treemacs-toggle-show-dotfiles]
                 [,(format "Follow-Mode (Currently %s)"
                           (if treemacs-follow-mode "Enabled" "Disabled"))
                  treemacs-follow-mode]
                 [,(format "Filewatch-Mode (Currently %s)"
                           (if treemacs-filewatch-mode "Enabled" "Disabled"))
                  treemacs-filewatch-mode]
                 [,(format "Fringe-Indicator-Mode (Currently %s)"
                           (if treemacs-fringe-indicator-mode "Enabled" "Disabled"))
                  treemacs-fringe-indicator-mode])))
             (choice (x-popup-menu event menu)))
        (when choice (call-interactively (lookup-key menu (apply 'vector choice))))) )
     'face 'treemacs-header-button-face)
    "Header button to open a minor-modes/toggles administration menu."))

(defconst treemacs-header-buttons-format
  (concat " " treemacs-header-close-button
          " " treemacs-header-projects-button
          " " treemacs-header-workspace-button
          " " treemacs-header-toggles-button)
  "Possible value setting for `treemacs-header-line-format'.
Conisits for 4 different buttons:
- `treemacs-header-close-button'
- `treemacs-header-projects-button'
- `treemacs-header-workspace-button'
- `treemacs-header-toggles-button'")

(provide 'treemacs-header-line)

;;; treemacs-header-line.el ends here
