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

;; Variations of header-line-format treemacs can use.

;;; Code:

(require 'dash)
(require 'treemacs-faces)
(require 'treemacs-interface)

(eval-when-compile
  (require 'treemacs-macros)
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
               `(["Edit Workspaces"        treemacs-edit-workspaces]
                 ["Create Workspace"       treemacs-create-workspace]
                 ["Remove Workspace"       treemacs-remove-workspace]
                 ["Rename Workspace"       treemacs-rename-workspace]
                 ["Switch Workspace"       treemacs-switch-workspace]
                 ["Set Fallback Workspace" treemacs-set-fallback-workspace])))
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
Consists for 4 different buttons:
- `treemacs-header-close-button'
- `treemacs-header-projects-button'
- `treemacs-header-workspace-button'
- `treemacs-header-toggles-button'")

(defun treemacs--header-top-scroll-indicator ()
  "Determine header line for `treemacs-indicate-top-scroll-mode'."
  (if (= (window-start) (point-min))
      (car treemacs-header-scroll-indicators)
    (cdr treemacs-header-scroll-indicators)))

;;;###autoload
(define-minor-mode treemacs-indicate-top-scroll-mode
  "Minor mode which shows whether treemacs is scrolled all the way to the top.

When this mode is enabled the header line of the treemacs window will display
whether the window's first line is visible or not.

The strings used for the display are determined by
`treemacs-header-scroll-indicators'.

This mode makes use of `treemacs-user-header-line-format' - and thus
`header-line-format' - and is therefore incompatible with other modifications to
these options."
  :init-value nil
  :global t
  :group 'treemacs
  (setf treemacs-user-header-line-format
        (when treemacs-indicate-top-scroll-mode
          '("%e" (:eval (treemacs--header-top-scroll-indicator)))))
  (treemacs-run-in-every-buffer
   (setf header-line-format treemacs-user-header-line-format)))

(provide 'treemacs-header-line)

;;; treemacs-header-line.el ends here
