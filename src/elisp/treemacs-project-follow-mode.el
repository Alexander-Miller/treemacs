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

;; Minor mode to automatically display just the current project.

;; NOTE: This module is lazy-loaded.

;;; Code:

(require 'treemacs-scope)
(require 'treemacs-follow-mode)
(require 'treemacs-core-utils)
(eval-when-compile
  (require 'treemacs-macros))

(defvar treemacs--project-follow-timer nil
  "Idle timer for `treemacs-project-follow-mode'.")

(defconst treemacs--project-follow-delay 1.5
  "Delay in seconds for `treemacs-project-follow-mode'.")

(defun treemacs--follow-project (_)
  "Debounced display of the current project for `treemacs-project-follow-mode'.
Used as a hook for `window-buffer-change-functions', thus the ignored parameter."
  (treemacs-debounce treemacs--project-follow-timer treemacs--project-follow-delay
    (treemacs--do-follow-project)))

(defun treemacs--do-follow-project()
  "Actual, un-debounced, implementation of project following."
  (-when-let (window (treemacs-get-local-window))
    (treemacs-block
      (let* ((ws (treemacs-current-workspace))
              (new-project-path (treemacs--find-current-user-project))
              (old-project-path (-some-> ws
                                  (treemacs-workspace->projects)
                                  (car)
                                  (treemacs-project->path))))
        (treemacs-return-if
          (or treemacs--in-this-buffer
            (null new-project-path)
            (and
              (null treemacs-project-follow-into-home)
              (string=
                (expand-file-name "~")
                new-project-path))
            (bound-and-true-p edebug-mode)
            (frame-parent)
            (and (= 1 (length (treemacs-workspace->projects ws)))
              (string= new-project-path old-project-path))))
        (save-selected-window
          (treemacs--show-single-project
            new-project-path (treemacs--filename new-project-path))
          (treemacs--follow)
          (hl-line-highlight))))))

(defun treemacs--follow-project-after-buffer-init ()
  "Hook to follow the current project when a treemacs buffer is created.
Used for `treemacs-post-buffer-init-hook', so it will run inside the treemacs
window."
  (with-selected-window (next-window (selected-window))
    (treemacs--do-follow-project)))

(defun treemacs--setup-project-follow-mode ()
  "Setup all the hooks needed for `treemacs-project-follow-mode'."
  (when treemacs--project-follow-timer (cancel-timer treemacs--project-follow-timer))
  (setf treemacs--project-follow-timer nil)
  (add-hook 'window-buffer-change-functions #'treemacs--follow-project)
  (add-hook 'window-selection-change-functions #'treemacs--follow-project)
  (add-hook 'treemacs-post-buffer-init-hook #'treemacs--follow-project-after-buffer-init))

(defun treemacs--tear-down-project-follow-mode ()
  "Remove the hooks added by `treemacs--setup-project-follow-mode'."
  (cancel-timer treemacs--project-follow-timer)
  (remove-hook 'window-buffer-change-functions #'treemacs--follow-project)
  (remove-hook 'window-selection-change-functions #'treemacs--follow-project)
  (remove-hook 'treemacs-post-buffer-init-hook #'treemacs--follow-project-after-buffer-init))

;;;###autoload
(define-minor-mode treemacs-project-follow-mode
  "Toggle `treemacs-only-current-project-mode'.

This is a minor mode meant for those who do not care about treemacs' workspace
features, or its preference to work with multiple projects simultaneously.  When
enabled it will function as an automated version of
`treemacs-display-current-project-exclusively', making sure that, after a small
idle delay, the current project, and *only* the current project, is displayed in
treemacs.

The project detection is based on the current buffer, and will try to determine
the project using the following methods, in the order they are listed:

- the current projectile.el project, if `treemacs-projectile' is installed
- the current project.el project
- the current `default-directory'

The update will only happen when treemacs is in the foreground, meaning a
treemacs window must exist in the current scope.

This mode requires at least Emacs version 27 since it relies on
`window-buffer-change-functions' and `window-selection-change-functions'."
  :init-value nil
  :global     t
  :lighter    nil
  :group      'treemacs
  (if treemacs-project-follow-mode
      (progn
        (unless (and (boundp 'window-buffer-change-functions)
                     (boundp 'window-selection-change-functions))
          (user-error "%s %s"
                      "Project-Follow-Mode is only available in Emacs"
                      "versions that support `window-buffer-change-functions'"))
        (treemacs--setup-project-follow-mode))
    (treemacs--tear-down-project-follow-mode)))

(provide 'treemacs-project-follow-mode)

;;; treemacs-project-follow-mode.el ends here
