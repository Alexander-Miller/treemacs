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
;;; Follow mode definition.

;;; Code:

(require 'hl-line)
(require 'dash)
(require 's)
(require 'f)
(require 'treemacs-customization)
(require 'treemacs-rendering)
(require 'treemacs-dom)
(require 'treemacs-async)
(require 'treemacs-core-utils)

(eval-when-compile
  (require 'treemacs-macros))

(treemacs-import-functions-from "dired"
  dired-current-directory)

(defvar treemacs--ready-to-follow nil
  "Signals to `treemacs-follow-mode' if a follow action may be run.
Must be set to nil when no following should be triggered, e.g. when the
treemacs buffer is being rebuilt or during treemacs' own window selection
functions.")

(defvar treemacs--follow-timer nil
  "Idle timer for `treemacs--follow' to run.")

(defun treemacs--follow ()
  "Move point to the current file in the treemacs buffer.
Expand directories if needed.  Do nothing if current file does not exist in the
file system or is not below current treemacs root or if the treemacs buffer is
not visible."
  ;; Treemacs selecting files with `ace-window' results in a large amount of
  ;; window selections, so we should be breaking out as soon as possbile
  (setq treemacs--follow-timer nil)
  (when treemacs--ready-to-follow
    (treemacs-without-following
     (let* ((treemacs-window (treemacs-get-local-window))
            (current-buffer  (current-buffer))
            (current-file    (or (buffer-file-name current-buffer)
                                 (when (eq major-mode 'dired-mode)
                                   (treemacs--canonical-path (dired-current-directory))))))
       (when (and treemacs-window
                  current-file
                  (not (s-starts-with? treemacs--buffer-name-prefix (buffer-name current-buffer)))
                  (f-exists? current-file))
         (-when-let (project-for-file (treemacs--find-project-for-buffer current-file))
           (with-selected-window treemacs-window
             (-let [selected-file (--if-let (treemacs-current-button)
                                      (treemacs--nearest-path it)
                                    (treemacs-project->path project-for-file))]
               (unless (treemacs-is-path selected-file :same-as current-file)
                 (when (treemacs-goto-file-node current-file project-for-file)
                   (when treemacs-project-follow-cleanup
                     (dolist (project (treemacs-workspace->projects (treemacs-current-workspace)))
                       (unless (or (not (treemacs-project->is-expanded? project))
                                   (eq project project-for-file))
                         (-when-let (project-pos (treemacs-project->position project))
                           (goto-char project-pos)
                           (treemacs--collapse-root-node project-pos)))))
                   (when treemacs-recenter-after-file-follow
                     (treemacs--maybe-recenter treemacs-recenter-after-file-follow))))))))))))

(defun treemacs--follow-after-buffer-list-update ()
  "Debounced call to `treemacs--follow'."
  (when treemacs--ready-to-follow
    (unless treemacs--follow-timer
      (setq treemacs--follow-timer
            (run-with-idle-timer treemacs-file-follow-delay nil #'treemacs--follow)))))

(defun treemacs--setup-follow-mode ()
  "Setup all the hooks needed for `treemacs-follow-mode'."
  (add-hook 'buffer-list-update-hook #'treemacs--follow-after-buffer-list-update)
  (treemacs--follow))

(defun treemacs--tear-down-follow-mode ()
  "Remove the hooks added by `treemacs--setup-follow-mode'."
  (remove-hook 'buffer-list-update-hook #'treemacs--follow-after-buffer-list-update))

(define-minor-mode treemacs-follow-mode
  "Toggle `treemacs-follow-mode'.
When enabled treemacs will keep track of and focus the currently selected
buffer's file. This only applies if the file is within the treemacs root
directory.
This functionality can also be manually invoked with `treemacs-find-file'."
  :init-value nil
  :global     t
  :lighter    nil
  (if treemacs-follow-mode
      (treemacs--setup-follow-mode)
    (treemacs--tear-down-follow-mode)))

(treemacs-only-during-init (treemacs-follow-mode))

(provide 'treemacs-follow-mode)

;;; treemacs-follow-mode.el ends here
