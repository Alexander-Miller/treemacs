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
;;; Follow mode definition.

;;; Code:

(require 'hl-line)
(require 'dash)
(require 's)
(require 'f)
(require 'treemacs-customization)
(require 'treemacs-branch-creation)
(require 'treemacs-structure)
(require 'treemacs-async)
(require 'treemacs-impl)
(eval-and-compile
  (require 'cl-lib)
  (require 'treemacs-macros))

(defvar treemacs--ready-to-follow nil
  "Signals to `treemacs-follow-mode' if a follow action may be run.
Must be set to nil when no following should be triggered, e.g. when the
treemacs buffer is being rebuilt or during treemacs' own window selection
functions.")

(defun treemacs--follow ()
  "Move point to the current file in the treemacs buffer.
Expand directories if needed. Do nothing if current file does not exist in the
file system or is not below current treemacs root or if the treemacs buffer is
not visible."
  ;; Treemacs selecting files with `ace-window' results in a large amount of
  ;; window selections, so we should be breaking out as soon as possbile
  (when treemacs--ready-to-follow
    (treemacs-without-following
     (let* ((treemacs-window (treemacs--is-visible?))
            (current-buffer  (current-buffer))
            (current-file    (buffer-file-name current-buffer)))
       (when (and treemacs-window
                  current-file
                  (not (s-starts-with? treemacs--buffer-name-prefix (buffer-name current-buffer)))
                  (f-exists? current-file))
         (-when-let- [project-for-file (treemacs--find-project-for-buffer)]
           (with-current-buffer (window-buffer treemacs-window)
             (-let [selected-file (--if-let (treemacs-current-button)
                                      (treemacs--nearest-path it)
                                    (treemacs-project->path project-for-file))]
               (unless (string= selected-file current-file)
                 (when (treemacs-goto-button current-file project-for-file)
                   (when treemacs-project-follow-cleanup
                     (dolist (project (treemacs-workspace->projects (treemacs-current-workspace)))
                       (unless (or (not (treemacs-project->is-expanded? project))
                                   (eq project project-for-file))
                         (-when-let- [project-pos (treemacs-project->position project)]
                           (goto-char project-pos)
                           (treemacs--collapse-root-node project-pos)))))
                   (when treemacs-recenter-after-file-follow
                     (treemacs--maybe-recenter))))))))))))

;; this is only to stop the compiler from complaining about unknown functions
(with-eval-after-load 'which-key
  (declare-function which-key--show-popup "which-key")
  (declare-function which-key--hide-popup "which-key"))

(defun treemacs--select-window-advice (&rest _)
  "Advice function for `treemacs-follow-mode'.
Ignores the original arguments of `select-window' and directly calls
`treemacs--follow'."
  (treemacs--follow))

(defun treemacs--follow-compatibility-advice (original-func &rest args)
  "Make ORIGINAL-FUNC compatible with `treemacs-follow-mode'.
Do so by running it and its ARGS through `treemacs-without-following'."
  (treemacs-without-following
   (apply original-func args)))

(defsubst treemacs--setup-follow-mode ()
  "Setup all the advice needed for `treemacs-follow-mode'."
  (advice-add 'select-window :after #'treemacs--select-window-advice)
  ;; which key compatibility
  (progn
    (when (fboundp 'which-key--show-popup)
      (advice-add #'which-key--show-popup :around #'treemacs--follow-compatibility-advice))
    (when (fboundp 'which-key--hide-popup)
      (advice-add #'which-key--hide-popup :around #'treemacs--follow-compatibility-advice)))
  ;; winum compatibility
  (when (fboundp 'winum--update)
    (advice-add #'winum--update :around #'treemacs--follow-compatibility-advice))
  (treemacs--follow))

(defsubst treemacs--tear-down-follow-mode ()
  "Remove all the advice added by `treemacs--setup-follow-mode'."
  (advice-remove 'select-window 'treemacs--select-window-advice)
  ;; which key compatibility
  (progn
    (when (advice-member-p #'treemacs--follow-compatibility-advice #'which-key--show-popup)
      (advice-remove #'which-key--show-popup #'treemacs--follow-compatibility-advice))
    (when (advice-member-p #'treemacs--follow-compatibility-advice #'which-key--hide-popup)
      (advice-remove #'which-key--hide-popup #'treemacs--follow-compatibility-advice)))
  ;; winum compatibility
  (when (and (fboundp 'winum--update)
             (advice-member-p #'treemacs--follow-compatibility-advice #'winum--update))
    (advice-remove #'winum--update #'treemacs--follow-compatibility-advice)))

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

(only-during-treemacs-init (treemacs-follow-mode))

(provide 'treemacs-follow-mode)

;;; treemacs-follow-mode.el ends here
