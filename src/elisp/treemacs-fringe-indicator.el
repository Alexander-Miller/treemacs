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

;; Handling of visuals in general and icons in particular.

;;; Code:

(require 'dash)
(require 'treemacs-core-utils)
(require 'treemacs-scope)
(require 'treemacs-customization)

(eval-when-compile
  (require 'inline)
  (require 'treemacs-macros))

(defvar-local treemacs--fringe-indicator-overlay nil)

(defconst treemacs--fringe-overlay-before-string
  (propertize
   " " 'display
   `(left-fringe ,treemacs--fringe-indicator-bitmap treemacs-fringe-indicator-face))
  "The `before-string' property value used by the fringe indicator overlay.")

(defun treemacs--move-fringe-indicator-to-point ()
  "Move the fringe indicator to the position of point."
  (when treemacs--fringe-indicator-overlay
    (-let [pabol (line-beginning-position)]
      (move-overlay treemacs--fringe-indicator-overlay pabol (1+ pabol)))))

(defun treemacs--enable-fringe-indicator ()
  "Enabled the fringe indicator in the current buffer."
  (unless treemacs--fringe-indicator-overlay
    (setq-local
     treemacs--fringe-indicator-overlay
     (-doto (make-overlay 1 1 (current-buffer))
       (overlay-put 'before-string treemacs--fringe-overlay-before-string)))
    (treemacs--move-fringe-indicator-to-point)))

(defun treemacs--disable-fringe-indicator ()
  "Enabled the fringe indicator in the current buffer."
  (when treemacs--fringe-indicator-overlay
    (delete-overlay treemacs--fringe-indicator-overlay)
    (setf treemacs--fringe-indicator-overlay nil)))

(defun treemacs--show-fringe-indicator-only-when-focused (window)
  "Hook to ensure the fringe indicator not shown when treemacs is not selected.
WINDOW is the treemacs window that has just been focused or unfocused."
  (if (eq treemacs--in-this-buffer t)
      (when treemacs--fringe-indicator-overlay
        (overlay-put
         treemacs--fringe-indicator-overlay 'before-string
         treemacs--fringe-overlay-before-string))
    (with-selected-window window
      (when treemacs--fringe-indicator-overlay
        (overlay-put
         treemacs--fringe-indicator-overlay
         'before-string nil)))))

(defun treemacs--tear-down-fringe-indicator-mode ()
  "Tear down `treemacs-fringe-indicator-mode'."
  (remove-hook 'treemacs-mode-hook
               #'treemacs--enable-fringe-indicator-in-current-buffer)
  (treemacs-run-in-all-derived-buffers
   (treemacs--disable-fringe-indicator)
   (advice-remove #'hl-line-highlight #'treemacs--move-fringe-indicator-to-point)
   (remove-hook 'window-selection-change-functions
                #'treemacs--show-fringe-indicator-only-when-focused
                :local)))

(define-minor-mode treemacs-fringe-indicator-mode
  "Toggle `treemacs-fringe-indicator-mode'.
When enabled, a visual indicator in the fringe will be displayed to highlight
the selected line in addition to `hl-line-mode'.  Useful if `hl-line-mode'
doesn't stand out enough with your colour theme.

Can be called with one of two arguments:

 - `always' will always show the fringe indicator.
 - `only-when-focused' will only show the fringe indicator when the treemacs
   window is focused (only possible with Emacs 27+).

For backward compatibility just enabling this mode without an explicit argument
has the same effect as using `always'."
  :init-value nil
  :global t
  :lighter nil
  :group 'treemacs
  (if treemacs-fringe-indicator-mode
      (progn
        (setf arg (or arg t))
        (if (memq arg '(always only-when-focused t))
            (treemacs--setup-fringe-indicator-mode arg)
          (call-interactively #'treemacs--setup-fringe-indicator-mode)))
    (treemacs--tear-down-fringe-indicator-mode)))

(defun treemacs--setup-fringe-indicator-mode (arg)
  "Setup `treemacs-fringe-indicator-mode'.
When ARG is `only-when-focused' a hook will be set up to only display the
fringe indicator when the treemacs window is selected."
  (interactive (list (->> (completing-read "Fringe Indicator" '("Always" "Only When Focused"))
                          (downcase)
                          (s-split " ")
                          (s-join "-")
                          (intern))))
  (setf treemacs-fringe-indicator-mode arg)
  (add-hook 'treemacs-mode-hook
            #'treemacs--enable-fringe-indicator-in-current-buffer)
  (treemacs-run-in-all-derived-buffers
   (treemacs--enable-fringe-indicator-in-current-buffer)))

(defun treemacs--enable-fringe-indicator-in-current-buffer ()
  "Set up fringe-indicator-mode for the current buffer."
  (treemacs--enable-fringe-indicator)
  (advice-add #'hl-line-highlight
              :after #'treemacs--move-fringe-indicator-to-point)
  (when (memq treemacs-fringe-indicator-mode '(t only-when-focused))
    (add-hook 'window-selection-change-functions
              #'treemacs--show-fringe-indicator-only-when-focused
              nil :local)))

(treemacs-only-during-init (treemacs-fringe-indicator-mode))

(provide 'treemacs-fringe-indicator)

;;; treemacs-fringe-indicator.el ends here
