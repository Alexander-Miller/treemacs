;;; treemacs-unfocused-hl-line-mode.el --- Dim the treemacs hl-line when unfocused -*- lexical-binding: t -*-

;; Copyright (C) 2026 rain-64

;; Author: rain-64 <https://github.com/rain-64>

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

;; Minor mode that swaps the selected-line highlight in treemacs buffers
;; to `treemacs-hl-line-unfocused-face' whenever the treemacs window is
;; not the selected window, and restores `treemacs-hl-line-face' when
;; the treemacs window regains focus.

;; NOTE: This module is lazy-loaded.

;;; Code:

(require 'face-remap)
(require 'hl-line)
(require 'treemacs-faces)

(eval-when-compile
  (require 'treemacs-macros))

(defun treemacs--hl-line-apply-unfocused ()
  "Swap `treemacs-hl-line-face' to the unfocused face in the current buffer.
Replaces the base definition of `treemacs-hl-line-face' locally so the
existing `hl-line' remap resolves to `treemacs-hl-line-unfocused-face'.
Calls `hl-line-highlight' to force the overlay to pick up the new face
without waiting for point to move."
  (face-remap-set-base 'treemacs-hl-line-face
                       'treemacs-hl-line-unfocused-face)
  (hl-line-highlight))

(defun treemacs--hl-line-apply-focused ()
  "Restore the default definition of `treemacs-hl-line-face' in this buffer.
Calls `hl-line-highlight' to force the overlay to pick up the restored
face without waiting for point to move."
  (face-remap-reset-base 'treemacs-hl-line-face)
  (hl-line-highlight))

(defun treemacs--hl-line-update-focus (&rest _)
  "Reapply the focused or unfocused hl-line face in all treemacs buffers.
A treemacs buffer is considered focused when it is displayed in the
globally selected window; all other treemacs buffers get the
unfocused face."
  (treemacs-run-in-all-derived-buffers
   (if (eq (window-buffer (selected-window)) (current-buffer))
       (treemacs--hl-line-apply-focused)
     (treemacs--hl-line-apply-unfocused))))

(defun treemacs--setup-unfocused-hl-line-mode ()
  "Setup for `treemacs-unfocused-hl-line-mode'."
  (add-hook 'window-selection-change-functions
            #'treemacs--hl-line-update-focus)
  (add-hook 'treemacs-mode-hook #'treemacs--hl-line-update-focus)
  (treemacs--hl-line-update-focus))

(defun treemacs--tear-down-unfocused-hl-line-mode ()
  "Tear-down for `treemacs-unfocused-hl-line-mode'."
  (remove-hook 'window-selection-change-functions
               #'treemacs--hl-line-update-focus)
  (remove-hook 'treemacs-mode-hook #'treemacs--hl-line-update-focus)
  (treemacs-run-in-all-derived-buffers
   (treemacs--hl-line-apply-focused)))

;;;###autoload
(define-minor-mode treemacs-unfocused-hl-line-mode
  "Minor mode to dim the treemacs hl-line when treemacs is not focused.

When enabled the selected-line highlight in every treemacs buffer is
drawn with `treemacs-hl-line-unfocused-face' whenever the treemacs
window is not the selected window, and with `treemacs-hl-line-face'
when it is.  Useful in IDE-style layouts, where a visible but dimmer
cursor line in the sidebar makes it obvious at a glance which side
has keyboard focus without losing the indicator of which file the
cursor is on.

Customise `treemacs-hl-line-unfocused-face' to control what the
unfocused highlight looks like.  Its default inherits from
`treemacs-hl-line-face' so appearance is unchanged until customised.

Requires `window-selection-change-functions', available since
Emacs 27.1."
  :init-value nil
  :global     t
  :lighter    nil
  :group      'treemacs
  (cond
   ((not treemacs-unfocused-hl-line-mode)
    (treemacs--tear-down-unfocused-hl-line-mode))
   ((not (boundp 'window-selection-change-functions))
    (setq treemacs-unfocused-hl-line-mode nil)
    (user-error "%s %s"
                "treemacs-unfocused-hl-line-mode is only available in Emacs"
                "versions that support `window-selection-change-functions'"))
   (t
    (treemacs--setup-unfocused-hl-line-mode))))

(provide 'treemacs-unfocused-hl-line-mode)

;;; treemacs-unfocused-hl-line-mode.el ends here
