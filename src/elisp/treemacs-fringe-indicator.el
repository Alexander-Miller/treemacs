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
;;; Handling of visuals in general and icons in particular.

;;; Code:

(require 'dash)
(require 'treemacs-impl)
(require 'treemacs-customization)
(eval-and-compile (require 'treemacs-macros))

(defvar-local treemacs--fringe-indicator-overlay nil)

(defsubst treemacs--move-fringe-indicator-to-point ()
  "Move the fringe indicator to the point."
  (move-overlay treemacs--fringe-indicator-overlay (point-at-bol) (1+ (point-at-eol))))

(defun treemacs--enable-fringe-indicator ()
  "Enabled the fringe indicator in the current buffer."
  (setq-local treemacs--fringe-indicator-overlay
              (-let [ov (make-overlay 1 1 (current-buffer))]
                (overlay-put ov 'before-string
                             (propertize " " 'display '(left-fringe
                                                        treemacs--fringe-indicator-bitmap
                                                        treemacs-fringe-indicator-face)))
                ov)))

(defun treemacs--disable-fringe-indicator ()
  "Enabled the fringe indicator in the current buffer."
  (delete-overlay treemacs--fringe-indicator-overlay))

(defun treemacs--setup-fringe-indicator-mode ()
  "Setup `treemacs-fringe-indicator-mode'."
  (treemacs-run-in-every-buffer (treemacs--enable-fringe-indicator))
  (add-hook 'treemacs-mode-hook #'treemacs--enable-fringe-indicator))

(defun treemacs--tear-down-fringe-indicator-mode ()
  "Tear down `treemacs-fringe-indicator-mode'."
  (treemacs-run-in-every-buffer (treemacs--disable-fringe-indicator))
  (remove-hook 'treemacs-mode-hook #'treemacs--enable-fringe-indicator))

(define-minor-mode treemacs-fringe-indicator-mode
  "Toggle `treemacs-fringe-indicator-mode'.
When enabled, a visual indicator in the fringe will be displayed to highlight the selected line even more.
Useful if hl-line-mode doesn't stand out enough with your color theme"
  :init-value nil
  :global t
  :lighter nil
  (if treemacs-fringe-indicator-mode
      (treemacs--setup-fringe-indicator-mode)
    (treemacs--tear-down-fringe-indicator-mode)))

(only-during-treemacs-init (treemacs-fringe-indicator-mode))

(provide 'treemacs-fringe-indicator)

;;; treemacs-fringe-indicator.el ends here
