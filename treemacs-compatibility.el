;;; treemacs.el --- A tree style file viewer package -*- lexical-binding: t -*-

;; Copyright (C) 2017 Alexander Miller

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
;;; Simple bits of code to make treemacs compatible with other packages
;;; that aren't worth the effort of being turned into their own package.

;;; Code:

(require 'treemacs-customization)
(require 'treemacs-impl)
(require 'treemacs-macros)

(with-eval-after-load 'winum

  ;; somestimes the compiler asks for the strangest things
  (declare-function treemacs--window-number-ten "treemacs-compatibility")

  (defun treemacs--window-number-ten ()
    (when (and (eq (selected-window) (frame-first-window))
               (treemacs-is-treemacs-window-selected?)
               (boundp 'winum-scope)
               (eq winum-scope 'frame-local))
      treemacs-winum-number))

  ;; only works in old versions of winum, should be deleted soon
  (when (boundp 'winum-assign-func)
    (setq winum-assign-func #'treemacs--window-number-ten))
  (when (boundp 'winum-assign-functions)
    (add-to-list 'winum-assign-functions #'treemacs--window-number-ten)))

(with-eval-after-load 'golden-ratio
  (when (bound-and-true-p golden-ratio-exclude-modes)
    (add-to-list 'golden-ratio-exclude-modes 'treemacs-mode)))

(with-eval-after-load 'indent-guide
  (when (boundp 'indent-guide-inhibit-modes)
    (push 'treemacs-mode indent-guide-inhibit-modes)))

(with-eval-after-load 'persp-mode
  (defun treemacs--remove-treemacs-window-in-new-frames (persp-activated-for)
    (when (or t(eq persp-activated-for 'frame))
      (-when-let (w (--first (treemacs-is-treemacs-window? it)
                             (window-list)))
        (unless (assoc (selected-frame) treemacs--buffer-access)
          (delete-window w)))))
  (declare-function treemacs--remove-treemacs-window-in-new-frames "treemacs-compatibility")
  (if (boundp 'persp-activated-functions)
      (add-to-list 'persp-activated-functions #'treemacs--remove-treemacs-window-in-new-frames)
    (treemacs--log "`persp-activated-functions' not defined - couldn't add compatibility.")))

(provide 'treemacs-compatibility)

;;; treemacs-compatibility.el ends here
