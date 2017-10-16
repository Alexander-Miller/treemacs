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

(require 'treemacs-impl)

(with-eval-after-load 'winum

  ;; somestimes the compiler asks for the strangest things
  (declare-function treemacs--window-number-ten "treemacs-compatibility")

  (defun treemacs--window-number-ten ()
    (when (and (eq (selected-window) (frame-first-window))
               (treemacs--is-treemacs-window-selected?)) 10))

  (when (boundp 'winum-assign-func)
    (setq winum-assign-func #'treemacs--window-number-ten)))

(with-eval-after-load 'popwin

  (defadvice popwin:create-popup-window
      (around treemacs--popwin-popup-buffer activate)
    (let ((v? (treemacs--is-visible?))
          (tb (get-buffer treemacs--buffer-name)))
      (when v?
        (with-current-buffer tb
          (setq window-size-fixed nil)))
      ad-do-it
      (when v?
        (with-current-buffer tb
          (setq window-size-fixed 'width)))))

  (defadvice popwin:close-popup-window
      (around treemacs--popwin-close-buffer activate)
    (let ((v? (treemacs--is-visible?))
          (tb (get-buffer treemacs--buffer-name)))
      (when v?
        (with-current-buffer tb
          (setq window-size-fixed nil)))
      ad-do-it
      (when v?
        (with-current-buffer tb
          (setq window-size-fixed 'width))))))

(with-eval-after-load 'golden-ratio
  (when (bound-and-true-p golden-ratio-exclude-modes)
    (add-to-list 'golden-ratio-exclude-modes 'treemacs-mode)))

(with-eval-after-load 'indent-guide
  (when (boundp 'indent-guide-inhibit-modes)
    (push 'treemacs-mode indent-guide-inhibit-modes)))

(provide 'treemacs-compatibility)

;;; treemacs-compatibility.el ends here
