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
;;; Simple bits of code to make treemacs compatible with other packages
;;; that aren't worth the effort of being turned into their own package.

;;; Code:

(require 'treemacs-customization)
(require 'treemacs-impl)
(eval-and-compile (require 'treemacs-macros))

(with-eval-after-load 'winum

  (when (boundp 'winum-ignored-buffers)
    (dolist (n (number-sequence 1 5))
      (add-to-list 'winum-ignored-buffers
                   (format "%sFramebuffer-%s*" treemacs--buffer-name-prefix n)))))

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
    (treemacs-log "`persp-activated-functions' not defined - couldn't add compatibility.")))

(defun treemacs--split-window-advice (original-split-function &rest args)
  "Advice to make sure window splits are sized correctly with treemacs.
This will treat the treemacs window as a side-window for the duration of the
split, calling the ORIGINAL-SPLIT-FUNCTION with its ARGS. This prevents the
calculations in `split-window-right' from outputting the wrong result for the
width of the new window when the treemacs window is visible."
  (-let [w (treemacs--is-visible?)]
    (unwind-protect
        (progn
          (when w (set-window-parameter w 'window-side treemacs-position))
          (apply original-split-function args))
      (when w (set-window-parameter w 'window-side nil)))))
(advice-add 'split-window-right :around #'treemacs--split-window-advice)

(provide 'treemacs-compatibility)

;;; treemacs-compatibility.el ends here
