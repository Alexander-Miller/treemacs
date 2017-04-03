;;; treemacs.el --- A tree style file viewer package

;; Copyright (C) 2017 Alexander Miller

;; Author: Alexander Miller <alexanderm@web.de>
;; Package-Requires: ((cl-lib "0.5") (dash "2.11.0") (s "1.10.0") (f "0.11.0") (ace-window "0.9.0"))
;; Homepage: https://github.com/Alexander-Miller/treemacs
;; Version: 1.0
;; Keywords: tree, file, explorer

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
;;; Follow mode definition only. Everything else is extracted into its
;;; own file to reduce clutter.

;;; Code:

(require 'treemacs-impl)
(declare-function treemacs-follow "treemacs")

(defsubst treemacs--do-follow (followed-file)
  "When in the treemacs buffer move point to FOLLOWED-FILE."
  (let ((root (treemacs--current-root)))
    (when (treemacs--is-path-in-dir? followed-file root)
      (let* ((search-start (point-min))
             (dir-parts    (->> (length root) (substring followed-file) (f-split) (cdr))))
        ;; hl-line *needs* to be toggled here otherwise it won't appear to
        ;; move until the treemacs buffer is selected again and follow must
        ;; work when called from outside the treemacs buffer with treemacs-follow-mode
        (hl-line-mode -1)
        (--each dir-parts
          (setq root (f-join root it))
          (let ((btn (treemacs--goto-button-at root search-start)))
            (when (eq 'dir-closed (button-get btn 'state))
              (treemacs--open-node btn)))
          (setq search-start (point)))
        (hl-line-mode t)
        (treemacs--evade-image)
        (set-window-point (get-buffer-window) (point))))))

;; this is only to stop the compiler from complaining about unknown functions
(with-eval-after-load 'which-key
  (declare-function which-key--show-popup "which-key")
  (declare-function which-key--hide-popup "which-key"))

(defmacro treemacs--without-following (&rest body)
  "Execute BODY with `treemacs--ready' set to nil."
  `(let ((treemacs--ready))
     ,@body))

(defun treemacs--select-window-advice (&rest _)
  "Advice function for `treemacs-follow-mode'.
Ignores the original arguments of `select-window' and directly calls
`treemacs-follow'."
  (treemacs-follow))

(defun treemacs--follow-compatibility-advice (original-func &rest args)
  "Make ORIGINAL-FUNC compatible with `treemacs-follow-mode'.
Do so by running it and its ARGS through `treemacs--without-following'."
  (treemacs--without-following
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
  (treemacs-follow))

(defsubst treemacs--tear-down-follow-mode ()
  "Remove all the advice added by `treemacs--setup-follow-mode'."
  (advice-remove 'select-window 'treemacs--select-window-advice)
  ;; which key compatibility
  (progn
    (when (advice-member-p #'treemacs--follow-compatibility-advice #'which-key--show-popup)
      (advice-remove #'which-key--show-popup #'treemacs--follow-compatibility-advice))
    (when (advice-member-p #'treemacs--follow-compatibility-advice #'which-key--hide-popup)
      (advice-remove #'which-key--hide-popup #'treemacs--follow-compatibility-advice))))

(define-minor-mode treemacs-follow-mode
  "Minor mode to run `treemacs-follow' on every window selection."
  :init-value nil
  :global     t
  :lighter    nil
  (if treemacs-follow-mode
      (treemacs--setup-follow-mode)
    (treemacs--tear-down-follow-mode)))

(provide 'treemacs-follow-mode)

;;; treemacs-follow-mode.el ends here
