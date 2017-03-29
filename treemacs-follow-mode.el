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

(declare-function treemacs-follow "treemacs")

(defun treemacs--follow-advice (&rest _)
  "Advice function for `treemacs-follow-mode'.
Ignores the original arguments of `select-window' and directly calls
`treemacs-follow'."
  (treemacs-follow))

(define-minor-mode treemacs-follow-mode
  "Minor mode to run `treemacs-follow' on every window selection."
  :init-value nil
  :global     t
  :lighter    nil
  (if treemacs-follow-mode
      (advice-add 'select-window :after #'treemacs--follow-advice)
    (advice-remove 'select-window 'treemacs--follow-advice)))

(provide 'treemacs-follow-mode)

;;; treemacs-follow-mode.el ends here
