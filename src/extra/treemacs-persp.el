;;; treemacs-persp.el --- Persp-mode integration for treemacs -*- lexical-binding: t -*-

;; Copyright (C) 2020 Alexander Miller

;; Author: Alexander Miller <alexanderm@web.de>
;; Package-Requires: ((emacs "25.2") (treemacs "0.0") (persp-mode "2.9.7"))
;; Version: 0
;; Homepage: https://github.com/Alexander-Miller/treemacs

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
;;; Integration of persp-mode into treemacs' buffer scoping framework.

;;; Code:

(require 'treemacs)
(require 'persp-mode)
(require 'eieio)

(defclass treemacs-persp-scope (treemacs-scope) () :abstract t)
(add-to-list 'treemacs-scope-types (cons 'Perspectives 'treemacs-persp-scope))

(cl-defmethod treemacs-scope->current-scope ((_ (subclass treemacs-persp-scope)))
  (or (get-current-persp) 'none))

(cl-defmethod treemacs-scope->current-scope-name ((_ (subclass treemacs-persp-scope)) persp)
  (if (eq 'none persp)
      "None"
    (persp-name persp)))

(cl-defmethod treemacs-scope->setup ((_ (subclass treemacs-persp-scope)))
  (add-hook 'persp-activated-functions #'treemacs--change-buffer-on-scope-change)
  (add-hook 'persp-before-kill-functions #'treemacs--on-scope-kill))

(cl-defmethod treemacs-scope->cleanup ((_ (subclass treemacs-persp-scope)))
  (remove-hook 'persp-activated-functions #'treemacs--change-buffer-on-scope-change)
  (remove-hook 'persp-before-kill-functions #'treemacs--on-scope-kill))

(provide 'treemacs-persp)

;;; treemacs-persp.el ends here
