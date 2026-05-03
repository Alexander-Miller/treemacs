;;; treemacs-arrow-keys-mode.el --- Arrow key navigation for treemacs -*- lexical-binding: t -*-

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

;; Minor mode to bind LEFT and RIGHT arrow keys in treemacs buffers.
;; LEFT collapses the current node or, when already collapsed, moves to the
;; parent.  RIGHT expands the current node.

;;; Code:

(require 'treemacs-interface)
(require 'treemacs-mode)

;;;###autoload
(define-minor-mode treemacs-arrow-keys-mode
  "Toggle arrow key navigation in treemacs buffers.

When enabled, the LEFT and RIGHT arrow keys are bound in `treemacs-mode-map'.

LEFT is bound to `treemacs-COLLAPSE-action': it will collapse an expanded node,
or move to its parent when the node is already collapsed or is a leaf.

RIGHT is bound to `treemacs-RIGHT-action': it will expand a collapsed node.
The exact behaviour of RIGHT is controlled by `treemacs-RIGHT-actions-config'."
  :init-value nil
  :global t
  :lighter nil
  :group 'treemacs
  (if treemacs-arrow-keys-mode
      (progn
        (define-key treemacs-mode-map [left]  #'treemacs-COLLAPSE-action)
        (define-key treemacs-mode-map [right] #'treemacs-RIGHT-action))
    (define-key treemacs-mode-map [left]  nil)
    (define-key treemacs-mode-map [right] nil)))

(provide 'treemacs-arrow-keys-mode)

;;; treemacs-arrow-keys-mode.el ends here
