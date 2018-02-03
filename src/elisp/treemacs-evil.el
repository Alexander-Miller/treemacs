;;; treemacs-evil.el --- Evil mode integration for treemacs -*- lexical-binding: t -*-

;; Copyright (C) 2018 Alexander Miller

;; Author: Alexander Miller <alexanderm@web.de>
;; Package-Requires: ((evil "1.2.12") (treemacs "0"))
;; Package-Version: 0
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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;; Evil mode compatibility.

;;; Code:

(require 'evil)
(require 'treemacs)

(evil-define-state treemacs
  "Treemacs state"
  :cursor '(bar . 0)
  :enable (motion))

(evil-set-initial-state 'treemacs-mode 'treemacs)

(defun treemacs--turn-off-visual-state-after-click (&rest _)
  "Go back to `evil-treemacs-state' after a mouse click."
  ;; a double click will likely have opened a file so we need to make
  ;; sure to go back in the right buffer
  (-when-let- [b (treemacs-buffer-exists?)]
    (with-current-buffer b
      (evil-treemacs-state))))

(advice-add 'treemacs-leftclick-action   :after #'treemacs--turn-off-visual-state-after-click)
(advice-add 'treemacs-doubleclick-action :after #'treemacs--turn-off-visual-state-after-click)

(define-key evil-treemacs-state-map (kbd "j")   #'treemacs-next-line)
(define-key evil-treemacs-state-map (kbd "k")   #'treemacs-previous-line)
(define-key evil-treemacs-state-map (kbd "h")   #'treemacs-uproot)
(define-key evil-treemacs-state-map (kbd "l")   #'treemacs-change-root)
(define-key evil-treemacs-state-map (kbd "M-j") #'treemacs-next-neighbour)
(define-key evil-treemacs-state-map (kbd "M-k") #'treemacs-previous-neighbour)
(define-key evil-treemacs-state-map (kbd "M-J") #'treemacs-next-line-other-window)
(define-key evil-treemacs-state-map (kbd "M-K") #'treemacs-previous-line-other-window)
(define-key evil-treemacs-state-map (kbd "th")  #'treemacs-toggle-show-dotfiles)
(define-key evil-treemacs-state-map (kbd "tw")  #'treemacs-toggle-fixed-width)
(define-key evil-treemacs-state-map (kbd "tf")  #'treemacs-follow-mode)
(define-key evil-treemacs-state-map (kbd "ta")  #'treemacs-filewatch-mode)
(define-key evil-treemacs-state-map (kbd "tg")  #'treemacs-git-mode)
(define-key evil-treemacs-state-map (kbd "w")   #'treemacs-reset-width)
(define-key evil-treemacs-state-map (kbd "b")   #'treemacs-add-bookmark)
(define-key evil-treemacs-state-map (kbd "?")   #'treemacs-helpful-hydra)
(define-key evil-treemacs-state-map (kbd "RET") #'treemacs-RET-action)

(evil-define-key 'treemacs treemacs-mode-map (kbd "yr")     #'treemacs-yank-root)
(evil-define-key 'treemacs treemacs-mode-map (kbd "yy")     #'treemacs-yank-path-at-point)
(evil-define-key 'treemacs treemacs-mode-map (kbd "gr")     #'treemacs-refresh)
(evil-define-key 'treemacs treemacs-mode-map [down-mouse-1] #'ignore)

(provide 'treemacs-evil)

;;; treemacs-evil.el ends here
