;;; treemacs-evil.el --- Evil mode integration for treemacs -*- lexical-binding: t -*-

;; Copyright (C) 2023 Alexander Miller

;; Author: Alexander Miller <alexanderm@web.de>
;; Package-Requires: ((emacs "26.1") (evil "1.2.12") (treemacs "0.0"))
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
;;; Evil mode compatibility.

;;; Code:

(require 'evil)
(require 'treemacs)

(treemacs-import-functions-from "treemacs-hydras"
  treemacs-common-helpful-hydra
  treemacs-advanced-helpful-hydra)

(treemacs-import-functions-from "treemacs-mouse-interface"
  treemacs-dragleftclick-action
  treemacs-leftclick-action)

(treemacs-import-functions-from "treemacs-mouse-interface"
  treemacs-copy-file)

(declare-function treemacs-add-bookmark "treemacs-bookmarks.el")

(declare-function treemacs-git-commit-diff-mode "treemacs-commit-diff-mode.el")

(evil-define-state treemacs
  "Treemacs state"
  :cursor '(bar . 0)
  :enable (motion))

(evil-set-initial-state 'treemacs-mode 'treemacs)

(defun treemacs-evil---turn-off-visual-state-after-click (&rest _)
  "Go back to `evil-treemacs-state' after a mouse click."
  ;; a double click will likely have opened a file so we need to make
  ;; sure to go back in the right buffer
  (--when-let (treemacs-get-local-buffer)
    (with-current-buffer it
      (evil-treemacs-state))))

(defun treemacs-evil--window-move-compatibility-advice (orig-fun &rest args)
  "Close Treemacs while moving windows around.
Then call ORIG-FUN with its ARGS and reopen treemacs if it was open before."
  (let* ((treemacs-window (treemacs-get-local-window))
         (is-active (and treemacs-window (window-live-p treemacs-window))))
    (when is-active (treemacs))
    (apply orig-fun args)
    (when is-active
      (save-selected-window
        (treemacs)))))

(dolist (func '(evil-window-move-far-left
                evil-window-move-far-right
                evil-window-move-very-top
                evil-window-move-very-bottom))
  (advice-add func :around #'treemacs-evil--window-move-compatibility-advice))

(advice-add 'treemacs-leftclick-action   :after #'treemacs-evil---turn-off-visual-state-after-click)
(advice-add 'treemacs-doubleclick-action :after #'treemacs-evil---turn-off-visual-state-after-click)

(define-key evil-treemacs-state-map (kbd "j")   #'treemacs-next-line)
(define-key evil-treemacs-state-map (kbd "k")   #'treemacs-previous-line)
(define-key evil-treemacs-state-map (kbd "M-j") #'treemacs-next-neighbour)
(define-key evil-treemacs-state-map (kbd "M-k") #'treemacs-previous-neighbour)
(define-key evil-treemacs-state-map (kbd "M-J") #'treemacs-next-line-other-window)
(define-key evil-treemacs-state-map (kbd "M-K") #'treemacs-previous-line-other-window)
(define-key evil-treemacs-state-map (kbd "th")  #'treemacs-toggle-show-dotfiles)
(define-key evil-treemacs-state-map (kbd "ti")  #'treemacs-hide-gitignored-files-mode)
(define-key evil-treemacs-state-map (kbd "tw")  #'treemacs-toggle-fixed-width)
(define-key evil-treemacs-state-map (kbd "tv")  #'treemacs-fringe-indicator-mode)
(define-key evil-treemacs-state-map (kbd "tf")  #'treemacs-follow-mode)
(define-key evil-treemacs-state-map (kbd "ta")  #'treemacs-filewatch-mode)
(define-key evil-treemacs-state-map (kbd "tg")  #'treemacs-git-mode)
(define-key evil-treemacs-state-map (kbd "tc")  #'treemacs-indicate-top-scroll-mode)
(define-key evil-treemacs-state-map (kbd "td")  #'treemacs-git-commit-diff-mode)
(define-key evil-treemacs-state-map (kbd "tn")  #'treemacs-indent-guide-mode)
(define-key evil-treemacs-state-map (kbd "w")   #'treemacs-set-width)
(define-key evil-treemacs-state-map (kbd ">")   #'treemacs-increase-width)
(define-key evil-treemacs-state-map (kbd "<")   #'treemacs-decrease-width)
(define-key evil-treemacs-state-map (kbd "b")   #'treemacs-add-bookmark)
(define-key evil-treemacs-state-map (kbd "?")   #'treemacs-common-helpful-hydra)
(define-key evil-treemacs-state-map (kbd "C-?") #'treemacs-advanced-helpful-hydra)
(define-key evil-treemacs-state-map (kbd "RET") #'treemacs-RET-action)
(define-key evil-treemacs-state-map (kbd "TAB") #'treemacs-TAB-action)
(define-key evil-treemacs-state-map (kbd "H")   #'treemacs-collapse-parent-node)
(define-key evil-treemacs-state-map (kbd "!")   #'treemacs-run-shell-command-for-current-node)
(define-key evil-treemacs-state-map (kbd "=")   #'treemacs-fit-window-width)
(define-key evil-treemacs-state-map (kbd "W")   #'treemacs-extra-wide-toggle)

(evil-define-key 'treemacs treemacs-mode-map (kbd "yp")     #'treemacs-copy-project-path-at-point)
(evil-define-key 'treemacs treemacs-mode-map (kbd "ya")     #'treemacs-copy-absolute-path-at-point)
(evil-define-key 'treemacs treemacs-mode-map (kbd "yr")     #'treemacs-copy-relative-path-at-point)
(evil-define-key 'treemacs treemacs-mode-map (kbd "yf")     #'treemacs-copy-file)
(evil-define-key 'treemacs treemacs-mode-map (kbd "yv")     #'treemacs-paste-dir-at-point-to-minibuffer)
(evil-define-key 'treemacs treemacs-mode-map (kbd "gr")     #'treemacs-refresh)
(evil-define-key 'treemacs treemacs-mode-map [down-mouse-1] #'treemacs-leftclick-action)
(evil-define-key 'treemacs treemacs-mode-map [drag-mouse-1] #'treemacs-dragleftclick-action)
(evil-define-key 'treemacs treemacs-mode-map (kbd "h")      #'treemacs-COLLAPSE-action)
(evil-define-key 'treemacs treemacs-mode-map (kbd "RET")    #'treemacs-RET-action)
(evil-define-key 'treemacs treemacs-mode-map (kbd "l")      #'treemacs-RET-action)
(unless (window-system)
  (evil-define-key 'treemacs treemacs-mode-map [C-i] #'treemacs-TAB-action))

(provide 'treemacs-evil)

;;; treemacs-evil.el ends here
