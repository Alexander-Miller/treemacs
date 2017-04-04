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
;;; Mode definition extracted into its own file to reduce clutter.

;;; Code:

(require 'treemacs-customization)
(require 'treemacs-impl)

(defvar treemacs-mode-map
  (let ((map (make-sparse-keymap)))

    (define-key map [tab]        #'treemacs-push-button)
    (define-key map [?\t]        #'treemacs-push-button)
    (define-key map [return]     #'treemacs-visit-file-no-split)
    (define-key map (kbd "l")    #'treemacs-change-root)
    (define-key map (kbd "r")    #'treemacs-refresh)
    (define-key map (kbd "g")    #'treemacs-refresh)
    (define-key map (kbd "d")    #'treemacs-delete)
    (define-key map (kbd "cf")   #'treemacs-create-file)
    (define-key map (kbd "cd")   #'treemacs-create-dir)
    (define-key map (kbd "h")    #'treemacs-uproot)
    (define-key map (kbd "u")    #'treemacs-goto-parent-node)
    (define-key map (kbd "q")    #'treemacs-toggle)
    (define-key map (kbd "Q")    #'treemacs-kill-buffer)
    (define-key map (kbd "ov")   #'treemacs-visit-file-vertical-split)
    (define-key map (kbd "oh")   #'treemacs-visit-file-horizontal-split)
    (define-key map (kbd "oo")   #'treemacs-visit-file-no-split)
    (define-key map (kbd "oaa")  #'treemacs-visit-file-ace)
    (define-key map (kbd "oah")  #'treemacs-visit-file-ace-horizontal-split)
    (define-key map (kbd "oav")  #'treemacs-visit-file-ace-vertical-split)
    (define-key map (kbd "ox")   #'treemacs-xdg-open)

    map)
  "Keymap for `treemacs-mode'.")

(defun treemacs--evil-config ()
  "Create an evil state for treemacs mode.
Use j & k for navigating the treemacs buffer."

  (with-eval-after-load 'evil
    ;; no, `treemacs' and `evil-treemacs-state-map' are NOT undefined variables
    (with-no-warnings
      (evil-define-state treemacs
        "Treemacs state"
        :cursor '(hbar . 0)
        :enable (motion))

      (evil-set-initial-state 'treemacs-mode 'treemacs)

      (define-key evil-treemacs-state-map (kbd "j")   #'treemacs-next-line)
      (define-key evil-treemacs-state-map (kbd "k")   #'treemacs-previous-line)
      (define-key evil-treemacs-state-map (kbd "h")   #'treemacs-uproot)
      (define-key evil-treemacs-state-map (kbd "l")   #'treemacs-change-root)
      (define-key evil-treemacs-state-map (kbd "M-j") #'treemacs-next-neighbour)
      (define-key evil-treemacs-state-map (kbd "M-k") #'treemacs-previous-neighbour)
      (define-key evil-treemacs-state-map (kbd "th")  #'treemacs-toggle-show-dotfiles)
      (define-key evil-treemacs-state-map (kbd "tw")  #'treemacs-toggle-fixed-width)
      (define-key evil-treemacs-state-map (kbd "w")   #'treemacs-reset-width)))

  t)

(defun treemacs--default-config ()
  "Use n & p for navigating the treemacs buffer."

  (define-key treemacs-mode-map (kbd "n")   #'treemacs-next-line)
  (define-key treemacs-mode-map (kbd "p")   #'treemacs-previous-line)
  (define-key treemacs-mode-map (kbd "M-n") #'treemacs-next-neighbour)
  (define-key treemacs-mode-map (kbd "M-p") #'treemacs-previous-neighbour)
  (define-key treemacs-mode-map (kbd "th")  #'treemacs-toggle-show-dotfiles)
  (define-key treemacs-mode-map (kbd "tw")  #'treemacs-toggle-fixed-width)
  (define-key treemacs-mode-map (kbd "w")   #'treemacs-reset-width)

  t)

(if treemacs-be-evil
    (treemacs--evil-config)
  (treemacs--default-config))

(treemacs--create-icons)

(define-derived-mode treemacs-mode special-mode "Treemacs"
  "A major mode for displaying the file system in a tree layout."

  (setq window-size-fixed   'width
        buffer-read-only    t
        truncate-lines      t
        indent-tabs-mode    nil
        cursor-type         nil
        desktop-save-buffer nil)

  (setq-local show-paren-mode nil)
  (electric-indent-local-mode -1)
  (hl-line-mode t)

  (add-hook 'kill-buffer-hook #'treemacs--buffer-teardown nil t)

  (when (fboundp 'spaceline-compile)
    (spaceline-install "treemacs"
                       '(((workspace-number window-number)
                          :separator "|"
                          :face highlight-face)
                         major-mode)
                       nil)
    (setq
     mode-line-format
     '("%e" (:eval (spaceline-ml-treemacs))))))

(provide 'treemacs-mode)

;;; treemacs-mode.el ends here
