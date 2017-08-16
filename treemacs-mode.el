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
;;; Major mode definition.

;;; Code:

(require 's)
(require 'treemacs-customization)
(require 'treemacs-impl)
(require 'treemacs-persist)

;; no warning - we cannot require treemacs.el where all the autoloaded functions
;; are defined or we get a recursive require, so it's either this or an equally
;; large block of `declare-function'
(with-no-warnings
  (defvar treemacs-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map [mouse-1]    #'treemacs-click-mouse1)
      (define-key map [tab]        #'treemacs-push-button)
      (define-key map [?\t]        #'treemacs-push-button)
      (define-key map [return]     #'treemacs-visit-node-default-action)
      (define-key map (kbd "l")    #'treemacs-change-root)
      (define-key map (kbd "r")    #'treemacs-refresh)
      (define-key map (kbd "d")    #'treemacs-delete)
      (define-key map (kbd "cf")   #'treemacs-create-file)
      (define-key map (kbd "cd")   #'treemacs-create-dir)
      (define-key map (kbd "h")    #'treemacs-uproot)
      (define-key map (kbd "u")    #'treemacs-goto-parent-node)
      (define-key map (kbd "q")    #'treemacs-toggle)
      (define-key map (kbd "Q")    #'treemacs-kill-buffer)
      (define-key map (kbd "ov")   #'treemacs-visit-node-vertical-split)
      (define-key map (kbd "oh")   #'treemacs-visit-node-horizontal-split)
      (define-key map (kbd "oo")   #'treemacs-visit-node-no-split)
      (define-key map (kbd "oaa")  #'treemacs-visit-node-ace)
      (define-key map (kbd "oah")  #'treemacs-visit-node-ace-horizontal-split)
      (define-key map (kbd "oav")  #'treemacs-visit-node-ace-vertical-split)
      (define-key map (kbd "ox")   #'treemacs-xdg-open)
      (define-key map (kbd "n")    #'treemacs-next-line)
      (define-key map (kbd "p")    #'treemacs-previous-line)
      (define-key map (kbd "M-n")  #'treemacs-next-neighbour)
      (define-key map (kbd "M-p")  #'treemacs-previous-neighbour)
      (define-key map (kbd "th")   #'treemacs-toggle-show-dotfiles)
      (define-key map (kbd "tw")   #'treemacs-toggle-fixed-width)
      (define-key map (kbd "tf")   #'treemacs-follow-mode)
      (define-key map (kbd "w")    #'treemacs-reset-width)
      (define-key map (kbd "yy")   #'treemacs-yank-path-at-point)
      (define-key map (kbd "yr")   #'treemacs-yank-root)
      (define-key map (kbd "g")    #'treemacs-refresh)
      map)
    "Keymap for `treemacs-mode'."))

(treemacs--create-icons)

(defun treemacs--setup-mode-line ()
  "Create either a simple modeline, or integrate into spaceline."
  (if (fboundp 'spaceline-install)
      (progn
        (spaceline-install
         "treemacs" '(((workspace-number window-number)
                       :separator "|"
                       :face highlight-face)
                      major-mode)
         nil)
        (setq mode-line-format '("%e" (:eval (spaceline-ml-treemacs)))))
    (setq mode-line-format '(" Treemacs "))))

(define-derived-mode treemacs-mode special-mode "Treemacs"
  "A major mode for displaying the file system in a tree layout."

  (setq window-size-fixed   'width
        buffer-read-only    t
        truncate-lines      t
        indent-tabs-mode    nil
        cursor-type         nil
        desktop-save-buffer t)

  (setq-local show-paren-mode nil)
  (electric-indent-local-mode -1)
  (visual-line-mode -1)
  (hl-line-mode t)
  ;; treemacs buffer is only completely empty when it has been revived
  ;; by something like persp.el
  (when (s-blank? (buffer-string))
    (treemacs-restore))

  (add-hook 'kill-buffer-hook #'treemacs--buffer-teardown nil t)

  (treemacs--setup-mode-line))

(provide 'treemacs-mode)

;;; treemacs-mode.el ends here
