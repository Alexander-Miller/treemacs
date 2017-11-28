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
(require 'treemacs-macros)
(require 'treemacs-interface)
(require 'treemacs-customization)
(require 'treemacs-faces)
(require 'treemacs-impl)
(require 'treemacs-visuals)
(require 'treemacs-persist)
(require 'hydra)

(declare-function treemacs-refresh "treemacs")
(declare-function treemacs--helpful-hydra/body "treemacs-mode")

(defun treemacs--find-keybind (func)
  "Find the keybind for FUNC in treemacs.
Return of cons of the key formatted for inclusion in the hydra string, including
a minimum width for alignment, and the key itself for the hydra heads.
Prefer evil keybinds, otherwise pick the first result."
  (-if-let (keys (where-is-internal func))
    (let ((key
           (key-description
            (-if-let (evil-keys (--first (eq 'treemacs-state (aref it 0)) keys))
                (--map (aref evil-keys it) (number-sequence 1 (- (length evil-keys) 1)))
              (--map (aref (car keys) it) (number-sequence 0 (- (length (car keys)) 1)))))))
      (setq key
            (pcase key
              ("<return>"  "RET")
              ("<left>"    "LEFT")
              ("<right>"   "RIGHT")
              ("<up>"      "UP")
              ("<down>"    "DOWN")
              (_ key)))
      (cons (s-pad-right 8 " " (format "_%s_:" key)) key))
    (cons (s-pad-right 8 " " (format "_%s_:" " ")) " ")))

(defun treemacs-helpful-hydra ()
  "Summon the helpful hydra to show you the treemacs keymap.
If the hydra, for whatever reason, is unable the find the key a command is bound
to it will instead show a blank."
  (interactive)
  (-if-let (b (treemacs-buffer-exists?))
      (with-current-buffer b
        (let*
            ((title              (propertize "Treemacs Helpful Hydra" 'face 'treemacs-help-title-face))
             (column-nav         (propertize "Navigation" 'face 'treemacs-help-column-face))
             (column-nodes       (propertize "Opening Nodes" 'face 'treemacs-help-column-face))
             (column-files       (propertize "File Management" 'face 'treemacs-help-column-face))
             (column-toggles     (propertize "Toggles " 'face 'treemacs-help-column-face))
             (column-misc        (propertize "Misc." 'face 'treemacs-help-column-face))
             (key-next-line      (treemacs--find-keybind #'treemacs-next-line))
             (key-prev-line      (treemacs--find-keybind #'treemacs-previous-line))
             (key-next-neighbour (treemacs--find-keybind #'treemacs-next-neighbour))
             (key-prev-neighbour (treemacs--find-keybind #'treemacs-previous-neighbour))
             (key-goto-parent    (treemacs--find-keybind #'treemacs-goto-parent-node))
             (key-root-up        (treemacs--find-keybind #'treemacs-uproot))
             (key-root-down      (treemacs--find-keybind #'treemacs-change-root))
             (key-open/close     (treemacs--find-keybind #'treemacs-push-button))
             (key-dwim           (treemacs--find-keybind #'treemacs-visit-node-default-action))
             (key-open           (treemacs--find-keybind #'treemacs-visit-node-no-split))
             (key-open-horiz     (treemacs--find-keybind #'treemacs-visit-node-horizontal-split))
             (key-open-vert      (treemacs--find-keybind #'treemacs-visit-node-vertical-split))
             (key-open-ace       (treemacs--find-keybind #'treemacs-visit-node-ace))
             (key-open-ace-h     (treemacs--find-keybind #'treemacs-visit-node-ace-horizontal-split))
             (key-open-ace-v     (treemacs--find-keybind #'treemacs-visit-node-ace-vertical-split))
             (key-open-ext       (treemacs--find-keybind #'treemacs-visit-node-in-external-application))
             (key-create-file    (treemacs--find-keybind #'treemacs-create-file))
             (key-create-dir     (treemacs--find-keybind #'treemacs-create-dir))
             (key-rename         (treemacs--find-keybind #'treemacs-rename))
             (key-follow-mode    (treemacs--find-keybind #'treemacs-follow-mode))
             (key-fwatch-mode    (treemacs--find-keybind #'treemacs-filewatch-mode))
             (key-show-dotfiles  (treemacs--find-keybind #'treemacs-toggle-show-dotfiles))
             (key-toggle-width   (treemacs--find-keybind #'treemacs-toggle-fixed-width))
             (key-refresh        (treemacs--find-keybind #'treemacs-refresh))
             (key-set-width      (treemacs--find-keybind #'treemacs-reset-width))
             (key-copy-path      (treemacs--find-keybind #'treemacs-yank-path-at-point))
             (key-copy-root      (treemacs--find-keybind #'treemacs-yank-root))
             (key-resort         (treemacs--find-keybind #'treemacs-resort))
             (hydra-str
              (format
               "
%s
%s            │ %s              │ %s    │ %s              │ %s
―――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――
%s next Line      │ %s open & close        │ %s create file │ %s follow mode    │ %s refresh
%s prev line      │ %s open dwim           │ %s create dir  │ %s filewatch mode │ %s (re)set width
%s next neighbour │ %s open no split       │ %s rename      │ %s show dotfiles  │ %s copy path
%s prev neighbour │ %s open horizontal     │                    │ %s resizability   │ %s copy root
%s go to parent   │ %s open vertical       │                    │                       │ %s re-sort
%s move root up   │ %s open ace            │                    │                       │
%s move root into │ %s open ace horizontal │                    │                       │
                      │ %s open ace vertical   │                    │                       │
                      │ %s open externally     │                    │                       │
"
               title
               column-nav               column-nodes          column-files          column-toggles          column-misc
               (car key-next-line)      (car key-open/close)  (car key-create-file) (car key-follow-mode)   (car key-refresh)
               (car key-prev-line)      (car key-dwim)        (car key-create-dir)  (car key-fwatch-mode)   (car key-set-width)
               (car key-next-neighbour) (car key-open)        (car key-rename)      (car key-show-dotfiles) (car key-copy-path)
               (car key-prev-neighbour) (car key-open-horiz)                        (car key-toggle-width)  (car key-copy-root)
               (car key-goto-parent)    (car key-open-vert)                                                 (car key-resort)
               (car key-root-up)        (car key-open-ace)
               (car key-root-down)      (car key-open-ace-h)
                                        (car key-open-ace-v)
                                        (car key-open-ext))))
          (eval
           `(defhydra treemacs--helpful-hydra (:exit nil :hint nil :columns 5)
              ,hydra-str
              (,(cdr key-next-line)      #'treemacs-next-line)
              (,(cdr key-prev-line)      #'treemacs-previous-line)
              (,(cdr key-next-neighbour) #'treemacs-next-neighbour)
              (,(cdr key-prev-neighbour) #'treemacs-previous-neighbour)
              (,(cdr key-goto-parent)    #'treemacs-goto-parent-node)
              (,(cdr key-root-up)        #'treemacs-uproot)
              (,(cdr key-root-down)      #'treemacs-change-root)
              (,(cdr key-open/close)     #'treemacs-push-button)
              (,(cdr key-dwim)           #'treemacs-visit-node-default-action)
              (,(cdr key-open)           #'treemacs-visit-node-no-split)
              (,(cdr key-open-horiz)     #'treemacs-visit-node-horizontal-split)
              (,(cdr key-open-vert)      #'treemacs-visit-node-vertical-split)
              (,(cdr key-open-ace)       #'treemacs-visit-node-ace)
              (,(cdr key-open-ace-h)     #'treemacs-visit-node-ace-horizontal-split)
              (,(cdr key-open-ace-v)     #'treemacs-visit-node-ace-vertical-split)
              (,(cdr key-open-ext)       #'treemacs-visit-node-in-external-application)
              (,(cdr key-create-file)    #'treemacs-create-file)
              (,(cdr key-create-dir)     #'treemacs-create-dir)
              (,(cdr key-rename)         #'treemacs-rename)
              (,(cdr key-follow-mode)    #'treemacs-follow-mode)
              (,(cdr key-show-dotfiles)  #'treemacs-toggle-show-dotfiles)
              (,(cdr key-toggle-width)   #'treemacs-toggle-fixed-width)
              (,(cdr key-refresh)        #'treemacs-refresh)
              (,(cdr key-set-width)      #'treemacs-reset-width)
              (,(cdr key-copy-path)      #'treemacs-yank-path-at-point)
              (,(cdr key-copy-root)      #'treemacs-yank-root)
              (,(cdr key-fwatch-mode)    #'treemacs-filewatch-mode)
              (,(cdr key-resort)         #'treemacs-resort)
              ("?" nil "Exit"))))
        (treemacs--helpful-hydra/body))
    (treemacs--log "The helpful hydra cannot be summoned without an existing treemacs buffer.")))

;; no warning - we cannot require treemacs.el where all the autoloaded functions
;; are defined or we get a recursive require, so it's either this or an equally
;; large block of `declare-function'
(with-no-warnings
  (defvar treemacs-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "?")    #'treemacs-helpful-hydra)
      (define-key map [mouse-1]    #'treemacs-click-mouse1)
      (define-key map [tab]        #'treemacs-push-button)
      (define-key map [?\t]        #'treemacs-push-button)
      (define-key map [return]     #'treemacs-visit-node-default-action)
      (define-key map (kbd "RET")  #'treemacs-visit-node-default-action)
      (define-key map (kbd "l")    #'treemacs-change-root)
      (define-key map (kbd "r")    #'treemacs-refresh)
      (define-key map (kbd "d")    #'treemacs-delete)
      (define-key map (kbd "cf")   #'treemacs-create-file)
      (define-key map (kbd "cd")   #'treemacs-create-dir)
      (define-key map (kbd "R")    #'treemacs-rename)
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
      (define-key map (kbd "ox")   #'treemacs-visit-node-in-external-application)
      (define-key map (kbd "n")    #'treemacs-next-line)
      (define-key map (kbd "p")    #'treemacs-previous-line)
      (define-key map (kbd "M-n")  #'treemacs-next-neighbour)
      (define-key map (kbd "M-p")  #'treemacs-previous-neighbour)
      (define-key map (kbd "th")   #'treemacs-toggle-show-dotfiles)
      (define-key map (kbd "tw")   #'treemacs-toggle-fixed-width)
      (define-key map (kbd "tf")   #'treemacs-follow-mode)
      (define-key map (kbd "ta")   #'treemacs-filewatch-mode)
      (define-key map (kbd "w")    #'treemacs-reset-width)
      (define-key map (kbd "yy")   #'treemacs-yank-path-at-point)
      (define-key map (kbd "yr")   #'treemacs-yank-root)
      (define-key map (kbd "g")    #'treemacs-refresh)
      (define-key map (kbd "s")    #'treemacs-resort)
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

;;;###autoload
(define-derived-mode treemacs-mode special-mode "Treemacs"
  "A major mode for displaying the file system in a tree layout."

  (setq buffer-read-only    t
        truncate-lines      t
        indent-tabs-mode    nil
        cursor-type         nil
        desktop-save-buffer t)

  (setq-local show-paren-mode nil)
  (electric-indent-local-mode -1)
  (visual-line-mode -1)
  (hl-line-mode t)

  ;; needs to run manually the first time treemacs is loaded, since the hook is only added *after*
  ;; the window config was changed to show treemacs
  (unless (member #'treemacs--on-window-config-change (default-value 'window-configuration-change-hook))
    (treemacs--on-window-config-change))

  (add-hook 'window-configuration-change-hook #'treemacs--on-window-config-change)
  (add-hook 'kill-buffer-hook #'treemacs--on-buffer-kill nil t)
  ;; (add-hook 'after-make-frame-functions #'treemacs--remove-treemacs-window-in-new-frames)
  (add-to-list 'delete-frame-functions #'treemacs--remove-framelocal-buffer)

  (treemacs--setup-icon-highlight)
  (treemacs--setup-mode-line))

(provide 'treemacs-mode)

;;; treemacs-mode.el ends here
