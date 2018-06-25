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
;;; Major mode definition.

;;; Code:

(require 's)
(require 'hydra)
(require 'treemacs-interface)
(require 'treemacs-customization)
(require 'treemacs-faces)
(require 'treemacs-impl)
(require 'treemacs-icons)
(require 'treemacs-persistence)
(eval-and-compile (require 'treemacs-macros))

(treemacs-import-functions-from  "treemacs"
  treemacs-refresh
  treemacs-version)

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
            ((title              (format (propertize "Treemacs v%s Helpful Hydra" 'face 'treemacs-help-title-face) (treemacs-version)))
             (column-nav         (propertize "Navigation" 'face 'treemacs-help-column-face))
             (column-nodes       (propertize "Opening Nodes" 'face 'treemacs-help-column-face))
             (column-files       (propertize "File Management" 'face 'treemacs-help-column-face))
             (column-toggles     (propertize "Toggles " 'face 'treemacs-help-column-face))
             (column-projects    (propertize "Projects" 'face 'treemacs-help-column-face))
             (column-misc        (propertize "Misc." 'face 'treemacs-help-column-face))
             (key-next-line      (treemacs--find-keybind #'treemacs-next-line))
             (key-prev-line      (treemacs--find-keybind #'treemacs-previous-line))
             (key-next-neighbour (treemacs--find-keybind #'treemacs-next-neighbour))
             (key-prev-neighbour (treemacs--find-keybind #'treemacs-previous-neighbour))
             (key-goto-parent    (treemacs--find-keybind #'treemacs-goto-parent-node))
             (key-ret            (treemacs--find-keybind #'treemacs-RET-action))
             (key-tab            (treemacs--find-keybind #'treemacs-TAB-action))
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
             (key-delete         (treemacs--find-keybind #'treemacs-delete))
             (key-follow-mode    (treemacs--find-keybind #'treemacs-follow-mode))
             (key-fringe-mode    (treemacs--find-keybind #'treemacs-fringe-indicator-mode))
             (key-fwatch-mode    (treemacs--find-keybind #'treemacs-filewatch-mode))
             (key-git-mode       (treemacs--find-keybind #'treemacs-git-mode))
             (key-show-dotfiles  (treemacs--find-keybind #'treemacs-toggle-show-dotfiles))
             (key-toggle-width   (treemacs--find-keybind #'treemacs-toggle-fixed-width))
             (key-refresh        (treemacs--find-keybind #'treemacs-refresh))
             (key-set-width      (treemacs--find-keybind #'treemacs-set-width))
             (key-copy-path      (treemacs--find-keybind #'treemacs-copy-path-at-point))
             (key-copy-root      (treemacs--find-keybind #'treemacs-copy-project-root))
             (key-resort         (treemacs--find-keybind #'treemacs-resort))
             (key-bookmark       (treemacs--find-keybind #'treemacs-add-bookmark))
             (key-down-next-w    (treemacs--find-keybind #'treemacs-next-line-other-window))
             (key-up-next-w      (treemacs--find-keybind #'treemacs-previous-line-other-window))
             (key-add-project    (treemacs--find-keybind #'treemacs-add-project))
             (key-remove-project (treemacs--find-keybind #'treemacs-remove-project))
             (key-rename-project (treemacs--find-keybind #'treemacs-rename-project))
             (hydra-str
              (format
               "
%s
%s              │ %s              │ %s    │ %s                │ %s              │ %s
――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――
%s next Line        │ %s dwim TAB            │ %s create file │ %s follow mode      │ %s add project    │ %s refresh
%s prev line        │ %s dwim RET            │ %s create dir  │ %s filewatch mode   │ %s remove project │ %s (re)set width
%s next neighbour   │ %s open no split       │ %s rename      │ %s git mode         │ %s rename project │ %s copy path
%s prev neighbour   │ %s open horizontal     │ %s delete      │ %s show dotfiles    │                       │ %s copy root
%s goto parent      │ %s open vertical       │                    │ %s resizability     │                       │ %s re-sort
%s down next window │ %s open ace            │                    │ %s fringe indicator │                       │ %s bookmark
%s up next window   │ %s open ace horizontal │                    │                         │                       │
                        │ %s open ace vertical   │                    │                         │                       │
                        │ %s open externally     │                    │                         │                       │
"
               title
               column-nav               column-nodes          column-files           column-toggles          column-projects          column-misc
               (car key-next-line)      (car key-tab)         (car key-create-file)  (car key-follow-mode)   (car key-add-project)    (car key-refresh)
               (car key-prev-line)      (car key-ret)         (car key-create-dir)   (car key-fwatch-mode)   (car key-remove-project) (car key-set-width)
               (car key-next-neighbour) (car key-open)        (car key-rename)       (car key-git-mode)      (car key-rename-project) (car key-copy-path)
               (car key-prev-neighbour) (car key-open-horiz)  (car key-delete)       (car key-show-dotfiles)                          (car key-copy-root)
               (car key-goto-parent)    (car key-open-vert)                          (car key-toggle-width)                           (car key-resort)
               (car key-down-next-w)    (car key-open-ace)                           (car key-fringe-mode)                            (car key-bookmark)
               (car key-up-next-w)      (car key-open-ace-h)
               (car key-open-ext)       (car key-open-ace-v)
               )))
          (eval
           `(defhydra treemacs--helpful-hydra (:exit nil :hint nil :columns 5)
              ,hydra-str
              (,(cdr key-next-line)      #'treemacs-next-line)
              (,(cdr key-prev-line)      #'treemacs-previous-line)
              (,(cdr key-down-next-w)    #'treemacs-next-line-other-window)
              (,(cdr key-up-next-w)      #'treemacs-previous-line-other-window)
              (,(cdr key-next-neighbour) #'treemacs-next-neighbour)
              (,(cdr key-prev-neighbour) #'treemacs-previous-neighbour)
              (,(cdr key-goto-parent)    #'treemacs-goto-parent-node)
              (,(cdr key-ret)            #'treemacs-RET-action)
              (,(cdr key-tab)            #'treemacs-TAB-action)
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
              (,(cdr key-delete)         #'treemacs-delete)
              (,(cdr key-follow-mode)    #'treemacs-follow-mode)
              (,(cdr key-show-dotfiles)  #'treemacs-toggle-show-dotfiles)
              (,(cdr key-toggle-width)   #'treemacs-toggle-fixed-width)
              (,(cdr key-fringe-mode)    #'treemacs-fringe-indicator-mode)
              (,(cdr key-refresh)        #'treemacs-refresh)
              (,(cdr key-set-width)      #'treemacs-set-width)
              (,(cdr key-copy-path)      #'treemacs-copy-path-at-point)
              (,(cdr key-copy-root)      #'treemacs-copy-project-root)
              (,(cdr key-git-mode)       #'treemacs-git-mode)
              (,(cdr key-fwatch-mode)    #'treemacs-filewatch-mode)
              (,(cdr key-resort)         #'treemacs-resort)
              (,(cdr key-bookmark)       #'treemacs-add-bookmark)
              (,(cdr key-add-project)    #'treemacs-add-project)
              (,(cdr key-remove-project) #'treemacs-remove-project)
              (,(cdr key-rename-project) #'treemacs-rename-project)
              ("?" nil "Exit"))))
        (treemacs--helpful-hydra/body))
    (treemacs-log "The helpful hydra cannot be summoned without an existing treemacs buffer.")))

;; no warning - we cannot require treemacs.el where all the autoloaded functions
;; are defined or we get a recursive require, so it's either this or an equally
;; large block of `declare-function'
(with-no-warnings
  (defvar treemacs-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "?")         #'treemacs-helpful-hydra)
      (define-key map [mouse-1]         #'treemacs-leftclick-action)
      (define-key map [double-mouse-1]  #'treemacs-doubleclick-action)
      (define-key map [tab]             #'treemacs-TAB-action)
      (define-key map [?\t]             #'treemacs-TAB-action)
      (define-key map [return]          #'treemacs-RET-action)
      (define-key map (kbd "RET")       #'treemacs-RET-action)
      (define-key map (kbd "r")         #'treemacs-refresh)
      (define-key map (kbd "d")         #'treemacs-delete)
      (define-key map (kbd "cf")        #'treemacs-create-file)
      (define-key map (kbd "cd")        #'treemacs-create-dir)
      (define-key map (kbd "R")         #'treemacs-rename)
      (define-key map (kbd "u")         #'treemacs-goto-parent-node)
      (define-key map (kbd "q")         #'bury-buffer)
      (define-key map (kbd "Q")         #'treemacs-kill-buffer)
      (define-key map (kbd "ov")        #'treemacs-visit-node-vertical-split)
      (define-key map (kbd "oh")        #'treemacs-visit-node-horizontal-split)
      (define-key map (kbd "oo")        #'treemacs-visit-node-no-split)
      (define-key map (kbd "oaa")       #'treemacs-visit-node-ace)
      (define-key map (kbd "oah")       #'treemacs-visit-node-ace-horizontal-split)
      (define-key map (kbd "oav")       #'treemacs-visit-node-ace-vertical-split)
      (define-key map (kbd "ox")        #'treemacs-visit-node-in-external-application)
      (define-key map (kbd "P")         #'treemacs-peek)
      (define-key map (kbd "n")         #'treemacs-next-line)
      (define-key map (kbd "p")         #'treemacs-previous-line)
      (define-key map (kbd "M-N")       #'treemacs-next-line-other-window)
      (define-key map (kbd "M-P")       #'treemacs-previous-line-other-window)
      (define-key map (kbd "<prior>")   #'treemacs-previous-page-other-window)
      (define-key map (kbd "<next>")    #'treemacs-next-page-other-window)
      (define-key map (kbd "M-n")       #'treemacs-next-neighbour)
      (define-key map (kbd "M-p")       #'treemacs-previous-neighbour)
      (define-key map (kbd "th")        #'treemacs-toggle-show-dotfiles)
      (define-key map (kbd "tw")        #'treemacs-toggle-fixed-width)
      (define-key map (kbd "tv")        #'treemacs-fringe-indicator-mode)
      (define-key map (kbd "tg")        #'treemacs-git-mode)
      (define-key map (kbd "tf")        #'treemacs-follow-mode)
      (define-key map (kbd "ta")        #'treemacs-filewatch-mode)
      (define-key map (kbd "w")         #'treemacs-set-width)
      (define-key map (kbd "yy")        #'treemacs-copy-path-at-point)
      (define-key map (kbd "yr")        #'treemacs-copy-project-root)
      (define-key map (kbd "g")         #'treemacs-refresh)
      (define-key map (kbd "s")         #'treemacs-resort)
      (define-key map (kbd "b")         #'treemacs-add-bookmark)
      (define-key map (kbd "C-p r")     #'treemacs-rename-project)
      (define-key map (kbd "C-p a")     #'treemacs-add-project)
      (define-key map (kbd "C-p d")     #'treemacs-remove-project)
      (define-key map (kbd "C-p c c")   #'treemacs-collapse-project)
      (define-key map (kbd "C-p c o")   #'treemacs-collapse-other-projects)
      (define-key map (kbd "C-p c a")   #'treemacs-collapse-all-projects)
      (define-key map (kbd "<backtab>") #'treemacs-collapse-all-projects)
      (define-key map (kbd "C-j")       #'treemacs-next-project)
      (define-key map (kbd "C-k")       #'treemacs-previous-project)
      (define-key map (kbd "h")         #'treemacs-root-up)
      (define-key map (kbd "l")         #'treemacs-root-down)
      map)
    "Keymap for `treemacs-mode'."))

(defun treemacs--setup-mode-line ()
  "Create either a simple modeline, or integrate into spaceline."
  (setq mode-line-format
        (cond ((fboundp 'spaceline-install)
               (spaceline-install
                "treemacs" '((workspace-number
                              :face highlight-face)
                             major-mode)
                nil)
               '("%e" (:eval (spaceline-ml-treemacs))))
              ((memq 'moody-mode-line-buffer-identification
                     (default-value 'mode-line-format))
               '(:eval (moody-tab " Treemacs " 10 'down)))
              (t
               '(" Treemacs ")))))

(defun treemacs--set-default-directory ()
  "Set the default directory to the nearest directory of the current node.
If there is no node at point use \"/\" instead.

Used as a post command hook."
  (--if-let (treemacs-current-button)
      (-let [path (treemacs--nearest-path it)]
        (when (file-exists-p path)
          (setq default-directory (if (file-directory-p path) path (file-name-directory path)))))
    "/"))

;;;###autoload
(define-derived-mode treemacs-mode special-mode "Treemacs"
  "A major mode for displaying the file system in a tree layout."

  (setq buffer-read-only    t
        truncate-lines      t
        indent-tabs-mode    nil
        cursor-type         nil
        desktop-save-buffer t)

  ;; higher fuzz value makes it less likely to start a mouse drag
  ;; and make a switch to visual state
  (setq-local double-click-fuzz 15)
  (setq-local show-paren-mode nil)
  (electric-indent-local-mode -1)
  (visual-line-mode -1)
  (font-lock-mode -1)
  (jit-lock-mode nil)
  (hl-line-mode t)

  ;; needs to run manually the first time treemacs is loaded, since the hook is only added *after*
  ;; the window config was changed to show treemacs
  (unless (member #'treemacs--on-window-config-change (default-value 'window-configuration-change-hook))
    (treemacs--on-window-config-change))

  (add-hook 'window-configuration-change-hook #'treemacs--on-window-config-change)
  (add-hook 'kill-buffer-hook #'treemacs--on-buffer-kill nil t)
  ;; (add-hook 'after-make-frame-functions #'treemacs--remove-treemacs-window-in-new-frames)
  (add-to-list 'delete-frame-functions #'treemacs--on-frame-kill)
  (add-hook 'post-command-hook #'treemacs--set-default-directory nil t)

  (treemacs--adjust-icons-to-window-system)
  (treemacs--setup-icon-highlight)
  (treemacs--setup-mode-line))

(provide 'treemacs-mode)

;;; treemacs-mode.el ends here
