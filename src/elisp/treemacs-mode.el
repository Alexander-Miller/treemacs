;;; treemacs.el --- A tree style file viewer package -*- lexical-binding: t -*-

;; Copyright (C) 2019 Alexander Miller

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

(require 'eldoc)
(require 's)
(require 'f)
(require 'hydra)
(require 'treemacs-interface)
(require 'treemacs-customization)
(require 'treemacs-faces)
(require 'treemacs-core-utils)
(require 'treemacs-icons)
(require 'treemacs-scope)
(require 'treemacs-persistence)
(require 'treemacs-dom)
(require 'treemacs-workspaces)
(require 'treemacs-visuals)
(eval-and-compile (require 'treemacs-macros))
(with-eval-after-load 'bookmark
  (require 'treemacs-bookmarks))

(treemacs-import-functions-from  "treemacs"
  treemacs-refresh
  treemacs-version)
(treemacs-import-functions-from "treemacs-bookmarks"
  treemacs-add-bookmark
  treemacs--make-bookmark-record)

(declare-function treemacs--helpful-hydra/body "treemacs-mode")

(defvar bookmark-make-record-function)

(defvar-local treemacs--eldoc-msg nil
  "Message to be output by `treemacs--eldoc-function'.
Will be set by `treemacs--post-command'.")

(defconst treemacs--eldoc-obarray
  (-let [ob (make-vector 59 0)]
    (mapatoms
     (lambda (cmd) (set (intern (symbol-name cmd) ob) t))
     eldoc-message-commands)
    (dolist (cmd '(treemacs-next-line
                   treemacs-previous-line
                   treemacs-next-neighbour
                   treemacs-previous-neighbour
                   treemacs-next-project
                   treemacs-previous-project
                   treemacs-goto-parent-node
                   treemacs-TAB-action
                   treemacs-select-window
                   treemacs-leftclick-action))
      (set (intern (symbol-name cmd) ob) t))
    ob)
  "Treemacs' own eldoc obarray.")

(cl-defun treemacs--find-keybind (func &optional (pad 8))
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
      (setf key
            (s-replace-all
             '(("<return>" . "RET")
               ("<left>"   . "LEFT")
               ("<right>"  . "RIGHT")
               ("<up>"     . "UP")
               ("<down>"   . "DOWN")
               ("^"        . "C-")
               ("⇢⌥"     . ">O-")
               ("⌥"       . "O-")
               ("⇢⌘"      . ">#-")
               ("⌘"       . "#-")
               ("⇧"        . "S-"))
             key))
      (cons (s-pad-right pad " " (format "_%s_:" key)) key))
    (cons (s-pad-right pad " " (format "_%s_:" " ")) " ")))

(defun treemacs-helpful-hydra ()
  "Summon the helpful hydra to show you the treemacs keymap.
If the hydra, for whatever reason, is unable the find the key a command is bound
to it will instead show a blank."
  (interactive)
  (-if-let (b (treemacs-get-local-buffer))
      (with-current-buffer b
        (let*
            ((title              (format (propertize "Treemacs %s Helpful Hydra" 'face 'treemacs-help-title-face) (treemacs-version)))
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
             (key-open-mru       (treemacs--find-keybind #'treemacs-visit-node-in-most-recently-used-window))
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
             (key-copy-file      (treemacs--find-keybind #'treemacs-copy-file))
             (key-move-file      (treemacs--find-keybind #'treemacs-move-file))
             (key-resort         (treemacs--find-keybind #'treemacs-resort))
             (key-bookmark       (treemacs--find-keybind #'treemacs-add-bookmark))
             (key-down-next-w    (treemacs--find-keybind #'treemacs-next-line-other-window))
             (key-up-next-w      (treemacs--find-keybind #'treemacs-previous-line-other-window))
             (key-add-project    (treemacs--find-keybind #'treemacs-add-project-to-workspace 12))
             (key-remove-project (treemacs--find-keybind #'treemacs-remove-project-from-workspace 12))
             (key-rename-project (treemacs--find-keybind #'treemacs-rename-project 12))
             (key-close-above    (treemacs--find-keybind #'treemacs-collapse-parent-node))
             (hydra-str
              (format
               "
%s
%s              │ %s              │ %s    │ %s                │ %s                  │ %s
――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――
%s next Line        │ %s dwim TAB            │ %s create file │ %s follow mode      │ %s add project    │ %s refresh
%s prev line        │ %s dwim RET            │ %s create dir  │ %s filewatch mode   │ %s remove project │ %s (re)set width
%s next neighbour   │ %s open no split       │ %s rename      │ %s git mode         │ %s rename project │ %s copy path
%s prev neighbour   │ %s open horizontal     │ %s delete      │ %s show dotfiles    │                           │ %s copy root
%s goto parent      │ %s open vertical       │ %s copy        │ %s resizability     │                           │ %s re-sort
%s down next window │ %s open ace            │ %s move        │ %s fringe indicator │                           │ %s bookmark
%s up next window   │ %s open ace horizontal │                    │                         │                           │
                        │ %s open ace vertical   │                    │                         │                           │
                        │ %s open mru window     │                    │                         │                           │
                        │ %s open externally     │                    │                         │                           │
                        │ %s close parent        │                    │                         │                           │
"
               title
               column-nav               column-nodes          column-files           column-toggles          column-projects          column-misc
               (car key-next-line)      (car key-tab)         (car key-create-file)  (car key-follow-mode)   (car key-add-project)    (car key-refresh)
               (car key-prev-line)      (car key-ret)         (car key-create-dir)   (car key-fwatch-mode)   (car key-remove-project) (car key-set-width)
               (car key-next-neighbour) (car key-open)        (car key-rename)       (car key-git-mode)      (car key-rename-project) (car key-copy-path)
               (car key-prev-neighbour) (car key-open-horiz)  (car key-delete)       (car key-show-dotfiles)                          (car key-copy-root)
               (car key-goto-parent)    (car key-open-vert)   (car key-copy-file)    (car key-toggle-width)                           (car key-resort)
               (car key-down-next-w)    (car key-open-ace)    (car key-move-file)    (car key-fringe-mode)                            (car key-bookmark)
               (car key-up-next-w)      (car key-open-ace-h)
                                        (car key-open-ace-v)
                                        (car key-open-mru)
                                        (car key-open-ext)
                                        (car key-close-above)
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
              (,(cdr key-open-mru)       #'treemacs-visit-node-in-most-recently-used-window)
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
              (,(cdr key-copy-file)      #'treemacs-copy-file)
              (,(cdr key-move-file)      #'treemacs-move-file)
              (,(cdr key-git-mode)       #'treemacs-git-mode)
              (,(cdr key-fwatch-mode)    #'treemacs-filewatch-mode)
              (,(cdr key-resort)         #'treemacs-resort)
              (,(cdr key-bookmark)       #'treemacs-add-bookmark)
              (,(cdr key-add-project)    #'treemacs-add-project-to-workspace)
              (,(cdr key-remove-project) #'treemacs-remove-project-from-workspace)
              (,(cdr key-rename-project) #'treemacs-rename-project)
              (,(cdr key-close-above)    #'treemacs-collapse-parent-node)
              ("?" nil "Exit"))))
        (treemacs--helpful-hydra/body))
    (treemacs-log "The helpful hydra cannot be summoned without an existing treemacs buffer.")))

;; no warning - we cannot require treemacs.el where all the autoloaded functions
;; are defined or we get a recursive require, so it's either this or an equally
;; large block of `declare-function'
(with-no-warnings
  (defvar treemacs-project-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "r")     #'treemacs-rename-project)
      (define-key map (kbd "a")     #'treemacs-add-project-to-workspace)
      (define-key map (kbd "d")     #'treemacs-remove-project-from-workspace)
      (define-key map (kbd "c c")   #'treemacs-collapse-project)
      (define-key map (kbd "c o")   #'treemacs-collapse-other-projects)
      (define-key map (kbd "c a")   #'treemacs-collapse-all-projects)
      map)
    "Keymap for project-related commands in `treemacs-mode'.")
  (defvar treemacs-workspace-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "r")     #'treemacs-rename-workspace)
      (define-key map (kbd "a")     #'treemacs-create-workspace)
      (define-key map (kbd "d")     #'treemacs-remove-workspace)
      (define-key map (kbd "s")     #'treemacs-switch-workspace)
      (define-key map (kbd "e")     #'treemacs-edit-workspaces)
      (define-key map (kbd "f")     #'treemacs-set-fallback-workspace)
      map)
    "Keymap for workspace-related commands in `treemacs-mode'.")
  (defvar treemacs-node-visit-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "v")        #'treemacs-visit-node-vertical-split)
      (define-key map (kbd "h")        #'treemacs-visit-node-horizontal-split)
      (define-key map (kbd "o")        #'treemacs-visit-node-no-split)
      (define-key map (kbd "aa")       #'treemacs-visit-node-ace)
      (define-key map (kbd "ah")       #'treemacs-visit-node-ace-horizontal-split)
      (define-key map (kbd "av")       #'treemacs-visit-node-ace-vertical-split)
      (define-key map (kbd "r")        #'treemacs-visit-node-in-most-recently-used-window)
      (define-key map (kbd "x")        #'treemacs-visit-node-in-external-application)
      map)
    "Keymap for node-visiting commands in `treemacs-mode'.")
  (defvar treemacs-toggle-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "h")        #'treemacs-toggle-show-dotfiles)
      (define-key map (kbd "w")        #'treemacs-toggle-fixed-width)
      (define-key map (kbd "v")        #'treemacs-fringe-indicator-mode)
      (define-key map (kbd "g")        #'treemacs-git-mode)
      (define-key map (kbd "f")        #'treemacs-follow-mode)
      (define-key map (kbd "a")        #'treemacs-filewatch-mode)
      map)
    "Keymap for commands that toggle state in `treemacs-mode'.")
  (defvar treemacs-copy-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "y")        #'treemacs-copy-path-at-point)
      (define-key map (kbd "r")        #'treemacs-copy-project-root)
      (define-key map (kbd "f")        #'treemacs-copy-file)
      map)
    "Keymap for copy commands in `treemacs-mode'.")
  (defvar treemacs-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "?")         #'treemacs-helpful-hydra)
      (define-key map [down-mouse-1]    #'treemacs-leftclick-action)
      (define-key map [drag-mouse-1]    #'treemacs-dragleftclick-action)
      (define-key map [double-mouse-1]  #'treemacs-doubleclick-action)
      (define-key map [mouse-3]         #'treemacs-rightclick-menu)
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
      (define-key map (kbd "q")         #'treemacs-quit)
      (define-key map (kbd "Q")         #'treemacs-kill-buffer)
      (define-key map (kbd "o")         treemacs-node-visit-map)
      (define-key map (kbd "P")         #'treemacs-peek)
      (define-key map (kbd "n")         #'treemacs-next-line)
      (define-key map (kbd "p")         #'treemacs-previous-line)
      (define-key map (kbd "M-N")       #'treemacs-next-line-other-window)
      (define-key map (kbd "M-P")       #'treemacs-previous-line-other-window)
      (define-key map (kbd "<prior>")   #'treemacs-previous-page-other-window)
      (define-key map (kbd "<next>")    #'treemacs-next-page-other-window)
      (define-key map (kbd "M-n")       #'treemacs-next-neighbour)
      (define-key map (kbd "M-p")       #'treemacs-previous-neighbour)
      (define-key map (kbd "t")         treemacs-toggle-map)
      (define-key map (kbd "w")         #'treemacs-set-width)
      (define-key map (kbd "y")         treemacs-copy-map)
      (define-key map (kbd "m")         #'treemacs-move-file)
      (define-key map (kbd "g")         #'treemacs-refresh)
      (define-key map (kbd "s")         #'treemacs-resort)
      (define-key map (kbd "b")         #'treemacs-add-bookmark)
      (define-key map (kbd "C-c C-p")   treemacs-project-map)
      (define-key map (kbd "C-c C-w")   treemacs-workspace-map)
      (define-key map (kbd "<M-up>")    #'treemacs-move-project-up)
      (define-key map (kbd "<M-down>")  #'treemacs-move-project-down)
      (define-key map (kbd "<backtab>") #'treemacs-collapse-all-projects)
      (define-key map (kbd "C-j")       #'treemacs-next-project)
      (define-key map (kbd "C-k")       #'treemacs-previous-project)
      (define-key map (kbd "h")         #'treemacs-root-up)
      (define-key map (kbd "l")         #'treemacs-root-down)
      (define-key map (kbd "H")         #'treemacs-collapse-parent-node)
      (define-key map (kbd "!")         #'treemacs-run-shell-command-for-current-node)
      (define-key map (kbd "M-!")       #'treemacs-run-shell-command-in-project-root)
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
              ((and (fboundp 'doom-modeline)
                    (fboundp 'doom-modeline-def-modeline))
               (doom-modeline-def-modeline 'treemacs '(bar " " major-mode))
               (doom-modeline 'treemacs))
              (t
               '(" Treemacs ")))))

(defun treemacs--post-command ()
  "Set the default directory to the nearest directory of the current node.
If there is no node at point use \"~/\" instead.
Also skip hidden buttons (as employed by variadic extensions).

Used as a post command hook."
  (-when-let (btn (treemacs-current-button))
    (when (treemacs-button-get btn 'invisible)
      (treemacs-next-line 1))
    (-if-let* ((project (treemacs-project-of-node btn))
               (path (or (treemacs-button-get btn :default-directory)
                         (treemacs--nearest-path btn))))
        (when (and (treemacs-project->is-readable? project)
                   (file-readable-p path))
          (setq treemacs--eldoc-msg path
                default-directory (treemacs--add-trailing-slash
                                   (if (file-directory-p path) path (file-name-directory path)))))
      (setq treemacs--eldoc-msg nil
            default-directory "~/"))))

(defun treemacs--eldoc-function ()
  "Treemacs' implementation of `eldoc-documentation-function'.
Will simply return `treemacs--eldoc-msg'."
  (when (and treemacs-eldoc-display treemacs--eldoc-msg)
    (propertize treemacs--eldoc-msg 'face 'font-lock-string-face)))

;;;###autoload
(define-derived-mode treemacs-mode special-mode "Treemacs"
  "A major mode for displaying the file system in a tree layout."

  (setq buffer-read-only         t
        truncate-lines           t
        indent-tabs-mode         nil
        desktop-save-buffer      nil
        window-size-fixed        (when treemacs--width-is-locked 'width)
        treemacs--in-this-buffer t)

  (unless treemacs-show-cursor
    (setq cursor-type nil))
  (when (boundp 'evil-treemacs-state-cursor)
    (with-no-warnings
      (setq evil-treemacs-state-cursor
            (if treemacs-show-cursor
                evil-motion-state-cursor
              '(hbar . 0)))))

  ;; higher fuzz value makes it less likely to start a mouse drag
  ;; and make a switch to visual state
  (setq-local double-click-fuzz 15)
  (setq-local show-paren-mode nil)
  (setq-local eldoc-documentation-function #'treemacs--eldoc-function)
  (setq-local eldoc-message-commands treemacs--eldoc-obarray)
  ;; integrate with bookmark.el
  (setq-local bookmark-make-record-function #'treemacs--make-bookmark-record)
  (electric-indent-local-mode -1)
  (visual-line-mode -1)
  (font-lock-mode -1)
  (jit-lock-mode nil)
  (buffer-disable-undo)
  ;; fringe indicator must be set up right here, before hl-line-mode, since activating hl-line-mode will
  ;; invoke the movement of the fringe overlay that would otherwise be nil
  (when treemacs-fringe-indicator-mode
    (treemacs--setup-fringe-indicator-mode))
  (hl-line-mode t)

  ;; needs to run manually the first time treemacs is loaded, since the hook is only added *after*
  ;; the window config was changed to show treemacs
  (unless (member #'treemacs--on-window-config-change (default-value 'window-configuration-change-hook))
    (treemacs--on-window-config-change))

  (add-hook 'window-configuration-change-hook #'treemacs--on-window-config-change)
  (add-hook 'kill-buffer-hook #'treemacs--on-buffer-kill nil t)
  (add-hook 'post-command-hook #'treemacs--post-command nil t)

  (treemacs--build-indentation-cache 6)
  (treemacs--select-icon-set)
  (treemacs--setup-icon-highlight)
  (treemacs--setup-icon-background-colors)
  (treemacs--setup-mode-line)
  (treemacs--reset-dom)
  (treemacs--reset-project-positions))

(defun treemacs--mode-check-advice (mode-activation &rest args)
  "Verify that `treemacs-mode' is called in the right place.
Must be run as advice to prevent changing of the major mode.
Will run original MODE-ACTIVATION and its ARGS only when
`treemacs--in-this-buffer' is non-nil."
  (cond
   (treemacs--in-this-buffer
    (apply mode-activation args))
   ((eq major-mode 'treemacs-mode)
    (ignore "Reactivating the major-mode resets buffer-local variables."))
   (t
    (switch-to-buffer (get-buffer-create "*Clippy*"))
    (erase-buffer)
    (insert
     (format
      "
 --------------------------------------------------------------------------------------
 | It looks like you are trying to run treemacs. Would you like some help with that?  |
 | You have called %s, but that is just the major mode for treemacs'       |
 | buffers, it is not meant to be used manually.                                      |
 |                                                                                    |
 | Instead you should call a function like                                            |
 |  * %s,                                                                       |
 |  * %s, or                                                      |
 |  * %s                                        |
 |                                                                                    |
 | You can safely delete this buffer.                                                 |
 --------------------------------------------------------------------------------------
%s
"
      (propertize "treemacs-mode" 'face 'font-lock-function-name-face)
      (propertize "treemacs" 'face 'font-lock-function-name-face)
      (propertize "treemacs-select-window" 'face 'font-lock-function-name-face)
      (propertize "treemacs-add-and-display-current-project" 'face 'font-lock-function-name-face)
      (propertize
       "     \\
     \\
   ____
   /  \\
   |  |
   @  @
   |  |
   || |/
   || ||
   |\\_/|
   \\___/" 'face 'font-lock-keyword-face))))))

(advice-add #'treemacs-mode :around #'treemacs--mode-check-advice)

(provide 'treemacs-mode)

;;; treemacs-mode.el ends here
