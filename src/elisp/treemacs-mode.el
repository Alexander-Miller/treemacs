;;; treemacs-mode.el --- A tree style file viewer package -*- lexical-binding: t -*-

;; Copyright (C) 2021 Alexander Miller

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

;; Major mode definition.

;;; Code:

(require 'eldoc)
(require 's)
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

(eval-when-compile
  (require 'treemacs-macros)
  (require 'cl-lib))

(with-eval-after-load 'bookmark
  (require 'treemacs-bookmarks))

(treemacs-import-functions-from  "treemacs"
  treemacs-refresh
  treemacs-version
  treemacs-edit-workspaces)

(treemacs-import-functions-from "treemacs-bookmarks"
  treemacs-add-bookmark
  treemacs--make-bookmark-record)

(treemacs-import-functions-from "treemacs-hydras"
  treemacs-helpful-hydra
  treemacs-common-helpful-hydra
  treemacs-advanced-helpful-hydra)

(treemacs-import-functions-from "treemacs-tags"
  treemacs--create-imenu-index-functione)

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

(defvar treemacs-project-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "r")     'treemacs-rename-project)
    (define-key map (kbd "a")     'treemacs-add-project-to-workspace)
    (define-key map (kbd "d")     'treemacs-remove-project-from-workspace)
    (define-key map (kbd "c c")   'treemacs-collapse-project)
    (define-key map (kbd "c o")   'treemacs-collapse-other-projects)
    (define-key map (kbd "c a")   'treemacs-collapse-all-projects)
    map)
  "Keymap for project-related commands in `treemacs-mode'.")

(defvar treemacs-workspace-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "r")     'treemacs-rename-workspace)
    (define-key map (kbd "a")     'treemacs-create-workspace)
    (define-key map (kbd "d")     'treemacs-remove-workspace)
    (define-key map (kbd "s")     'treemacs-switch-workspace)
    (define-key map (kbd "e")     'treemacs-edit-workspaces)
    (define-key map (kbd "f")     'treemacs-set-fallback-workspace)
    (define-key map (kbd "n")     'treemacs-next-workspace)
    map)
  "Keymap for workspace-related commands in `treemacs-mode'.")

(defvar treemacs-node-visit-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "v")        'treemacs-visit-node-vertical-split)
    (define-key map (kbd "c")        'treemacs-visit-node-close-treemacs)
    (define-key map (kbd "h")        'treemacs-visit-node-horizontal-split)
    (define-key map (kbd "o")        'treemacs-visit-node-no-split)
    (define-key map (kbd "aa")       'treemacs-visit-node-ace)
    (define-key map (kbd "ah")       'treemacs-visit-node-ace-horizontal-split)
    (define-key map (kbd "av")       'treemacs-visit-node-ace-vertical-split)
    (define-key map (kbd "r")        'treemacs-visit-node-in-most-recently-used-window)
    (define-key map (kbd "x")        'treemacs-visit-node-in-external-application)
    map)
  "Keymap for node-visiting commands in `treemacs-mode'.")

(defvar treemacs-toggle-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "h")        'treemacs-toggle-show-dotfiles)
    (define-key map (kbd "i")        'treemacs-hide-gitignored-files-mode)
    (define-key map (kbd "w")        'treemacs-toggle-fixed-width)
    (define-key map (kbd "v")        'treemacs-fringe-indicator-mode)
    (define-key map (kbd "g")        'treemacs-git-mode)
    (define-key map (kbd "f")        'treemacs-follow-mode)
    (define-key map (kbd "a")        'treemacs-filewatch-mode)
    (define-key map (kbd "n")        'treemacs-indent-guide-mode)
    map)
  "Keymap for commands that toggle state in `treemacs-mode'.")

(defvar treemacs-copy-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "a")        'treemacs-copy-absolute-path-at-point)
    (define-key map (kbd "r")        'treemacs-copy-relative-path-at-point)
    (define-key map (kbd "p")        'treemacs-copy-project-path-at-point)
    (define-key map (kbd "f")        'treemacs-copy-file)
    map)
  "Keymap for copy commands in `treemacs-mode'.")

(defvar treemacs-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "?")         'treemacs-common-helpful-hydra)
    (define-key map (kbd "C-?")       'treemacs-advanced-helpful-hydra)
    (define-key map [down-mouse-1]    'treemacs-leftclick-action)
    (define-key map [drag-mouse-1]    'treemacs-dragleftclick-action)
    (define-key map [double-mouse-1]  'treemacs-doubleclick-action)
    (define-key map [mouse-3]         'treemacs-rightclick-menu)
    (define-key map [tab]             'treemacs-TAB-action)
    (define-key map [?\t]             'treemacs-TAB-action)
    (define-key map [return]          'treemacs-RET-action)
    (define-key map (kbd "RET")       'treemacs-RET-action)
    (define-key map (kbd "r")         'treemacs-refresh)
    (define-key map (kbd "d")         'treemacs-delete-file)
    (define-key map (kbd "cf")        'treemacs-create-file)
    (define-key map (kbd "cd")        'treemacs-create-dir)
    (define-key map (kbd "R")         'treemacs-rename-file)
    (define-key map (kbd "u")         'treemacs-goto-parent-node)
    (define-key map (kbd "q")         'treemacs-quit)
    (define-key map (kbd "Q")         'treemacs-kill-buffer)
    (define-key map (kbd "o")         treemacs-node-visit-map)
    (define-key map (kbd "P")         'treemacs-peek-mode)
    (define-key map (kbd "n")         'treemacs-next-line)
    (define-key map (kbd "p")         'treemacs-previous-line)
    (define-key map (kbd "M-N")       'treemacs-next-line-other-window)
    (define-key map (kbd "M-P")       'treemacs-previous-line-other-window)
    (define-key map (kbd "<prior>")   'treemacs-previous-page-other-window)
    (define-key map (kbd "<next>")    'treemacs-next-page-other-window)
    (define-key map (kbd "M-n")       'treemacs-next-neighbour)
    (define-key map (kbd "M-p")       'treemacs-previous-neighbour)
    (define-key map (kbd "t")         treemacs-toggle-map)
    (define-key map (kbd "w")         'treemacs-set-width)
    (define-key map (kbd "<")         'treemacs-decrease-width)
    (define-key map (kbd ">")         'treemacs-increase-width)
    (define-key map (kbd "y")         treemacs-copy-map)
    (define-key map (kbd "m")         'treemacs-move-file)
    (define-key map (kbd "g")         'treemacs-refresh)
    (define-key map (kbd "s")         'treemacs-resort)
    (define-key map (kbd "b")         'treemacs-add-bookmark)
    (define-key map (kbd "C-c C-p")   treemacs-project-map)
    (define-key map (kbd "C-c C-w")   treemacs-workspace-map)
    (define-key map (kbd "<M-up>")    'treemacs-move-project-up)
    (define-key map (kbd "<M-down>")  'treemacs-move-project-down)
    (define-key map (kbd "<backtab>") 'treemacs-collapse-all-projects)
    (define-key map (kbd "C-j")       'treemacs-next-project)
    (define-key map (kbd "C-k")       'treemacs-previous-project)
    (define-key map (kbd "h")         'treemacs-COLLAPSE-action)
    (define-key map (kbd "l")         'treemacs-RET-action)
    (define-key map (kbd "M-h")       'treemacs-COLLAPSE-action)
    (define-key map (kbd "M-l")       'treemacs-RET-action)
    (define-key map (kbd "M-H")       'treemacs-root-up)
    (define-key map (kbd "M-L")       'treemacs-root-down)
    (define-key map (kbd "H")         'treemacs-collapse-parent-node)
    (define-key map (kbd "!")         'treemacs-run-shell-command-for-current-node)
    (define-key map (kbd "M-!")       'treemacs-run-shell-command-in-project-root)
    (define-key map (kbd "C")         'treemacs-cleanup-litter)
    (define-key map (kbd "=")         'treemacs-fit-window-width)
    (define-key map (kbd "W")         'treemacs-extra-wide-toggle)
    map)
  "Keymap for `treemacs-mode'.")

(defun treemacs--setup-mode-line ()
  "Create either a simple modeline, or integrate into spaceline."
  (setq mode-line-format
        (cond (treemacs-user-mode-line-format
               (if (eq 'none treemacs-user-mode-line-format)
                   nil
                 treemacs-user-mode-line-format))
              ((fboundp 'spaceline-install)
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
        window-size-fixed        (when treemacs-width-is-initially-locked 'width)
        treemacs--in-this-buffer t)

  (unless treemacs-show-cursor
    (setq cursor-type nil))
  (when (boundp 'evil-treemacs-state-cursor)
    (with-no-warnings
      (setq evil-treemacs-state-cursor
            (if treemacs-show-cursor
                evil-motion-state-cursor
              '(bar . 0)))))

  ;; higher fuzz value makes it less likely to start a mouse drag
  ;; and make a switch to visual state
  (setq-local double-click-fuzz 15)
  (setq-local show-paren-mode nil)
  (setq-local tab-width 1)
  (setq-local eldoc-documentation-function #'treemacs--eldoc-function)
  (setq-local eldoc-message-commands treemacs--eldoc-obarray)
  (setq-local imenu-create-index-function #'treemacs--create-imenu-index-function)
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
    (treemacs--enable-fringe-indicator))
  (if treemacs-user-header-line-format
      (setf header-line-format treemacs-user-header-line-format)
    (when header-line-format
      (setf header-line-format nil)))
  (hl-line-mode t)

  ;; needs to run manually the first time treemacs is loaded, since the hook is only added *after*
  ;; the window config was changed to show treemacs
  (unless (member #'treemacs--on-window-config-change (default-value 'window-configuration-change-hook))
    (treemacs--on-window-config-change))
  ;; set the parameter immediately so it can take effect when `treemacs' is called programatically
  ;; alongside other window layout chaning commands that might delete it again
  (set-window-parameter (selected-window) 'no-delete-other-windows treemacs-no-delete-other-windows)

  (when treemacs-window-background-color
    (face-remap-add-relative 'default :background (car treemacs-window-background-color))
    (face-remap-add-relative 'fringe  :background (car treemacs-window-background-color))
    (face-remap-add-relative 'hl-line :background (cdr treemacs-window-background-color)))

  (when treemacs-text-scale
    (text-scale-increase treemacs-text-scale))

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
       "\
    \\
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
