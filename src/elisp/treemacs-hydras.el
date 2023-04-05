;;; treemacs.el --- A tree style file viewer package -*- lexical-binding: t -*-

;; Copyright (C) 2023 Alexander Miller

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

;; Definition for the Helpful Hydras.

;; NOTE: This module is lazy-loaded.

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'treemacs-logging)
(require 'treemacs-scope)
(require 'treemacs-interface)
(require 'treemacs-bookmarks)
(eval-when-compile
  (require 'treemacs-macros))

(treemacs-import-functions-from "treemacs"
  treemacs-edit-workspaces
  treemacs-version)

(treemacs-import-functions-from "treemacs-file-management"
  treemacs-rename-file
  treemacs-create-file
  treemacs-create-dir
  treemacs-copy-file
  treemacs-move-file
  treemacs-delete-file
  treemacs-bulk-file-actions)

(treemacs-import-functions-from "treemacs-hydras"
  treemacs--common-helpful-hydra/body
  treemacs--advanced-helpful-hydra/body)

(treemacs-import-functions-from "treemacs-peek-mode"
  treemacs-peek-mode)

(treemacs-import-functions-from "treemacs-header-line"
  treemacs-indicate-top-scroll-mode)

(treemacs-import-functions-from "treemacs-git-commit-diff-mode"
  treemacs-git-commit-diff-mode)

(cl-defun treemacs--find-keybind (func &optional (pad 8))
  "Find the keybind for FUNC in treemacs.
Return of cons of the key formatted for inclusion in the hydra string, including
a minimum PAD width for alignment, and the key itself for the hydra heads.
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

;;;###autoload
(defun treemacs-common-helpful-hydra ()
  "Summon a helpful hydra to show you the treemacs keymap.

This hydra will show the most commonly used keybinds for treemacs.  For the more
advanced (probably rarely used keybinds) see `treemacs-advanced-helpful-hydra'.

The keybinds shown in this hydra are not static, but reflect the actual
keybindings currently in use (including evil mode).  If the hydra is unable to
find the key a command is bound to it will show a blank instead."
  (interactive)
  (-if-let (b (treemacs-get-local-buffer))
      (with-current-buffer b
        (let*
            ((title              (format (propertize "Treemacs %s Common Helpful Hydra" 'face 'treemacs-help-title-face) (treemacs-version)))
             (adv-hint           (format "%s %s"
                                         (propertize "For advanced keybinds see" 'face 'treemacs-help-title-face)
                                         (propertize "treemacs-advanced-helpful-hydra" 'face 'font-lock-function-name-face)))
             (column-nav         (propertize "Navigation" 'face 'treemacs-help-column-face))
             (column-nodes       (propertize "Opening Nodes" 'face 'treemacs-help-column-face))
             (column-toggles     (propertize "Toggles " 'face 'treemacs-help-column-face))
             (column-projects    (propertize "Projects" 'face 'treemacs-help-column-face))
             (key-adv-hydra      (treemacs--find-keybind #'treemacs-advanced-helpful-hydra))
             (key-root-up        (treemacs--find-keybind #'treemacs-root-up))
             (key-root-down      (treemacs--find-keybind #'treemacs-root-down))
             (key-next-line      (treemacs--find-keybind #'treemacs-next-line))
             (key-prev-line      (treemacs--find-keybind #'treemacs-previous-line))
             (key-next-neighbour (treemacs--find-keybind #'treemacs-next-neighbour))
             (key-prev-neighbour (treemacs--find-keybind #'treemacs-previous-neighbour))
             (key-goto-parent    (treemacs--find-keybind #'treemacs-goto-parent-node))
             (key-down-next-w    (treemacs--find-keybind #'treemacs-next-line-other-window))
             (key-up-next-w      (treemacs--find-keybind #'treemacs-previous-line-other-window))
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
             (key-open-close     (treemacs--find-keybind #'treemacs-visit-node-close-treemacs))
             (key-close-above    (treemacs--find-keybind #'treemacs-collapse-parent-node))
             (key-follow-mode    (treemacs--find-keybind #'treemacs-follow-mode))
             (key-header-mode    (treemacs--find-keybind #'treemacs-indicate-top-scroll-mode))
             (key-fringe-mode    (treemacs--find-keybind #'treemacs-fringe-indicator-mode))
             (key-fwatch-mode    (treemacs--find-keybind #'treemacs-filewatch-mode))
             (key-commit-diff    (treemacs--find-keybind #'treemacs-git-commit-diff-mode))
             (key-git-mode       (treemacs--find-keybind #'treemacs-git-mode))
             (key-show-dotfiles  (treemacs--find-keybind #'treemacs-toggle-show-dotfiles))
             (key-indent-guide   (treemacs--find-keybind #'treemacs-indent-guide-mode))
             (key-show-gitignore (treemacs--find-keybind #'treemacs-hide-gitignored-files-mode))
             (key-toggle-width   (treemacs--find-keybind #'treemacs-toggle-fixed-width))
             (key-add-project    (treemacs--find-keybind #'treemacs-add-project-to-workspace 12))
             (key-remove-project (treemacs--find-keybind #'treemacs-remove-project-from-workspace 12))
             (key-rename-project (treemacs--find-keybind #'treemacs-rename-project 12))
             (hydra-str
              (format
               "
%s
%s (%s)

%s              ^^^^^^^^│ %s              ^^^^^^^^^^^│ %s                     ^^^^^^│  %s
――――――――――――――――――――――――┼――――――――――――――――――――――――――――┼――――――――――――――――――――――――――――――┼――――――――――――――――――――――――――
%s next line        ^^^^│ %s dwim TAB            ^^^^│ %s follow mode           ^^^^│ %s add project
%s prev line        ^^^^│ %s dwim RET            ^^^^│ %s filewatch mode        ^^^^│ %s remove project
%s next neighbour   ^^^^│ %s open no split       ^^^^│ %s git mode              ^^^^│ %s rename project
%s prev neighbour   ^^^^│ %s open horizontal     ^^^^│ %s show dotfiles         ^^^^│
%s goto parent      ^^^^│ %s open vertical       ^^^^│ %s show gitignored files ^^^^│
%s down next window ^^^^│ %s open ace            ^^^^│ %s resizability          ^^^^│
%s up next window   ^^^^│ %s open ace horizontal ^^^^│ %s fringe indicator      ^^^^│
%s root up          ^^^^│ %s open ace vertical   ^^^^│ %s indent guide          ^^^^│
%s root down        ^^^^│ %s open mru window     ^^^^│ %s top scroll indicator  ^^^^│
                        │ %s open externally     ^^^^│ %s git commit difference ^^^^│
                        │ %s open close treemacs ^^^^│                              │
                        │ %s close parent        ^^^^│                              │
"
               title
               adv-hint (car (s-split":" (car key-adv-hydra)))
               column-nav               column-nodes          column-toggles           column-projects
               (car key-next-line)      (car key-tab)         (car key-follow-mode)    (car key-add-project)
               (car key-prev-line)      (car key-ret)         (car key-fwatch-mode)    (car key-remove-project)
               (car key-next-neighbour) (car key-open)        (car key-git-mode)       (car key-rename-project)
               (car key-prev-neighbour) (car key-open-horiz)  (car key-show-dotfiles)
               (car key-goto-parent)    (car key-open-vert)   (car key-show-gitignore)
               (car key-down-next-w)    (car key-open-ace)    (car key-toggle-width)
               (car key-up-next-w)      (car key-open-ace-h)  (car key-fringe-mode)
               (car key-root-up)        (car key-open-ace-v)  (car key-indent-guide)
               (car key-root-down)      (car key-open-mru)    (car key-header-mode)
                                        (car key-open-ext)    (car key-commit-diff)
                                        (car key-open-close)
                                        (car key-close-above))))
          (eval
           `(defhydra treemacs--common-helpful-hydra (:exit nil :hint nil :columns 4)
              ,hydra-str
              (,(cdr key-adv-hydra)      #'treemacs-advanced-helpful-hydra :exit t)
              (,(cdr key-next-line)      #'treemacs-next-line)
              (,(cdr key-prev-line)      #'treemacs-previous-line)
              (,(cdr key-root-up)        #'treemacs-root-up)
              (,(cdr key-root-down)      #'treemacs-root-down)
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
              (,(cdr key-open-close)     #'treemacs-visit-node-close-treemacs)
              (,(cdr key-close-above)    #'treemacs-collapse-parent-node)
              (,(cdr key-follow-mode)    #'treemacs-follow-mode)
              (,(cdr key-header-mode)    #'treemacs-indicate-top-scroll-mode)
              (,(cdr key-show-dotfiles)  #'treemacs-toggle-show-dotfiles)
              (,(cdr key-show-gitignore) #'treemacs-hide-gitignored-files-mode)
              (,(cdr key-toggle-width)   #'treemacs-toggle-fixed-width)
              (,(cdr key-commit-diff)    #'treemacs-git-commit-diff-mode)
              (,(cdr key-fringe-mode)    #'treemacs-fringe-indicator-mode)
              (,(cdr key-indent-guide)   #'treemacs-indent-guide-mode)
              (,(cdr key-git-mode)       #'treemacs-git-mode)
              (,(cdr key-fwatch-mode)    #'treemacs-filewatch-mode)
              (,(cdr key-add-project)    #'treemacs-add-project-to-workspace)
              (,(cdr key-remove-project) #'treemacs-remove-project-from-workspace)
              (,(cdr key-rename-project) #'treemacs-rename-project)
              ("<escape>" nil "Exit"))))
        (treemacs--common-helpful-hydra/body))
    (treemacs-log-failure "The helpful hydra cannot be summoned without an existing treemacs buffer.")))

(defalias 'treemacs-helpful-hydra #'treemacs-common-helpful-hydra)

;;;###autoload
(defun treemacs-advanced-helpful-hydra ()
  "Summon a helpful hydra to show you the treemacs keymap.

This hydra will show the more advanced (rarely used) keybinds for treemacs.  For
the more commonly used keybinds see `treemacs-common-helpful-hydra'.

The keybinds shown in this hydra are not static, but reflect the actual
keybindings currently in use (including evil mode).  If the hydra is unable to
find the key a command is bound to it will show a blank instead."
  (interactive)
  (-if-let (b (treemacs-get-local-buffer))
      (with-current-buffer b
        (let*
            ((title              (format (propertize "Treemacs %s Advanced Helpful Hydra" 'face 'treemacs-help-title-face) (treemacs-version)))
             (column-files       (propertize "File Management" 'face 'treemacs-help-column-face))
             (column-ws          (propertize "Workspaces" 'face 'treemacs-help-column-face))
             (column-misc        (propertize "Misc." 'face 'treemacs-help-column-face))
             (column-window      (propertize "Other Window" 'face 'treemacs-help-column-face))
             (common-hint        (format "%s %s"
                                         (propertize "For common keybinds see" 'face 'treemacs-help-title-face)
                                         (propertize "treemacs-common-helpful-hydra" 'face 'font-lock-function-name-face)))
             (key-common-hydra   (treemacs--find-keybind #'treemacs-common-helpful-hydra))
             (key-create-file    (treemacs--find-keybind #'treemacs-create-file))
             (key-create-dir     (treemacs--find-keybind #'treemacs-create-dir))
             (key-rename         (treemacs--find-keybind #'treemacs-rename-file))
             (key-delete         (treemacs--find-keybind #'treemacs-delete-file))
             (key-copy-file      (treemacs--find-keybind #'treemacs-copy-file))
             (key-move-file      (treemacs--find-keybind #'treemacs-move-file))
             (key-refresh        (treemacs--find-keybind #'treemacs-refresh))
             (key-set-width      (treemacs--find-keybind #'treemacs-set-width))
             (key-copy-path-abs  (treemacs--find-keybind #'treemacs-copy-absolute-path-at-point))
             (key-copy-path-rel  (treemacs--find-keybind #'treemacs-copy-relative-path-at-point))
             (key-copy-root      (treemacs--find-keybind #'treemacs-copy-project-path-at-point))
             (key-resort         (treemacs--find-keybind #'treemacs-resort))
             (key-bookmark       (treemacs--find-keybind #'treemacs-add-bookmark))
             (key-edit-ws        (treemacs--find-keybind #'treemacs-edit-workspaces 12))
             (key-create-ws      (treemacs--find-keybind #'treemacs-create-workspace 12))
             (key-remove-ws      (treemacs--find-keybind #'treemacs-remove-workspace 12))
             (key-rename-ws      (treemacs--find-keybind #'treemacs-rename-workspace 12))
             (key-switch-ws      (treemacs--find-keybind #'treemacs-switch-workspace 12))
             (key-next-ws        (treemacs--find-keybind #'treemacs-next-workspace 12))
             (key-fallback-ws    (treemacs--find-keybind #'treemacs-set-fallback-workspace 12))
             (key-peek           (treemacs--find-keybind #'treemacs-peek-mode 10))
             (key-line-down      (treemacs--find-keybind #'treemacs-next-line-other-window 10))
             (key-line-up        (treemacs--find-keybind #'treemacs-previous-line-other-window 10))
             (key-page-down      (treemacs--find-keybind #'treemacs-next-page-other-window 10))
             (key-page-up        (treemacs--find-keybind #'treemacs-previous-page-other-window 10))
             (key-bulk-actions   (treemacs--find-keybind #'treemacs-bulk-file-actions))
             (hydra-str
              (format
               "
%s
%s (%s)

%s      ^^^^^^^^^^^^^│ %s                  ^^^^^^^^│ %s       ^^^^^^^^^^│ %s
―――――――――――――――――――――┼―――――――――――――――――――――――――――――┼――――――――――――――――――――┼――――――――――――――――――――――
 %s create file  ^^^^│ %s Edit Workspaces  ^^^^^^^^│ %s peek      ^^^^^^│ %s refresh
 %s create dir   ^^^^│ %s Create Workspace ^^^^^^^^│ %s line down ^^^^^^│ %s (re)set width
 %s rename       ^^^^│ %s Remove Workspace ^^^^^^^^│ %s line up   ^^^^^^│ %s copy path absolute
 %s delete       ^^^^│ %s Rename Workspace ^^^^^^^^│ %s page down ^^^^^^│ %s copy path relative
 %s copy         ^^^^│ %s Switch Workspace ^^^^^^^^│ %s page up   ^^^^^^│ %s copy root path
 %s move         ^^^^│ %s Next Workspace   ^^^^^^^^│                    │ %s re-sort
 %s bulk actions ^^^^│ %s Set Fallback     ^^^^^^^^│                    │ %s bookmark

"
               title
               common-hint (car (s-split":" (car key-common-hydra)))
               column-files           column-ws             column-window       column-misc
               (car key-create-file)  (car key-edit-ws)     (car key-peek)      (car key-refresh)
               (car key-create-dir)   (car key-create-ws)   (car key-line-down) (car key-set-width)
               (car key-rename)       (car key-remove-ws)   (car key-line-up)   (car key-copy-path-abs)
               (car key-delete)       (car key-rename-ws)   (car key-page-down) (car key-copy-path-rel)
               (car key-copy-file)    (car key-switch-ws)   (car key-page-up)   (car key-copy-root)
               (car key-move-file)    (car key-next-ws)                         (car key-resort)
               (car key-bulk-actions) (car key-fallback-ws)                     (car key-bookmark))))
          (eval
           `(defhydra treemacs--advanced-helpful-hydra (:exit nil :hint nil :columns 3)
              ,hydra-str
              (,(cdr key-common-hydra)   #'treemacs-common-helpful-hydra :exit t)
              (,(cdr key-create-file)    #'treemacs-create-file)
              (,(cdr key-create-dir)     #'treemacs-create-dir)
              (,(cdr key-rename)         #'treemacs-rename-file)
              (,(cdr key-delete)         #'treemacs-delete-file)
              (,(cdr key-copy-file)      #'treemacs-copy-file)
              (,(cdr key-move-file)      #'treemacs-move-file)
              (,(cdr key-refresh)        #'treemacs-refresh)
              (,(cdr key-set-width)      #'treemacs-set-width)
              (,(cdr key-copy-path-rel)  #'treemacs-copy-absolute-path-at-point)
              (,(cdr key-copy-path-abs)  #'treemacs-copy-relative-path-at-point)
              (,(cdr key-copy-root)      #'treemacs-copy-project-path-at-point)
              (,(cdr key-resort)         #'treemacs-resort)
              (,(cdr key-bookmark)       #'treemacs-add-bookmark)
              (,(cdr key-edit-ws)        #'treemacs-edit-workspaces)
              (,(cdr key-create-ws)      #'treemacs-create-workspace)
              (,(cdr key-remove-ws)      #'treemacs-remove-workspace)
              (,(cdr key-rename-ws)      #'treemacs-rename-workspace)
              (,(cdr key-switch-ws)      #'treemacs-switch-workspace)
              (,(cdr key-next-ws)        #'treemacs-next-workspace)
              (,(cdr key-fallback-ws)    #'treemacs-set-fallback-workspace)
              (,(cdr key-peek)           #'treemacs-peek-mode)
              (,(cdr key-line-down)      #'treemacs-next-line-other-window)
              (,(cdr key-line-up)        #'treemacs-previous-line-other-window)
              (,(cdr key-page-down)      #'treemacs-next-page-other-window)
              (,(cdr key-page-up)        #'treemacs-previous-previous-other-window)
              (,(cdr key-bulk-actions)   #'treemacs-bulk-file-actions)
              ("<escape>" nil "Exit"))))
        (treemacs--advanced-helpful-hydra/body))
    (treemacs-log-failure "The helpful hydra cannot be summoned without an existing treemacs buffer.")))

(provide 'treemacs-hydras)

;;; treemacs-hydras.el ends here
