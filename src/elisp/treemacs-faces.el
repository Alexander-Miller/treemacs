;;; treemacs.el --- A tree style file viewer package -*- lexical-binding: t -*-

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

;; Treemacs faces.

;;; Code:

(defface treemacs-directory-face
  '((t :inherit font-lock-function-name-face))
  "Face used by treemacs for directories."
  :group 'treemacs-faces)

(defface treemacs-directory-collapsed-face
  '((t :inherit treemacs-directory-face))
  "Face used by treemacs for collapsed directories.
This is the face used for the collapsed part of nodes, so
if the node is 'foo/bar/baz', the face is used for 'foo/bar/'.

Using this face is incompatible with `treemacs-git-mode' (exept for the simple
variant), so it will only be used if git-mode is disabled or set to simple."
  :group 'treemacs-faces)

(defface treemacs-file-face
  '((t :inherit default))
  "Face used by treemacs for files."
  :group 'treemacs-faces)

(defface treemacs-root-face
  '((t :inherit font-lock-constant-face :underline t :bold t :height 1.2))
  "Face used by treemacs for its root nodes."
  :group 'treemacs-faces)

(defface treemacs-root-unreadable-face
  '((t :inherit treemacs-root-face :strike-through t))
  "Face used by treemacs for unreadable root nodes."
  :group 'treemacs-faces)

(defface treemacs-root-remote-face
  '((t :inherit (font-lock-function-name-face treemacs-root-face)))
  "Face used by treemacs for remote (Tramp) root nodes."
  :group 'treemacs-faces)

(defface treemacs-root-remote-unreadable-face
  '((t :inherit treemacs-root-unreadable-face))
  "Face used by treemacs for unreadable remote (Tramp) root nodes."
  :group 'treemacs-faces)

(defface treemacs-root-remote-disconnected-face
  '((t :inherit (warning treemacs-root-remote-face)))
  "Face used by treemacs for disconnected remote (Tramp) root nodes."
  :group 'treemacs-faces)

(defface treemacs-term-node-face
  '((t :inherit font-lock-string-face))
  "Face used by treemacs in the terminal for directory node symbols."
  :group 'treemacs-faces)

(defface treemacs-git-unmodified-face
  '((t :inherit treemacs-file-face))
  "Face used for unmodified files."
  :group 'treemacs-faces)

(defface treemacs-git-modified-face
  '((t :inherit font-lock-variable-name-face))
  "Face used for modified files."
  :group 'treemacs-faces)

(defface treemacs-git-renamed-face
  '((t :inherit font-lock-doc-face))
  "Face used for renamed files."
  :group 'treemacs-faces)

(defface treemacs-git-ignored-face
  '((t :inherit font-lock-comment-face))
  "Face for ignored files."
  :group 'treemacs-faces)

(defface treemacs-git-untracked-face
  '((t :inherit font-lock-string-face))
  "Face for untracked files."
  :group 'treemacs-faces)

(defface treemacs-git-added-face
  '((t :inherit font-lock-type-face))
  "Face for newly added files."
  :group 'treemacs-faces)

(defface treemacs-git-conflict-face
  '((t :inherit error))
  "Face for conflicting files."
  :group 'treemacs-faces)

(defface treemacs-tags-face
  '((t :inherit font-lock-builtin-face))
  "Face for tags."
  :group 'treemacs-faces)

(defface treemacs-help-title-face
  `((t :inherit ,(if (facep 'spacemacs-transient-state-title-face)
                     'spacemacs-transient-state-title-face
                   'font-lock-constant-face)))
  "Face for the title of the helpful hydra."
  :group 'treemacs-faces)

(defface treemacs-help-column-face
  '((t :inherit font-lock-keyword-face :underline t))
  "Face for column headers of the helpful hydra."
  :group 'treemacs-faces)

(defface treemacs-on-failure-pulse-face
  '((t :foreground "#111111" :background "#ab3737" :extend t))
  "Pulse face used when an error occurs or an action fails."
  :group 'treemacs-faces)

(defface treemacs-on-success-pulse-face
  '((t :foreground "#111111" :background "#669966" :extend t))
  "Pulse face used to signal a successful action."
  :group 'treemacs-faces)

(defface treemacs-fringe-indicator-face
  `((t :foreground ,(face-background 'cursor nil t)))
  "Face for the fringe indicator."
  :group 'treemacs-faces)

(defface treemacs-header-button-face
  '((t :inherit 'font-lock-keyword-face))
  "Face used for header buttons.
Applies to buttons like
 - `treemacs-header-close-button'
 - `treemacs-header-projects-button'
 - `treemacs-header-workspace-button'"
  :group 'treemacs-faces)

(defface treemacs-peek-mode-indicator-face
  '((t :background "#669966"))
  "Face used to indicate that `treemacs-peek-mode' is enabled."
  :group 'treemacs-faces)

(defface treemacs-marked-file-face
  '((t :foreground "#F0C674" :background "#AB3737" :bold t))
  "Face for files marked by treemacs."
  :group 'treemacs-faces)

(provide 'treemacs-faces)

;;; treemacs-faces.el ends here
