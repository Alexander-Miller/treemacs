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
;;; Faces extracted into their own file to reduce clutter.

;;; Code:

(defface treemacs-directory-face
  '((t :inherit font-lock-function-name-face))
  "Face used by treemacs for directories."
  :group 'treemacs-faces)

(defface treemacs-file-face
  '((t :inherit default))
  "Face used by treemacs for files."
  :group 'treemacs-faces)

(defface treemacs-header-face
  '((t :inherit font-lock-constant-face :underline t :size 1.4))
  "Face used by treemacs for its header."
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

(provide 'treemacs-faces)

;;; treemacs-faces.el ends here
