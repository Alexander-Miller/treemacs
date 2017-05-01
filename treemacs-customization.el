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
;;; Custom options extracted into their own file to reduce clutter.

;;; Code:

(defgroup treemacs nil
  "A major mode for displaying the file system in a tree layout."
  :group 'files
  :prefix "treemacs-"
  :link '(url-link :tag "Repository" "https://github.com/Alexander-Miller/treemacs"))

(defgroup treemacs-faces nil
  "Faces for treemacs' syntax highlighting."
  :group 'treemacs
  :group 'faces)

(defgroup treemacs-configuration nil
  "Treemacs configuration options."
  :group 'treemacs
  :prefix "treemacs-")

(defcustom treemacs-indentation 2
  "The number of spaces each level is indented in the tree."
  :type 'integer
  :group 'treemacs-configuration)

(defcustom treemacs-width 35
  "Width of the treemacs buffer."
  :type 'integer
  :group 'treemacs-configuration)

(defcustom treemacs-show-hidden-files t
  "Dotfiles will be shown if this is set to t and be hidden otherwise."
  :type 'boolean
  :group 'treemacs-configuration)

(defcustom treemacs-follow-after-init nil
  "When t always run `treemacs-follow' after building a treemacs-buffer.

A treemacs buffer is built when after calling `treemacs-init' or
`treemacs-projectle-init'. This will ignore `treemacs-follow-mode'."
  :type 'boolean
  :group 'treemacs-configuration)

(defcustom treemacs-header-function 'treemacs--create-header
  "The function which is used to create the header string for treemacs buffers.
Treemacs offers two builtin header creators:
1) `treemacs--create-header' (the default), which will simply output the current
   treemacs root.
2) `treemacs--create-header-projectile', which will first try to find the name of
   the current projectile project and fall back on `treemacs--create-header' if
   no project name is found.
Other than these two functions this value may be made to use any custom function
which takes as input a string (the absolute path of the current treemacs root)
and outputs the string header to be inserted in the treemacs buffer."
  :type 'function
  :group 'treemacs-configuration)

(defcustom treemacs-icons-hash (make-hash-table :test 'equal)
  "Hash table containing a mapping of icons onto file extensions."
  :type 'plist
  :group 'treemacs-configuration)

(defcustom treemacs-be-evil nil
  "When t use evil keys for navigation (j/k instead of n/p)."
  :type 'boolean
  :group 'treemacs-configuration)

(defcustom treemacs-git-integration nil
  "When t use different faces for files' different git states."
  :type 'boolean
  :group 'treemacs-configuration)

(defcustom treemacs-dotfiles-regex (rx bol "." (1+ any))
  "Files matching this regular expression count as dotfiles."
  :type 'regexp
  :group 'treemacs-configuration)

(defcustom treemacs-change-root-without-asking nil
  "When t don't ask to change the root when calling `treemacs-find-file'."
  :type 'boolean
  :group 'treemacs-configuration)

(defcustom treemacs--never-persist nil
  "When t treemacs will never persist its state.
By default treemacs' state is written to disk in `treemacs--persist-file' if it
detects a session saving mechanism like desktop save mode so it can be restored
on the next launch."
  :type 'boolean
  :group 'treemacs-configuration)

(provide 'treemacs-customization)

;;; treemacs-customization.el ends here
