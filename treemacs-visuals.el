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
;;; Handling of visuals in general and icons in particular.

;;; Code:

(require 'treemacs-impl)

(defmacro treemacs--setup-icon (var file-name &rest extensions)
  "Define string VAR with its display being the image created from FILE-NAME.
Insert VAR into icon-cache for each of the given file EXTENSIONS."
  `(let ((image (create-image (f-join treemacs-dir "icons/" ,file-name)
                              'png nil :ascent 'center)))
     (defconst ,var
       (concat (propertize " " 'display image) " "))
     (--each (quote ,extensions) (puthash it ,var treemacs-icons-hash))))

(defun treemacs--create-icons ()
  "Create icons and put them in the icons hash."

  (setq treemacs-icons-hash (make-hash-table :test #'equal))

  (treemacs--setup-icon treemacs-icon-closed-png "dir_closed.png")
  (treemacs--setup-icon treemacs-icon-open-png   "dir_open.png")
  (treemacs--setup-icon treemacs-icon-text       "txt.png")

  (treemacs--setup-icon treemacs-icon-shell      "shell.png"      "sh" "zsh" "fish")
  (treemacs--setup-icon treemacs-icon-pdf        "pdf.png"        "pdf")
  (treemacs--setup-icon treemacs-icon-c          "c.png"          "c" "h")
  (treemacs--setup-icon treemacs-icon-cpp        "cpp.png"        "cpp" "hpp")
  (treemacs--setup-icon treemacs-icon-haskell    "haskell.png"    "hs")
  (treemacs--setup-icon treemacs-icon-python     "python.png"     "py" "pyc")
  (treemacs--setup-icon treemacs-icon-markdown   "markdown.png"   "md")
  (treemacs--setup-icon treemacs-icon-rust       "rust.png"       "rs" "toml")
  (treemacs--setup-icon treemacs-icon-image      "image.png"      "jpg" "jpeg" "bmp" "svg" "png" "xpm")
  (treemacs--setup-icon treemacs-icon-emacs      "emacs.png"      "el" "elc" "org")
  (treemacs--setup-icon treemacs-icon-clojure    "clojure.png"    "clj" "cljs" "cljc")
  (treemacs--setup-icon treemacs-icon-typescript "typescript.png" "ts")
  (treemacs--setup-icon treemacs-icon-css        "css.png"        "css")
  (treemacs--setup-icon treemacs-icon-conf       "conf.png"       "conf" "config" "ini" "xdefaults" "xresources")
  (treemacs--setup-icon treemacs-icon-html       "html.png"       "html" "htm")
  (treemacs--setup-icon treemacs-icon-git        "git.png"        "git" "gitignore" "gitconfig")
  (treemacs--setup-icon treemacs-icon-dart       "dart.png"       "dart")
  (treemacs--setup-icon treemacs-icon-js         "js.png"         "js" "jsx")
  (treemacs--setup-icon treemacs-icon-json       "json.png"       "json")

  (defconst treemacs-icon-closed-text (propertize "+ " 'face 'treemacs-term-node-face))
  (defconst treemacs-icon-open-text   (propertize "- " 'face 'treemacs-term-node-face))
  (with-no-warnings
    (defvar treemacs-icon-closed treemacs-icon-closed-png)
    (defvar treemacs-icon-open treemacs-icon-open-png)))

(defun treemacs-define-custom-icon (icon &rest file-extensions)
  "Define a custom ICON to use for FILE-EXTENSIONS.

Note that treemacs has a very loose definition of what constitutes a file
extension - it's either everything past the last period, or just the file's full
name if there is no period. This makes it possible to match file names like
'.gitignore' and 'Makefile'.

FILE-EXTENSIONS are also not case sensitive and will be downcased before they're
inserted into `treemacs-icons-hash'."
  (--each file-extensions
    (puthash (downcase it)
             (concat icon " ")
             treemacs-icons-hash)))

(provide 'treemacs-visuals)

;;; treemacs-visuals.el ends here
