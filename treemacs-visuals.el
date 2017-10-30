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
(require 'treemacs-customization)
(require 'image)
(require 'hl-line)

;; An explanation for the what and why of the icon highlighting code below:
;; Using png images in treemacs has one annoying visual flaw: they overwrite the overlay
;; used by hl-line, such that the line marked by hl-line will always show a 22x22 pixel
;; gap wherever treemacs places an icon, regardessof transparency.
;; Using xpm instead of png images is one way to work around this, but it degrades icon
;; quality to an unacceptable degree. Another way is to directly change images' :background
;; property. The backgrounds colors are derived from the current theme with `treemacs--setup-icon-highlight'
;; and saved in `treemacs--selected-icon-background' and `treemacs--not-selected-icon-background'.
;; Every icon string stores two images with the proper :background values in its properties
;; 'img-selected and 'img-unselected. The 'display property of the icon in the current line
;; is then highlighted, and the previously highlighted icon unhighlighted, by advising
;; `hl-line-highlight'. The last displayed icon is saved as a button marker in `treemacs--last-highlight'.
;; Since it is a marker in the treemacs buffer it is important for it to be reset whenever it might
;; become invalid.

(defvar treemacs--icons nil
  "Stash of all created icons.
Used by `treemacs--setup-icon-highlight' to realign icons' highlight colors
after a theme change.")

(defvar treemacs--last-highlight nil
  "The last button treemacs has highlighted.")

(defvar treemacs--not-selected-icon-background
  (let ((bg (face-attribute 'default :background nil t)))
    (if (eq 'unspecified bg)
        (prog1 "#2d2d31"
          (message "[Treemacs] Warning: coudn't find default background color, falling back on #2d2d31."))
      bg))
  "Background for non-selected icons.")

(defvar treemacs--selected-icon-background
  (let ((bg (face-attribute 'hl-line :background nil t)))
    (if (eq 'unspecified bg)
        (prog1 treemacs--not-selected-icon-background
          (message "[Treemacs] Warning: couldn't find hl-line-mode's background color, falling back on %s."
                   treemacs--not-selected-icon-background))
      bg))
  "Background for selected icons.")

(defsubst treemacs--set-img-property (image property value)
  "Set IMAGE's PROPERTY to VALUE."
  ;; the emacs26 code where this is copied from says it's for internal
  ;; use only - let's se how that goes
  (plist-put (cdr image) property value)
  value)

(defsubst treemacs--forget-last-highlight ()
  "Set `treemacs--last-highlight' to nil."
  (setq treemacs--last-highlight nil))

(defun treemacs--setup-icon-highlight ()
  "Make sure treemacs icons background aligns with hi-line's."
  (advice-add #'hl-line-highlight :after #'treemacs--update-icon-selection)
  (advice-add #'enable-theme        :after #'treemacs--setup-icon-background-colors)
  (advice-add #'disable-theme        :after #'treemacs--setup-icon-background-colors))

(defun treemacs--tear-down-icon-highlight ()
  "Tear down highlighting advice when no treemacs buffer exists anymore."
  (advice-remove #'hl-line-highlight #'treemacs--update-icon-selection)
  (advice-remove #'enable-theme        #'treemacs--setup-icon-background-colors)
  (advice-remove #'disable-theme        #'treemacs--setup-icon-background-colors)
  (treemacs--forget-last-highlight))

(defun treemacs--setup-icon-background-colors (&rest _)
  "Align icon backgrounds with current theme.
Fetch the current theme's background & hl-line colors and inject them into
`treemacs--icons'. Also called as advice after `load-theme', hence the ignored
argument."
  (setq treemacs--not-selected-icon-background (face-attribute 'default :background nil t)
        treemacs--selected-icon-background     (face-attribute 'hl-line :background nil t))
  (--each treemacs--icons
    (progn
      (treemacs--set-img-property
       (get-text-property 0 'img-selected it)
       :background treemacs--selected-icon-background)
      (treemacs--set-img-property
       (get-text-property 0 'img-unselected it)
       :background treemacs--not-selected-icon-background))))

(defun treemacs--update-icon-selection ()
  "Highlight current icon, unhighlight `treemacs--last-highlight'."
  (when (eq major-mode 'treemacs-mode)
    (condition-case e
        (-when-let (btn (treemacs--current-button))
          (let* ((pos (- (button-start btn) 2))
                 (img-selected (get-text-property pos 'img-selected)))
            (treemacs--with-writable-buffer
             (when treemacs--last-highlight
               (let* ((last-pos (- (button-start treemacs--last-highlight) 2))
                      (img-unselected (get-text-property last-pos 'img-unselected)))
                 (put-text-property last-pos (1+ last-pos) 'display img-unselected)))
             (when img-selected
               (put-text-property pos (1+ pos) 'display img-selected)
               (setq treemacs--last-highlight btn)))))
      (error
       (treemacs--log "Error on highlight, this shouldn't happen: %s" e)))))

(defmacro treemacs--setup-icon (var file-name &rest extensions)
  "Define string VAR with its display being the image created from FILE-NAME.
Insert VAR into icon-cache for each of the given file EXTENSIONS."
  `(let* ((image-unselected (create-image (f-join treemacs-dir "icons/" ,file-name) 'png nil :ascent 'center))
          (image-selected   (create-image (f-join treemacs-dir "icons/" ,file-name) 'png nil :ascent 'center)))
     (treemacs--set-img-property image-selected   :background treemacs--selected-icon-background)
     (treemacs--set-img-property image-unselected :background treemacs--not-selected-icon-background)
     (defconst ,var
       (concat (propertize " "
                           'display image-unselected
                           'img-selected image-selected
                           'img-unselected image-unselected)
               " "))
     (push ,var treemacs--icons)
     (--each (quote ,extensions) (puthash it ,var treemacs-icons-hash))))

(defun treemacs--create-icons ()
  "Create icons and put them in the icons hash."

  (defconst treemacs-icon-closed-text (propertize "+ " 'face 'treemacs-term-node-face))
  (defconst treemacs-icon-open-text   (propertize "- " 'face 'treemacs-term-node-face))

  (defvar treemacs-icon-closed)
  (defvar treemacs-icon-open)

  (if treemacs-no-images
      (setq treemacs-icon-closed treemacs-icon-closed-text
            treemacs-icon-open   treemacs-icon-open-text)

    (setq treemacs-icons-hash (make-hash-table :test #'equal))

    (treemacs--setup-icon treemacs-icon-closed-png "dir_closed.png")
    (treemacs--setup-icon treemacs-icon-open-png   "dir_open.png")
    (treemacs--setup-icon treemacs-icon-text       "txt.png")

    (treemacs--setup-icon treemacs-icon-shell      "shell.png"      "sh" "zsh" "fish")
    (treemacs--setup-icon treemacs-icon-pdf        "pdf.png"        "pdf")
    (treemacs--setup-icon treemacs-icon-c          "c.png"          "c" "h")
    (treemacs--setup-icon treemacs-icon-cpp        "cpp.png"        "cpp" "cxx" "hpp" "tpp" "cc" "hh")
    (treemacs--setup-icon treemacs-icon-haskell    "haskell.png"    "hs" "lhs" "cabal")
    (treemacs--setup-icon treemacs-icon-python     "python.png"     "py" "pyc")
    (treemacs--setup-icon treemacs-icon-markdown   "markdown.png"   "md")
    (treemacs--setup-icon treemacs-icon-rust       "rust.png"       "rs" "toml")
    (treemacs--setup-icon treemacs-icon-image      "image.png"      "jpg" "jpeg" "bmp" "svg" "png" "xpm")
    (treemacs--setup-icon treemacs-icon-emacs      "emacs.png"      "el" "elc" "org")
    (treemacs--setup-icon treemacs-icon-clojure    "clojure.png"    "clj" "cljs" "cljc")
    (treemacs--setup-icon treemacs-icon-typescript "typescript.png" "ts")
    (treemacs--setup-icon treemacs-icon-css        "css.png"        "css")
    (treemacs--setup-icon treemacs-icon-conf       "conf.png"       "properties" "conf" "config" "ini" "xdefaults" "xresources" "terminalrc")
    (treemacs--setup-icon treemacs-icon-html       "html.png"       "html" "htm")
    (treemacs--setup-icon treemacs-icon-git        "git.png"        "git" "gitignore" "gitconfig")
    (treemacs--setup-icon treemacs-icon-dart       "dart.png"       "dart")
    (treemacs--setup-icon treemacs-icon-js         "js.png"         "js" "jsx")
    (treemacs--setup-icon treemacs-icon-json       "json.png"       "json")))

(defun treemacs-define-custom-icon (icon &rest file-extensions)
  "Define a custom ICON to use for FILE-EXTENSIONS.

Note that treemacs has a very loose definition of what constitutes a file
extension - it's either everything past the last period, or just the file's full
name if there is no period. This makes it possible to match file names like
'.gitignore' and 'Makefile'.

FILE-EXTENSIONS are also not case sensitive and will be downcased before they're
inserted into `treemacs-icons-hash'."
  (push icon treemacs--icons)
  (--each file-extensions
    (puthash (downcase it)
             (concat icon " ")
             treemacs-icons-hash)))

(provide 'treemacs-visuals)

;;; treemacs-visuals.el ends here
