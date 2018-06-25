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
;;; Most of everything related to icons is handled here. Specifically the
;;; definition, instantiation, customization, resizing and resetting of icons.

;;; Code:

(require 'image)
(require 'dash)
(require 's)
(require 'treemacs-visuals)
(require 'treemacs-workspaces)
(eval-and-compile (require 'treemacs-macros))

(defconst treemacs--image-creation-impossible
  (condition-case e
      (progn (create-image "" 'xpm) nil)
    (error e))
  "This variable is a non-nil error value when Emacs is unable to create images.
In this scenario (usually caused by running Emacs without a graphical
environment) treemacs will not create any of its icons and will be forced to
permanently use its simple string icon fallack.")

(defvar treemacs-icons-hash (make-hash-table :size 200 :test #'equal)
  "Hash table containing a mapping of icons onto file extensions.")

(defvar treemacs--icon-size nil
  "Size in pixels icons will be resized to.
See also `treemacs-resize-icons'.")

(defvar treemacs--created-icons nil
  "Stash of all created icons.
Needed by `treemacs--setup-icon-highlight' to realign icons' highlight colors
after a theme change.")

(defvar treemacs--default-icons-alist nil
  "Stores the default values of the directory and tag icons.
Maps icons' names as symbols to their values, so that they can be queried
via `assq'.")

(defsubst treemacs--created-icons ()
  "Return `treemacs--created-icons'."
  treemacs--created-icons)

(defun treemacs--create-image (file-path)
  "Load image from FILE-PATH and size it based on `treemacs--icon-size'."
  (-let [(height width) `(,treemacs--icon-size ,treemacs--icon-size)]
    ;; special case for the root icon which is unique in being 20x26 pixels large
    (when (and (integerp treemacs--icon-size)
               (s-ends-with? "root.png" file-path))
      (setq width (round (* width 0.9090))
            height (round (* height 1.1818))))
    (if (and (integerp treemacs--icon-size) (image-type-available-p 'imagemagick))
        (create-image file-path 'imagemagick nil :ascent 'center :width width :height height)
      ;; interning the extension lets up pass both png and xpm icons
      (create-image file-path (intern (f-ext file-path)) nil :ascent 'center))))

(defmacro treemacs--setup-icon (var file-name &rest extensions)
  "Define VAR with its display property being the image created from FILE-NAME.
Insert VAR into `treemacs-icon-hash' for each of the given file EXTENSIONS."
  `(progn
     (defvar ,var nil)
     (let* ((image-unselected (treemacs--create-image (f-join treemacs-dir "icons/" ,file-name)))
             (image-selected   (treemacs--create-image (f-join treemacs-dir "icons/" ,file-name))))
        (treemacs--set-img-property image-selected   :background treemacs--selected-icon-background)
        (treemacs--set-img-property image-unselected :background treemacs--not-selected-icon-background)
        (setq ,var
              (concat (propertize " "
                                  'display image-unselected
                                  'img-selected image-selected
                                  'img-unselected image-unselected)
                      " "))
        (push ,var treemacs--created-icons)
        (--each (quote ,extensions) (ht-set! treemacs-icons-hash it ,var))
        ,var)))

(defmacro treemacs--define-icon-with-default (var val)
  "Define a VAR with value VAL.
Remember the value in `treemacs--default-icons-alist'."
  `(progn
     (defvar ,var ,val)
     (push (cons ',var ,val) treemacs--default-icons-alist)))

;; setup of all the icons not based on files' extensions
;; the exact values will be set later

(defvar treemacs-icon-root            "")
(defvar treemacs-icon-closed          "")
(defvar treemacs-icon-open            "")
(defvar treemacs-icon-fallback        "")
(defvar treemacs-icon-tag-leaf        "")
(defvar treemacs-icon-tag-node-closed "")
(defvar treemacs-icon-tag-node-open   "")

;; each of these icons takes one of 2 possible values - a 'png' variant for Emacsen
;; capable of displaying images and a 'txt' variant as fallback for TUI frames

(defvar treemacs-icon-root-text            "")
(defvar treemacs-icon-closed-text          (propertize "+ " 'face 'treemacs-term-node-face))
(defvar treemacs-icon-open-text            (propertize "- " 'face 'treemacs-term-node-face))
(defvar treemacs-icon-fallback-text        (propertize "  " 'face 'font-lock-keyword-face))
(defvar treemacs-icon-tag-leaf-text        (propertize "• " 'face 'font-lock-constant-face))
(defvar treemacs-icon-tag-node-closed-text (propertize "▸ " 'face 'font-lock-string-face))
(defvar treemacs-icon-tag-node-open-text   (propertize "▾ " 'face 'font-lock-string-face))

(defvar treemacs-icon-root-png            "")
(defvar treemacs-icon-closed-png          "")
(defvar treemacs-icon-open-png            "")
(defvar treemacs-icon-tag-leaf-png        "")
(defvar treemacs-icon-tag-node-closed-png "")
(defvar treemacs-icon-tag-node-open-png   "")
(defvar treemacs-icon-text                "")

(defmacro treemacs--set-icon-save-default (&rest key-values)
  "Pass KEY-VALUES to `setq'.
Also save the assignments in `treemacs--default-icons-alist'."
  (cl-assert (= 0 (% (length key-values) 2)))
  (let ((key-vals (copy-sequence key-values))
        (assignments))
    (while key-vals
      (-let [(_ value . rest) key-vals]
        (setq key-vals rest)
        (push `(push (cons (quote ,value) ,value) treemacs--default-icons-alist) assignments)))
    `(progn
       (setq ,@key-values)
       ,@assignments)))

(defsubst treemacs--setup-tui-icons ()
  "Will set textual icon values for non-file icons."
  (treemacs--set-icon-save-default
   treemacs-icon-closed          treemacs-icon-closed-text
   treemacs-icon-open            treemacs-icon-open-text
   treemacs-icon-root            treemacs-icon-root-text
   treemacs-icon-fallback        treemacs-icon-fallback-text
   treemacs-icon-tag-leaf        treemacs-icon-tag-leaf-text
   treemacs-icon-tag-node-closed treemacs-icon-tag-node-closed-text
   treemacs-icon-tag-node-open   treemacs-icon-tag-node-open-text))

(defsubst treemacs--setup-gui-icons ()
  "Will set graphical values for non-file icons.
Will also fill `treemacs-icons-hash' with graphical file icons."
  ;; first setup png variants of the non-file icons
  (treemacs--setup-icon treemacs-icon-closed-png          "dir_closed.png")
  (treemacs--setup-icon treemacs-icon-open-png            "dir_open.png")
  (treemacs--setup-icon treemacs-icon-root-png            "root.png")
  (treemacs--setup-icon treemacs-icon-tag-leaf-png        "tags-leaf.xpm")
  (treemacs--setup-icon treemacs-icon-tag-node-closed-png "tags-closed.xpm")
  (treemacs--setup-icon treemacs-icon-tag-node-open-png   "tags-open.xpm")
  (treemacs--setup-icon treemacs-icon-text                "txt.png")

  ;; then assign them and save the default value
  (treemacs--set-icon-save-default
   treemacs-icon-fallback        treemacs-icon-text
   treemacs-icon-closed          treemacs-icon-closed-png
   treemacs-icon-open            treemacs-icon-open-png
   treemacs-icon-root            treemacs-icon-root-png
   treemacs-icon-tag-leaf        treemacs-icon-tag-leaf-png
   treemacs-icon-tag-node-closed treemacs-icon-tag-node-closed-png
   treemacs-icon-tag-node-open   treemacs-icon-tag-node-open-png)

  ;; then create and hash all the extension-based icons
  (treemacs--setup-icon treemacs-icon-yaml       "yaml.png"       "yml" "yaml")
  (treemacs--setup-icon treemacs-icon-shell      "shell.png"      "sh" "zsh" "fish")
  (treemacs--setup-icon treemacs-icon-pdf        "pdf.png"        "pdf")
  (treemacs--setup-icon treemacs-icon-c          "c.png"          "c" "h")
  (treemacs--setup-icon treemacs-icon-cpp        "cpp.png"        "cpp" "cxx" "hpp" "tpp" "cc" "hh")
  (treemacs--setup-icon treemacs-icon-haskell    "haskell.png"    "hs" "lhs" "cabal")
  (treemacs--setup-icon treemacs-icon-python     "python.png"     "py" "pyc")
  (treemacs--setup-icon treemacs-icon-markdown   "markdown.png"   "md")
  (treemacs--setup-icon treemacs-icon-rust       "rust.png"       "rs")
  (treemacs--setup-icon treemacs-icon-image      "image.png"      "jpg" "jpeg" "bmp" "svg" "png" "xpm")
  (treemacs--setup-icon treemacs-icon-emacs      "emacs.png"      "el" "elc" "org")
  (treemacs--setup-icon treemacs-icon-clojure    "clojure.png"    "clj" "cljs" "cljc")
  (treemacs--setup-icon treemacs-icon-typescript "typescript.png" "ts" "tsx")
  (treemacs--setup-icon treemacs-icon-vue        "vue.png"        "vue")
  (treemacs--setup-icon treemacs-icon-css        "css.png"        "css")
  (treemacs--setup-icon treemacs-icon-conf       "conf.png"       "properties" "conf" "config" "ini" "xdefaults" "xresources" "terminalrc" "toml")
  (treemacs--setup-icon treemacs-icon-html       "html.png"       "html" "htm")
  (treemacs--setup-icon treemacs-icon-git        "git.png"        "git" "gitignore" "gitconfig")
  (treemacs--setup-icon treemacs-icon-dart       "dart.png"       "dart")
  (treemacs--setup-icon treemacs-icon-java       "java.png"       "java")
  (treemacs--setup-icon treemacs-icon-kotlin     "kotlin.png"     "kt")
  (treemacs--setup-icon treemacs-icon-scala      "scala.png"      "scala")
  (treemacs--setup-icon treemacs-icon-sbt        "sbt.png"        "sbt")
  (treemacs--setup-icon treemacs-icon-go         "go.png"         "go")
  (treemacs--setup-icon treemacs-icon-js         "js.png"         "js" "jsx")
  (treemacs--setup-icon treemacs-icon-hy         "hy.png"         "hy")
  (treemacs--setup-icon treemacs-icon-json       "json.png"       "json")
  (treemacs--setup-icon treemacs-icon-julia      "julia.png"      "jl"))

(defun treemacs--setup-icons ()
  "Create and define all icons-related caches, hashes and stashes."
  (setq treemacs-icons-hash (make-hash-table :size 100 :test #'equal))
  (if treemacs--image-creation-impossible
      (treemacs--setup-tui-icons)
    (treemacs--setup-gui-icons)))

(only-during-treemacs-init
 (treemacs--setup-icons))

(defun treemacs-reset-icons ()
  "Reset customized icons to their default values."
  (interactive)
  ;; no warnings since the variables are known to exist
  (setq treemacs-icon-fallback        (alist-get 'treemacs-icon-fallback        treemacs--default-icons-alist)
        treemacs-icon-closed          (alist-get 'treemacs-icon-closed          treemacs--default-icons-alist)
        treemacs-icon-open            (alist-get 'treemacs-icon-open            treemacs--default-icons-alist)
        treemacs-icon-root            (alist-get 'treemacs-icon-root            treemacs--default-icons-alist)
        treemacs-icon-tag-leaf        (alist-get 'treemacs-icon-tag-leaf        treemacs--default-icons-alist)
        treemacs-icon-tag-node-closed (alist-get 'treemacs-icon-tag-node-closed treemacs--default-icons-alist)
        treemacs-icon-tag-node-open   (alist-get 'treemacs-icon-tag-node-open   treemacs--default-icons-alist))
  (treemacs--setup-icons)
  (treemacs--refresh-buffer-icons))

(defun treemacs--refresh-buffer-icons ()
  "Refreshes all icons in a buffer, including projects' root icons.
Run after commands that (potentially) change icons like resizing or resetting
them."
  (when (fboundp 'clear-image-cache)
    (clear-image-cache t))
  (treemacs-run-in-every-buffer
   (treemacs--adjust-icons-to-window-system)
   (treemacs--do-refresh (current-buffer) 'all)
   (save-excursion
     (treemacs-with-writable-buffer
      ;; root icons aren't switched on a refresh so we do it manually
      (dolist (project (-> (treemacs-current-workspace)
                           (treemacs-workspace->projects)))
        (goto-char (treemacs-project->position project))
        (treemacs--button-symbol-switch treemacs-icon-root))))))

;;;###autoload
(defun treemacs-resize-icons (size)
  "SIZE in pixels icons should be resized to.

If SIZE is 'nil' the icons are not resized and will retain their default size of
22 pixels.

There is only one size, the icons are square and the aspect ratio will be
preserved when resizing them therefore width and height are the same.

Resizing the icons only works if Emacs was built with ImageMagick support. If
this is not the case this function will report an error.

Custom icons are not taken into account, only the site of treemacs' own icons
is changed."
  (interactive "nIcon size in pixels: ")
  (setq treemacs--icon-size size)
  ;; resizing only works in gui displays so we just re-run the routine that creates
  ;; all the icons after the new size is set
  (treemacs--setup-gui-icons)
  (treemacs--refresh-buffer-icons))

(defun treemacs--adjust-icons-to-window-system ()
  "Check if the current treemacs buffer should use TUI icons.
If it's running in a TUI switch to simple text icons.

TUI icons will be used if
 * `treemacs--image-creation-impossible' is t,
 * `treemacs-no-png-images' is it
 * or if the current frame is a TUI frame"
  (if (or treemacs--image-creation-impossible
            treemacs-no-png-images
            (not (window-system)))
      (progn
        (setq-local treemacs-icons-hash           (ht))
        (setq-local treemacs-icon-root            treemacs-icon-root-text)
        (setq-local treemacs-icon-open            treemacs-icon-open-text)
        (setq-local treemacs-icon-closed          treemacs-icon-closed-text)
        (setq-local treemacs-icon-fallback        treemacs-icon-fallback-text)
        (setq-local treemacs-icon-tag-node-open   treemacs-icon-tag-node-open-text)
        (setq-local treemacs-icon-tag-node-closed treemacs-icon-tag-node-closed-text)
        (setq-local treemacs-icon-tag-leaf        treemacs-icon-tag-leaf-text))
    (setq-local treemacs-icon-root            treemacs-icon-root-png)
    (setq-local treemacs-icon-open            treemacs-icon-open-png)
    (setq-local treemacs-icon-closed          treemacs-icon-closed-png)
    (setq-local treemacs-icon-fallback        treemacs-icon-text)
    (setq-local treemacs-icon-tag-node-open   treemacs-icon-tag-node-open-png)
    (setq-local treemacs-icon-tag-node-closed treemacs-icon-tag-node-closed-png)
    (setq-local treemacs-icon-tag-leaf        treemacs-icon-tag-leaf-png)))

;;;###autoload
(defun treemacs-define-custom-icon (icon &rest file-extensions)
  "Define a custom ICON to use for FILE-EXTENSIONS.

Note that treemacs has a very loose definition of what constitutes a file
extension - it's either everything past the last period, or just the file's full
name if there is no period. This makes it possible to match file names like
'.gitignore' and 'Makefile'.

FILE-EXTENSIONS are also not case sensitive and will be downcased before they're
inserted into `treemacs-icons-hash'."
  (push icon treemacs--created-icons)
  (--each file-extensions
    (ht-set! treemacs-icons-hash
             (downcase it)
             (concat icon " "))))

;;;###autoload
(defun treemacs-map-icons-with-auto-mode-alist (extensions mode-icon-alist)
  "Remaps icons for EXTENSIONS according to `auto-mode-alist'.
EXTENSIONS should be a list of file extensions such that they match the regex
stored in `auto-mode-alist', for example '\(\".cc\"\).
MODE-ICON-ALIST is an alist that maps which mode from `auto-mode-alist' should
be assigned which treemacs icon, for exmaple
'\(\(c-mode . treemacs-icon-c\)
  \(c++-mode . treemacs-icon-cpp\)\)"
  (dolist (extension extensions)
    (-when-let* ((mode (cdr (--first (s-matches? (car it) extension) auto-mode-alist)))
                 (icon (cdr (assq mode mode-icon-alist))))
      (treemacs-log "Map %s to %s" extension (symbol-name icon))
      (ht-set! treemacs-icons-hash (substring extension 1) (symbol-value icon)))))

(provide 'treemacs-icons)

;;; treemacs-icons.el ends here
