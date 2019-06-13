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
(eval-and-compile
  (require 'inline)
  (require 'treemacs-macros))

(define-inline treemacs--is-image-creation-impossible? ()
  "Will return non-nil when Emacs is unable to create images.
In this scenario (usually caused by running Emacs without a graphical
environment) treemacs will not create any of its icons and will be forced to
permanently use its simple string icon fallack."
  (declare (pure t) (side-effect-free t))
  (inline-quote (not (image-type-available-p 'png))))

(defvar treemacs-icons-hash (make-hash-table :size 200 :test #'equal)
  "Hash table containing a mapping of icons onto file extensions.")

(defvar treemacs--icon-size 22
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

(define-inline treemacs--created-icons ()
  "Importable getter for `treemacs--created-icons'."
  (declare (side-effect-free t))
  (inline-quote treemacs--created-icons))

(defmacro treemacs--size-adjust (width height)
  "Special adjust for the WIDTH and HEIGHT of an icon.
Necessary since root icons are not rectangular."
  `(let ((w (round (* ,width 0.9090)))
         (h (round (* ,height 1.1818))))
     (setq ,width w ,height h)))

(define-inline treemacs--create-image (file-path)
  "Load image from FILE-PATH and size it based on `treemacs--icon-size'."
  (inline-letevals (file-path)
    (inline-quote
     (let ((height treemacs--icon-size)
           (width treemacs--icon-size))
       ;; special case for the root icon which is unique in being 20x26 pixels large
       (when (and (integerp treemacs--icon-size)
                  (s-ends-with? "root.png" ,file-path))
         (treemacs--size-adjust width height))
       (if (and (integerp treemacs--icon-size) (image-type-available-p 'imagemagick))
           (create-image ,file-path 'imagemagick nil :ascent 'center :width width :height height)
         (create-image ,file-path 'png nil :ascent 'center))))))

(defmacro treemacs--setup-icon (var file-name &rest extensions)
  "Define VAR with its display property being the image created from FILE-NAME.
Insert VAR into `treemacs-icon-hash' for each of the given file EXTENSIONS."
  `(progn
     (defvar ,var nil)
     (setq ,var
           (eval-when-compile
             (if (treemacs--is-image-creation-impossible?)
                 treemacs-icon-fallback-text
               ;; need to defvar here or the compiler will complain
               (defvar treemacs--icon-size nil)
               (let* ((image-unselected (treemacs--create-image (f-join treemacs-dir "icons/" ,file-name)))
                      (image-selected   (treemacs--create-image (f-join treemacs-dir "icons/" ,file-name))))
                 (treemacs--set-img-property image-selected   :background treemacs--selected-icon-background)
                 (treemacs--set-img-property image-unselected :background treemacs--not-selected-icon-background)
                 (concat (propertize " "
                                     'display image-unselected
                                     'img-selected image-selected
                                     'img-unselected image-unselected)
                         " ")))))
     (push ,var treemacs--created-icons)
     (--each (quote ,extensions) (ht-set! treemacs-icons-hash it ,var))
     ,var))

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
(defvar treemacs-icon-error           "")
(defvar treemacs-icon-warning         "")
(defvar treemacs-icon-info            "")


;; each of these icons takes one of 2 possible values - a 'png' variant for Emacsen
;; capable of displaying images and a 'txt' variant as fallback for TUI frames

(defvar treemacs-icon-root-text            "")
(defvar treemacs-icon-closed-text          (eval-when-compile (propertize "+ " 'face 'treemacs-term-node-face)))
(defvar treemacs-icon-open-text            (eval-when-compile (propertize "- " 'face 'treemacs-term-node-face)))
(defvar treemacs-icon-fallback-text        (eval-when-compile (propertize "  " 'face 'font-lock-keyword-face)))
(defvar treemacs-icon-tag-leaf-text        (eval-when-compile (propertize "• " 'face 'font-lock-constant-face)))
(defvar treemacs-icon-tag-node-closed-text (eval-when-compile (propertize "▸ " 'face 'font-lock-string-face)))
(defvar treemacs-icon-tag-node-open-text   (eval-when-compile (propertize "▾ " 'face 'font-lock-string-face)))
(defvar treemacs-icon-error-text           (eval-when-compile (propertize "• " 'face 'font-lock-string-face)))
(defvar treemacs-icon-warning-text         (eval-when-compile (propertize "• " 'face 'font-lock-string-face)))
(defvar treemacs-icon-info-text            (eval-when-compile (propertize "• " 'face 'font-lock-string-face)))

(defvar treemacs-icon-root-png            "")
(defvar treemacs-icon-closed-png          "")
(defvar treemacs-icon-open-png            "")
(defvar treemacs-icon-tag-leaf-png        "")
(defvar treemacs-icon-tag-node-closed-png "")
(defvar treemacs-icon-tag-node-open-png   "")
(defvar treemacs-icon-text                "")
(defvar treemacs-icon-error-png           "")
(defvar treemacs-icon-warning-png         "")
(defvar treemacs-icon-info-png            "")

(define-inline treemacs-icon-for-file (path)
  "Retrieve an icon for PATH from `treemacs-icons-hash'.
Uses `treemacs-icon-fallback' as fallback."
  (declare (pure t))
  (inline-letevals (path)
    (inline-quote
     (ht-get treemacs-icons-hash
             (-> ,path (treemacs--file-extension) (downcase))
             treemacs-icon-fallback))))

(defmacro treemacs--set-icon-save-default (&rest key-values)
  "Pass KEY-VALUES to `setq'.
Also save the assignments in `treemacs--default-icons-alist'."
  (treemacs-static-assert (= 0 (% (length key-values) 2))
    "Keys and values must sum to an even number.")
  (let ((key-vals (copy-sequence key-values))
        (assignments))
    (while key-vals
      (-let [(_ value . rest) key-vals]
        (setq key-vals rest)
        (push `(push (cons (quote ,value) ,value) treemacs--default-icons-alist) assignments)))
    `(progn
       (setq ,@key-values)
       ,@assignments)))

(defun treemacs--setup-gui-icons ()
  "Will set graphical values for non-file icons.
Will also fill `treemacs-icons-hash' with graphical file icons."
  ;; first setup png variants of the non-file icons
  (treemacs--setup-icon treemacs-icon-closed-png          "dir_closed.png")
  (treemacs--setup-icon treemacs-icon-open-png            "dir_open.png")
  (treemacs--setup-icon treemacs-icon-root-png            "root.png")
  (treemacs--setup-icon treemacs-icon-tag-leaf-png        "tags-leaf.png")
  (treemacs--setup-icon treemacs-icon-tag-node-closed-png "tags-closed.png")
  (treemacs--setup-icon treemacs-icon-tag-node-open-png   "tags-open.png")
  (treemacs--setup-icon treemacs-icon-text                "txt.png")
  (treemacs--setup-icon treemacs-icon-error-png           "error.png")
  (treemacs--setup-icon treemacs-icon-warning-png         "warning.png")
  (treemacs--setup-icon treemacs-icon-info-png            "info.png")

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
  (treemacs--setup-icon treemacs-icon-ledger       "ledger.png"           "ledger")
  (treemacs--setup-icon treemacs-icon-yaml         "yaml.png"             "yml" "yaml")
  (treemacs--setup-icon treemacs-icon-shell        "shell.png"            "sh" "zsh" "fish")
  (treemacs--setup-icon treemacs-icon-tex          "tex.png"              "tex")  
  (treemacs--setup-icon treemacs-icon-pdf          "pdf.png"              "pdf")
  (treemacs--setup-icon treemacs-icon-c            "c.png"                "c" "h")
  (treemacs--setup-icon treemacs-icon-cpp          "cpp.png"              "cpp" "cxx" "hpp" "tpp" "cc" "hh")
  (treemacs--setup-icon treemacs-icon-haskell      "haskell.png"          "hs" "lhs" "cabal")
  (treemacs--setup-icon treemacs-icon-python       "python.png"           "py" "pyc")
  (treemacs--setup-icon treemacs-icon-markdown     "markdown.png"         "md")
  (treemacs--setup-icon treemacs-icon-asciidoc     "asciidoc.png"         "adoc" "asciidoc")
  (treemacs--setup-icon treemacs-icon-rust         "rust.png"             "rs")
  (treemacs--setup-icon treemacs-icon-image        "image.png"            "jpg" "jpeg" "bmp" "svg" "png" "xpm" "gif")
  (treemacs--setup-icon treemacs-icon-emacs        "emacs.png"            "el" "elc")
  (treemacs--setup-icon treemacs-icon-clojure      "clojure.png"          "clj" "cljs" "cljc")
  (treemacs--setup-icon treemacs-icon-racket       "racket.png"          "racket" "rkt" "rktl" "rktd" "scrbl" "scribble" "plt")  
  (treemacs--setup-icon treemacs-icon-typescript   "typescript.png"       "ts" "tsx")
  (treemacs--setup-icon treemacs-icon-vue          "vue.png"              "vue")
  (treemacs--setup-icon treemacs-icon-css          "css.png"              "css")
  (treemacs--setup-icon treemacs-icon-conf         "conf.png"             "properties" "conf" "config" "cfg" "ini" "xdefaults" "xresources" "terminalrc" "ledgerrc")
  (treemacs--setup-icon treemacs-icon-html         "html.png"             "html" "htm")
  (treemacs--setup-icon treemacs-icon-git          "git.png"              "git" "gitignore" "gitconfig" "gitmodules")
  (treemacs--setup-icon treemacs-icon-dart         "dart.png"             "dart")
  (treemacs--setup-icon treemacs-icon-java         "java.png"             "java")
  (treemacs--setup-icon treemacs-icon-kotlin       "kotlin.png"           "kt")
  (treemacs--setup-icon treemacs-icon-scala        "scala.png"            "scala")
  (treemacs--setup-icon treemacs-icon-sbt          "sbt.png"              "sbt")
  (treemacs--setup-icon treemacs-icon-go           "go.png"               "go")
  (treemacs--setup-icon treemacs-icon-js           "js.png"               "js" "jsx")
  (treemacs--setup-icon treemacs-icon-hy           "hy.png"               "hy")
  (treemacs--setup-icon treemacs-icon-json         "json.png"             "json")
  (treemacs--setup-icon treemacs-icon-julia        "julia.png"            "jl")
  (treemacs--setup-icon treemacs-icon-elixir       "elixir.png"           "ex")
  (treemacs--setup-icon treemacs-icon-elixir-light "elixir_light.png"     "exs" "eex")
  (treemacs--setup-icon treemacs-icon-ocaml        "ocaml.png"            "ml" "mli")
  (treemacs--setup-icon treemacs-icon-puppet       "puppet.png"           "pp")
  (treemacs--setup-icon treemacs-icon-docker       "docker.png"           "dockerfile")
  (treemacs--setup-icon treemacs-icon-vagrant      "vagrant.png"          "vagrantfile")
  (treemacs--setup-icon treemacs-icon-jinja2       "jinja2.png"           "j2" "jinja2")
  (treemacs--setup-icon treemacs-icon-video        "video.png"            "webm" "mp4" "avi" "mkv" "flv" "mov" "wmv" "mpg" "mpeg" "mpv")
  (treemacs--setup-icon treemacs-icon-makefile     "vsc/makefile.png"     "makefile")
  (treemacs--setup-icon treemacs-icon-license      "vsc/license.png"      "license")
  (treemacs--setup-icon treemacs-icon-zip          "vsc/zip.png"          "zip" "7z" "tar" "gz" "rar")
  (treemacs--setup-icon treemacs-icon-elm          "vsc/elm.png"          "elm")
  (treemacs--setup-icon treemacs-icon-xml          "vsc/xml.png"          "xml" "xsl")
  (treemacs--setup-icon treemacs-icon-binary       "vsc/binary.png"       "exe" "dll" "obj" "so" "o")
  (treemacs--setup-icon treemacs-icon-ruby         "vsc/ruby.png"         "rb")
  (treemacs--setup-icon treemacs-icon-scss         "vsc/scss.png"         "scss")
  (treemacs--setup-icon treemacs-icon-lua          "vsc/lua.png"          "lua")
  (treemacs--setup-icon treemacs-icon-log          "vsc/log.png"          "log")
  (treemacs--setup-icon treemacs-icon-lisp         "vsc/lisp.png"         "lisp")
  (treemacs--setup-icon treemacs-icon-sql          "vsc/sql.png"          "sql")
  (treemacs--setup-icon treemacs-icon-toml         "vsc/toml.png"         "toml")
  (treemacs--setup-icon treemacs-icon-nim          "vsc/nim.png"          "nim")
  (treemacs--setup-icon treemacs-icon-org          "vsc/org.png"          "org")
  (treemacs--setup-icon treemacs-icon-perl         "vsc/perl.png"         "pl" "pm" "perl")
  (treemacs--setup-icon treemacs-icon-vim          "vsc/vim.png"          "vimrc" "tridactylrc" "vimperatorrc" "ideavimrc" "vrapperrc")
  (treemacs--setup-icon treemacs-icon-depend       "vsc/dependencies.png" "cask")
  (treemacs--setup-icon treemacs-icon-r            "vsc/r.png"            "r")
  (treemacs--setup-icon treemacs-icon-reason       "vsc/reason.png"       "re" "rei"))

(defun treemacs--setup-icons ()
  "Create and define all icons-related caches, hashes and stashes."
  (setq treemacs-icons-hash (make-hash-table :size 300 :test #'equal))
  (treemacs--setup-gui-icons)
  ;; prevent nil values, as in https://github.com/hlissner/doom-emacs/issues/941#issuecomment-429154169
  ;; however that happens
  (setq treemacs--created-icons (-reject #'null treemacs--created-icons)))

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
  (--each (treemacs--created-icons)
    (let ((display        (get-text-property 0 'display it))
          (img-selected   (get-text-property 0 'img-selected it))
          (img-unselected (get-text-property 0 'img-unselected it))
          (width          treemacs--icon-size)
          (height         treemacs--icon-size))
      (when (s-ends-with? "root.png" (plist-get (cdr display) :file))
        (treemacs--size-adjust width height))
      (dolist (property (list display img-selected img-unselected))
        (plist-put (cdr property) :height height)
        (plist-put (cdr property) :width width)))))

(defun treemacs--adjust-icons-to-window-system ()
  "Check if the current treemacs buffer should use TUI icons.
If it's running in a TUI switch to simple text icons.

TUI icons will be used if
 * `treemacs--is-image-creation-impossible?' returns t,
 * `treemacs-no-png-images' is it
 * or if the current frame is a TUI frame"
  (if (or (treemacs--is-image-creation-impossible?)
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
        (setq-local treemacs-icon-tag-leaf        treemacs-icon-tag-leaf-text)
        (setq-local treemacs-icon-warning         treemacs-icon-warning-text)
        (setq-local treemacs-icon-error           treemacs-icon-error-text)
        (setq-local treemacs-icon-info            treemacs-icon-info-text))
    (setq-local treemacs-icon-root            treemacs-icon-root-png)
    (setq-local treemacs-icon-open            treemacs-icon-open-png)
    (setq-local treemacs-icon-closed          treemacs-icon-closed-png)
    (setq-local treemacs-icon-fallback        treemacs-icon-text)
    (setq-local treemacs-icon-tag-node-open   treemacs-icon-tag-node-open-png)
    (setq-local treemacs-icon-tag-node-closed treemacs-icon-tag-node-closed-png)
    (setq-local treemacs-icon-tag-leaf        treemacs-icon-tag-leaf-png)
    (setq-local treemacs-icon-warning         treemacs-icon-warning-png)
    (setq-local treemacs-icon-error           treemacs-icon-error-png)
    (setq-local treemacs-icon-info            treemacs-icon-info-png)))

;;;###autoload
(defun treemacs-define-custom-icon (icon &rest file-extensions)
  "Define a custom ICON to use for FILE-EXTENSIONS.

Note that treemacs has a very loose definition of what constitutes a file
extension - it's either everything past the last period, or just the file's full
name if there is no period. This makes it possible to match file names like
'.gitignore' and 'Makefile'.

FILE-EXTENSIONS are also not case sensitive and will be downcased before they're
inserted into `treemacs-icons-hash'."
  (unless icon
    (user-error "Custom icon cannot be nil"))
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

(treemacs-only-during-init
 (treemacs--setup-icons))

(provide 'treemacs-icons)

;;; treemacs-icons.el ends here
