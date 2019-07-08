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
;;; Most of everything related to icons is handled here. Specifically the
;;; definition, instantiation, customization, resizing and resetting of icons.

;;; Code:

(require 'image)
(require 'dash)
(require 's)
(require 'ht)
(require 'treemacs-visuals)
(require 'treemacs-themes)
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

(define-inline treemacs--should-use-tui-icons? ()
  "Determines whether the current buffer must use TUI instead of GUI icons."
  (declare (side-effect-free t))
  (inline-quote
   (or (treemacs--is-image-creation-impossible?)
       treemacs-no-png-images
       (not (window-system)))))

(defvar treemacs-icons nil
  "Currently used icons.
Aliased to the current theme's gui or tui icons.")

(defvar treemacs--icon-symbols nil
  "List of icons with variables.
Every symbol S maps to a variable named \"treemacs-icons-S\". In addition S is
also the key for the icon in both `treemacs-gui-icons' and `treemacs-tui-icons'.
This combination alllows these icons-with-variables to be correctly changed in
`treemacs--select-icon-set'.")

(defvar treemacs--icon-size 22
  "Size in pixels icons will be resized to.
See also `treemacs-resize-icons'.")

(defvar treemacs--icon-vars nil
  "List of all icons assigned to variables.")

(defmacro treemacs--root-icon-size-adjust (width height)
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
       (when (and (integerp treemacs--icon-size)
                  (s-ends-with? "root.png" ,file-path))
         (treemacs--root-icon-size-adjust width height))
       (if (and (integerp treemacs--icon-size) (image-type-available-p 'imagemagick))
           (create-image ,file-path 'imagemagick nil :ascent 'center :width width :height height)
         (create-image ,file-path 'png nil :ascent 'center))))))

(define-inline treemacs--create-icon-strings (file fallback)
  "Create propertized icon strings for a given FILE image and TUI FALLBACK."
  (inline-letevals (file fallback)
    (inline-quote
     (let ((tui-icon ,fallback)
           (gui-icon
            (if (treemacs--is-image-creation-impossible?)
                ,fallback
              (let* ((img-selected   (treemacs--create-image ,file))
                     (img-unselected (copy-sequence img-selected)))
                (nconc img-selected   `(:background ,treemacs--selected-icon-background))
                (nconc img-unselected `(:background ,treemacs--not-selected-icon-background))
                (concat (propertize " "
                                    'display img-unselected
                                    'img-selected img-selected
                                    'img-unselected img-unselected)
                        " ")))))
       (cons gui-icon tui-icon)))))

(defmacro treemacs--splice-icon (icon)
  "Splice the given ICON data depending on whether it is a value or an sexp."
  (if (listp icon)
      `(progn ,@icon)
    `(progn ,icon)))

(cl-defmacro treemacs-create-icon (&key file icon (fallback " ") icons-dir extensions)
  "Create an icon for the current theme.
- FILE is a file path relative to the icon directory of the current theme.
- ICON is a string of an already created icon. Mutually exclusive with FILE.
- ICONS-DIR can optionally be used to overwrite the path used to find icons.
  Normally the current theme's icon-path is used, but it may be convenient to
  use another when calling `treemacs-modify-theme'.
- FALLBACK is the fallback string for situations where png images are
  unavailable.
- EXTENSIONS is a list of file extensions the icon should be used for.
  Note that treemacs has a loose understanding of what constitutes an extension:
  it's either the text past the last period or the entire filename, so names
  like \".gitignore\" and \"Makefile\" can be matched as well.
  An extension may also be a symbol instead of a string. In this case treemacs
  will also create a variable named \"treemacs-icon-%s\" making it universally
  accessible."
  (treemacs-static-assert (or (null icon) (null file))
    "FILE and ICON arguments are mutually exclusive")
  `(let* ((icons-dir ,(if icons-dir icons-dir `(treemacs-theme->path treemacs--current-theme)))
          (icon-path ,(if file `(f-join icons-dir ,file) nil))
          (icon-pair ,(if file `(treemacs--create-icon-strings icon-path ,fallback)
                        `(cons ,(treemacs--splice-icon icon) ,fallback)))
          (gui-icons (treemacs-theme->gui-icons treemacs--current-theme))
          (tui-icons (treemacs-theme->tui-icons treemacs--current-theme))
          (gui-icon  (car icon-pair))
          (tui-icon  (cdr icon-pair)))
     ,(unless file `(ignore icon-path))
     ,@(->> (-filter #'symbolp extensions)
            (--map `(progn (add-to-list 'treemacs--icon-symbols ',it)
                           (defvar ,(intern (format "treemacs-icon-%s" it)) nil))))
     (--each ',extensions
       (ht-set! gui-icons it gui-icon)
       (ht-set! tui-icons it tui-icon))))

(treemacs-create-theme "Default"
  :icon-directory (f-join treemacs-dir "icons/default")
  :config
  (progn
    ;; directory and other icons
    (treemacs-create-icon :file "root.png"        :extensions (root)       :fallback "")
    (treemacs-create-icon :file "dir-closed.png"  :extensions (dir-closed) :fallback (propertize "+ " 'face 'treemacs-term-node-face))
    (treemacs-create-icon :file "dir-open.png"    :extensions (dir-open)   :fallback (propertize "- " 'face 'treemacs-term-node-face))
    (treemacs-create-icon :file "tags-leaf.png"   :extensions (tag-leaf)   :fallback (propertize "• " 'face 'font-lock-constant-face))
    (treemacs-create-icon :file "tags-open.png"   :extensions (tag-open)   :fallback (propertize "▸ " 'face 'font-lock-string-face))
    (treemacs-create-icon :file "tags-closed.png" :extensions (tag-closed) :fallback (propertize "▾ " 'face 'font-lock-string-face))
    (treemacs-create-icon :file "error.png"       :extensions (error)      :fallback (propertize "• " 'face 'font-lock-string-face))
    (treemacs-create-icon :file "warning.png"     :extensions (warning)    :fallback (propertize "• " 'face 'font-lock-string-face))
    (treemacs-create-icon :file "info.png"        :extensions (info)       :fallback (propertize "• " 'face 'font-lock-string-face))

    ;; ;; file icons
    (treemacs-create-icon :file "txt.png"         :extensions (fallback))
    (treemacs-create-icon :file "emacs.png"       :extensions ("el" "elc"))
    (treemacs-create-icon :file "ledger.png"      :extensions ("ledger"))
    (treemacs-create-icon :file "yaml.png"        :extensions ("yml" "yaml"))
    (treemacs-create-icon :file "shell.png"       :extensions ("sh" "zsh" "fish"))
    (treemacs-create-icon :file "pdf.png"         :extensions ("pdf"))
    (treemacs-create-icon :file "c.png"           :extensions ("c" "h"))
    (treemacs-create-icon :file "cpp.png"         :extensions ("cpp" "cxx" "hpp" "tpp" "cc" "hh"))
    (treemacs-create-icon :file "haskell.png"     :extensions ("hs" "lhs" "cabal"))
    (treemacs-create-icon :file "python.png"      :extensions ("py" "pyc"))
    (treemacs-create-icon :file "markdown.png"    :extensions ("md"))
    (treemacs-create-icon :file "asciidoc.png"    :extensions ("adoc" "asciidoc"))
    (treemacs-create-icon :file "rust.png"        :extensions ("rs"))
    (treemacs-create-icon :file "image.png"       :extensions ("jpg" "jpeg" "bmp" "svg" "png" "xpm" "gif"))
    (treemacs-create-icon :file "emacs.png"       :extensions ("el" "elc"))
    (treemacs-create-icon :file "clojure.png"     :extensions ("clj" "cljs" "cljc"))
    (treemacs-create-icon :file "ts.png"          :extensions ("ts" "tsx"))
    (treemacs-create-icon :file "vue.png"         :extensions ("vue"))
    (treemacs-create-icon :file "css.png"         :extensions ("css"))
    (treemacs-create-icon :file "conf.png"        :extensions ("properties" "conf" "config" "cfg" "ini" "xdefaults" "xresources" "terminalrc" "ledgerrc"))
    (treemacs-create-icon :file "html.png"        :extensions ("html" "htm"))
    (treemacs-create-icon :file "git.png"         :extensions ("git" "gitignore" "gitconfig" "gitmodules"))
    (treemacs-create-icon :file "dart.png"        :extensions ("dart"))
    (treemacs-create-icon :file "java.png"        :extensions ("java"))
    (treemacs-create-icon :file "kotlin.png"      :extensions ("kt"))
    (treemacs-create-icon :file "scala.png"       :extensions ("scala"))
    (treemacs-create-icon :file "sbt.png"         :extensions ("sbt"))
    (treemacs-create-icon :file "go.png"          :extensions ("go"))
    (treemacs-create-icon :file "js.png"          :extensions ("js" "jsx"))
    (treemacs-create-icon :file "hy.png"          :extensions ("hy"))
    (treemacs-create-icon :file "json.png"        :extensions ("json"))
    (treemacs-create-icon :file "julia.png"       :extensions ("jl"))
    (treemacs-create-icon :file "elx.png"         :extensions ("ex"))
    (treemacs-create-icon :file "elx-light.png"   :extensions ("exs" "eex"))
    (treemacs-create-icon :file "ocaml.png"       :extensions ("ml" "mli"))
    (treemacs-create-icon :file "puppet.png"      :extensions ("pp"))
    (treemacs-create-icon :file "docker.png"      :extensions ("dockerfile"))
    (treemacs-create-icon :file "vagrant.png"     :extensions ("vagrantfile"))
    (treemacs-create-icon :file "jinja2.png"      :extensions ("j2" "jinja2"))
    (treemacs-create-icon :file "video.png"       :extensions ("webm" "mp4" "avi" "mkv" "flv" "mov" "wmv" "mpg" "mpeg" "mpv"))
    (treemacs-create-icon :file "tex.png"         :extensions ("tex"))
    (treemacs-create-icon :file "racket.png"      :extensions ("racket" "rkt" "rktl" "rktd" "scrbl" "scribble" "plt"))
    (treemacs-create-icon :file "vsc/make.png"    :extensions ("makefile"))
    (treemacs-create-icon :file "vsc/license.png" :extensions ("license"))
    (treemacs-create-icon :file "vsc/zip.png"     :extensions ("zip" "7z" "tar" "gz" "rar"))
    (treemacs-create-icon :file "vsc/elm.png"     :extensions ("elm"))
    (treemacs-create-icon :file "vsc/xml.png"     :extensions ("xml" "xsl"))
    (treemacs-create-icon :file "vsc/binary.png"  :extensions ("exe" "dll" "obj" "so" "o"))
    (treemacs-create-icon :file "vsc/ruby.png"    :extensions ("rb"))
    (treemacs-create-icon :file "vsc/scss.png"    :extensions ("scss"))
    (treemacs-create-icon :file "vsc/lua.png"     :extensions ("lua"))
    (treemacs-create-icon :file "vsc/log.png"     :extensions ("log"))
    (treemacs-create-icon :file "vsc/lisp.png"    :extensions ("lisp"))
    (treemacs-create-icon :file "vsc/sql.png"     :extensions ("sql"))
    (treemacs-create-icon :file "vsc/toml.png"    :extensions ("toml"))
    (treemacs-create-icon :file "vsc/nim.png"     :extensions ("nim"))
    (treemacs-create-icon :file "vsc/org.png"     :extensions ("org"))
    (treemacs-create-icon :file "vsc/perl.png"    :extensions ("pl" "pm" "perl"))
    (treemacs-create-icon :file "vsc/vim.png"     :extensions ("vimrc" "tridactylrc" "vimperatorrc" "ideavimrc" "vrapperrc"))
    (treemacs-create-icon :file "vsc/deps.png"    :extensions ("cask"))
    (treemacs-create-icon :file "vsc/r.png"       :extensions ("r"))
    (treemacs-create-icon :file "vsc/reason.png"  :extensions ("re" "rei"))))

(define-inline treemacs-icon-for-file (file)
  "Retrieve an icon for FILE from `treemacs-icons' based on its extension.
Uses `treemacs-icon-fallback' as fallback."
  (declare (pure t))
  (inline-letevals (file)
    (inline-quote
     (ht-get treemacs-icons
             (-> ,file (treemacs--file-extension) (downcase))
             ;; no warnings since the fallback var is defined at the end of the module
             (with-no-warnings treemacs-icon-fallback)))))

;;;###autoload
(defun treemacs-resize-icons (size)
  "Resize the current theme's icons to the given SIZE.

If SIZE is 'nil' the icons are not resized and will retain their default size of
22 pixels.

There is only one size, the icons are square and the aspect ratio will be
preserved when resizing them therefore width and height are the same.

Resizing the icons only works if Emacs was built with ImageMagick support. If
this is not the case this function will report an error.

Custom icons are not taken into account, only the size of treemacs' own icons
is changed."
  (interactive "nIcon size in pixels: ")
  (setq treemacs--icon-size size)
  (treemacs--maphash (treemacs-theme->gui-icons treemacs--current-theme) (_ icon)
    (let ((display        (get-text-property 0 'display icon))
          (img-selected   (get-text-property 0 'img-selected icon))
          (img-unselected (get-text-property 0 'img-unselected icon))
          (width          treemacs--icon-size)
          (height         treemacs--icon-size))
      (when (s-ends-with? "root.png" (plist-get (cdr display) :file))
        (treemacs--root-icon-size-adjust width height))
      (dolist (property (list display img-selected img-unselected))
        (plist-put (cdr property) :height height)
        (plist-put (cdr property) :width width)))))

(defun treemacs--select-icon-set ()
  "Select the right set of icons for the current buffer.
Will select either the GUI or the TUI icons of the current theme.

TUI icons will be used if
 * `treemacs--is-image-creation-impossible?' returns t,
 * `treemacs-no-png-images' is it
 * or if the current frame is a TUI frame"
  (-let [icons (if (treemacs--should-use-tui-icons?)
                   (treemacs-theme->tui-icons treemacs--current-theme)
                 (treemacs-theme->gui-icons treemacs--current-theme))]
    (setq-local treemacs-icons icons)
    (dolist (icon-symbol treemacs--icon-symbols)
      (let ((variable (intern (format "treemacs-icon-%s" icon-symbol)))
            (value    (ht-get icons icon-symbol)))
        (set (make-local-variable variable) value)))))

(defmacro treemacs-get-icon-value (ext &optional tui theme)
  "Get the value of an icon for extension EXT.
If TUI is non-nil the terminal fallback value is returned.
THEME is the name of the theme to look in. Will cause an error if the theme
does not exist."
  `(let* ((theme ,(if theme
                      `(treemacs--find-theme ,theme)
                    `(treemacs-current-theme)))
          (icons ,(if tui
                     `(treemacs-theme->tui-icons theme)
                   `(treemacs-theme->gui-icons theme))))
     (ht-get icons ,ext)))

;;;###autoload
(defun treemacs-define-custom-icon (icon &rest file-extensions)
  "Define a custom ICON for the current theme to use for FILE-EXTENSIONS.

Note that treemacs has a very loose definition of what constitutes a file
extension - it's either everything past the last period, or just the file's full
name if there is no period. This makes it possible to match file names like
'.gitignore' and 'Makefile'.

Additionally FILE-EXTENSIONS are also not case sensitive and will be stored in a
downcased state."
  (unless icon
    (user-error "Custom icon cannot be nil"))
  (dolist (ext file-extensions)
    (ht-set! (treemacs-theme->gui-icons treemacs--current-theme)
             (downcase ext)
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
      (ht-set! (treemacs-theme->gui-icons treemacs--current-theme)
               (substring extension 1)
               (symbol-value icon)))))

(treemacs-only-during-init
  (treemacs-load-theme "Default"))

(provide 'treemacs-icons)

;;; treemacs-icons.el ends here
