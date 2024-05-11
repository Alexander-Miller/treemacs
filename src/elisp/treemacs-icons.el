;;; treemacs.el --- A tree style file viewer package -*- lexical-binding: t -*-

;; Copyright (C) 2024 Alexander Miller

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

;; Most of everything related to icons is handled here.  Specifically
;; the definition, instantiation, customization, resizing and
;; resetting of icons.

;;; Code:

(require 'image)
(require 'dash)
(require 's)
(require 'ht)
(require 'treemacs-themes)
(require 'treemacs-logging)
(require 'treemacs-scope)

(eval-when-compile
  (require 'cl-lib)
  (require 'inline)
  (require 'treemacs-macros))

(define-inline treemacs--set-img-property (image property value)
  "Set IMAGE's PROPERTY to VALUE."
  ;; the emacs26 code where this is copied from says it's for internal
  ;; use only - let's se how that goes
  (inline-letevals (image property value)
    (inline-quote
     (progn
       (plist-put (cdr ,image) ,property ,value)
       ,value))))

(define-inline treemacs--get-img-property (image property)
  "Return the value of PROPERTY in IMAGE."
  ;; code aken from emacs 26
  (declare (side-effect-free t))
  (inline-letevals (image property)
    (inline-quote
     (plist-get (cdr ,image) ,property))))
(gv-define-setter treemacs--get-img-property (val img prop)
  `(plist-put (cdr ,img) ,prop ,val))

(defmacro treemacs-get-icon-value (ext &optional tui theme)
  "Get the value of an icon for extension EXT.
If TUI is non-nil the terminal fallback value is returned.
THEME is the name of the theme to look in.  Will cause an error if the theme
does not exist."
  `(let* ((theme ,(if theme
                      `(treemacs--find-theme ,theme)
                    `(treemacs-current-theme)))
          (icons ,(if tui
                      `(treemacs-theme->tui-icons theme)
                    `(treemacs-theme->gui-icons theme))))
     (ht-get icons ,ext)))

(define-inline treemacs--get-local-face-background (face)
  "Get the `:background' of the given face.
Unlike `face-attribute' this will take the `faces-remapping-alist' into
account."
  (declare (side-effect-free t))
  (inline-letevals (face)
    (inline-quote
     (--if-let (car (alist-get ,face face-remapping-alist))
         (plist-get it :background)
       (face-attribute ,face :background nil t)))))

(define-inline treemacs--is-image-creation-impossible? ()
  "Will return non-nil when Emacs is unable to create images.
In this scenario (usually caused by running Emacs without a graphical
environment) treemacs will not create any of its icons and will be forced to
permanently use its simple string icon fallback."
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
This combination allows these icons-with-variables to be correctly changed in
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

(defun treemacs--create-image (file-path)
  "Load image from FILE-PATH and size it based on `treemacs--icon-size'."
  (let ((height treemacs--icon-size)
        (width treemacs--icon-size))
    (when (and (integerp treemacs--icon-size)
               (s-starts-with? "root-" file-path))
      (treemacs--root-icon-size-adjust width height))
    (if (and (integerp treemacs--icon-size) (image-type-available-p 'imagemagick))
        (create-image
         file-path 'imagemagick nil
         :ascent 'center
         :width width
         :height height
         :mask 'heuristic)
      (create-image
       file-path
       (intern (treemacs--file-extension (treemacs--filename file-path)))
       nil
       :ascent 'center
       :width width
       :height height
       :mask 'heuristic))))

(defun treemacs--create-icon-strings (file fallback)
  "Create propertized icon strings for a given FILE image and TUI FALLBACK."
  (let ((tui-icon fallback)
        (gui-icon
         (if (treemacs--is-image-creation-impossible?)
             fallback
           (concat (propertize
                    " "
                    'display (treemacs--create-image file))
                   " "))))
    (cons gui-icon tui-icon)))

(defmacro treemacs--splice-icon (icon)
  "Splice the given ICON data depending on whether it is a value or an sexp."
  (if (listp icon)
      `(progn ,@icon)
    `(progn ,icon)))

(cl-defmacro treemacs-create-icon (&key file icon (fallback " ") icons-dir extensions)
  "Create an icon for the current theme.
- FILE is a file path relative to the icon directory of the current theme.
- ICON is a string of an already created icon.  Mutually exclusive with FILE.
- FALLBACK is the fallback string for situations where png images are
  unavailable.  Can be set to `same-as-icon' to use the same value as ICON.
- ICONS-DIR can optionally be used to overwrite the path used to find icons.
  Normally the current theme's icon-path is used, but it may be convenient to
  use another when calling `treemacs-modify-theme'.
- EXTENSIONS is a list of file extensions the icon should be used for.
  Note that treemacs has a loose understanding of what constitutes an extension:
  it's either the text past the last period or the entire filename, so names
  like \".gitignore\" and \"Makefile\" can be matched as well.
  An extension may also be a symbol instead of a string.  In this case treemacs
  will also create a variable named \"treemacs-icon-%s\" making it universally
  accessible."
  (treemacs-static-assert (or (null icon) (null file))
    "FILE and ICON arguments are mutually exclusive")
  (when (and (consp extensions) (or (symbolp (car extensions))
                                    (stringp (car extensions))))
    (setf extensions `(quote (,@extensions))))
  `(let* ((xs (--map (if (stringp it) (downcase it) it) ,extensions))
          (fallback   ,(if (equal fallback (quote 'same-as-icon))
                           icon
                         fallback))
          (icons-dir  ,(if icons-dir icons-dir `(treemacs-theme->path treemacs--current-theme)))
          (icon-path  ,(if file `(treemacs-join-path icons-dir ,file) nil))
          (icon-pair  ,(if file `(treemacs--create-icon-strings icon-path fallback)
                         `(cons ,(treemacs--splice-icon icon) fallback)))
          (gui-icons  (treemacs-theme->gui-icons treemacs--current-theme))
          (tui-icons  (treemacs-theme->tui-icons treemacs--current-theme))
          (gui-icon   (car icon-pair))
          (tui-icon   (cdr icon-pair)))
     ,(unless file
        `(progn
           (ignore icon-path)
           (ignore icons-dir)))
     ;; prefer to have icons as empty strings with a display property for compatibility
     ;; in e.g. dired, where an actual text icon would break `dired-goto-file-1'
     (unless (get-text-property 0 'display gui-icon)
       (setf gui-icon (propertize " " 'display gui-icon)))
     (dolist (ext xs)
       (when (symbolp ext)
         (-let [symbol (intern (format "treemacs-icon-%s" ext))]
           (add-to-list 'treemacs--icon-symbols ext)
           (set symbol nil))))
     (--each xs
       ;; NOTE: Disable creation of GUI svg icons without getting in the way of the rest
       ;; of the icon creation process. This is good enough a workaround for Emacs versions
       ;; that don't support svg images for as long as svg icons are a minority.
       (unless (and ,file
                    (not (image-type-available-p 'svg))
                    (string= (treemacs--file-extension ,file) "svg"))
         (ht-set! gui-icons it gui-icon))
       (ht-set! tui-icons it tui-icon))))

(treemacs-create-theme "Default"
  :icon-directory (treemacs-join-path treemacs-dir "icons/default")
  :config
  (progn
    ;; directory and other icons
    (treemacs-create-icon :file "vsc/root-closed.png"   :extensions (root-closed) :fallback "")
    (treemacs-create-icon :file "vsc/root-open.png"     :extensions (root-open)   :fallback "")
    (treemacs-create-icon :file "vsc/dir-closed.png"    :extensions (dir-closed)  :fallback (propertize "+ " 'face 'treemacs-term-node-face))
    (treemacs-create-icon :file "vsc/dir-open.png"      :extensions (dir-open)    :fallback (propertize "- " 'face 'treemacs-term-node-face))
    (treemacs-create-icon :file "tags-leaf.png"         :extensions (tag-leaf)    :fallback (propertize "• " 'face 'font-lock-constant-face))
    (treemacs-create-icon :file "tags-open.png"         :extensions (tag-open)    :fallback (propertize "▸ " 'face 'font-lock-string-face))
    (treemacs-create-icon :file "tags-closed.png"       :extensions (tag-closed)  :fallback (propertize "▾ " 'face 'font-lock-string-face))
    (treemacs-create-icon :file "error.png"             :extensions (error)       :fallback (propertize "• " 'face 'font-lock-string-face))
    (treemacs-create-icon :file "warning.png"           :extensions (warning)     :fallback (propertize "• " 'face 'font-lock-string-face))
    (treemacs-create-icon :file "info.png"              :extensions (info)        :fallback (propertize "• " 'face 'font-lock-string-face))
    (treemacs-create-icon :file "bookmark.png"          :extensions (bookmark)    :fallback " ")
    (treemacs-create-icon :file "svgrepo/screen.png"    :extensions (screen)      :fallback " ")
    (treemacs-create-icon :file "svgrepo/house.png"     :extensions (house)       :fallback " ")
    (treemacs-create-icon :file "svgrepo/list.png"      :extensions (list)        :fallback " ")
    (treemacs-create-icon :file "svgrepo/repeat.png"    :extensions (repeat)      :fallback " ")
    (treemacs-create-icon :file "svgrepo/suitcase.png"  :extensions (suitcase)    :fallback " ")
    (treemacs-create-icon :file "svgrepo/close.png"     :extensions (close)       :fallback " ")
    (treemacs-create-icon :file "svgrepo/cal.png"       :extensions (calendar)    :fallback " ")
    (treemacs-create-icon :file "svgrepo/briefcase.png" :extensions (briefcase)   :fallback " ")
    (treemacs-create-icon :file "svgrepo/mail.png"      :extensions (mail)        :fallback " ")
    (treemacs-create-icon :file "svgrepo/mail-plus.png" :extensions (mail-plus)   :fallback " ")

    ;; custom dir icons
    (treemacs-create-icon :file "svgrepo/dir-src-closed.png"    :extensions ("src-closed")     :fallback (propertize "+ " 'face 'treemacs-term-node-face))
    (treemacs-create-icon :file "svgrepo/dir-src-open.png"      :extensions ("src-open")       :fallback (propertize "- " 'face 'treemacs-term-node-face))
    (treemacs-create-icon :file "svgrepo/dir-test-closed.png"   :extensions ("test-closed")    :fallback (propertize "+ " 'face 'treemacs-term-node-face))
    (treemacs-create-icon :file "svgrepo/dir-test-open.png"     :extensions ("test-open")      :fallback (propertize "- " 'face 'treemacs-term-node-face))
    (treemacs-create-icon :file "vsc/dir-binary-closed.png"     :extensions ("bin-closed")     :fallback (propertize "+ " 'face 'treemacs-term-node-face))
    (treemacs-create-icon :file "vsc/dir-binary-open.png"       :extensions ("bin-open")       :fallback (propertize "- " 'face 'treemacs-term-node-face))
    (treemacs-create-icon :file "vsc/dir-services-closed.png"   :extensions ("build-closed")   :fallback (propertize "+ " 'face 'treemacs-term-node-face))
    (treemacs-create-icon :file "vsc/dir-services-open.png"     :extensions ("build-open")     :fallback (propertize "- " 'face 'treemacs-term-node-face))
    (treemacs-create-icon :file "svgrepo/dir-git-closed.png"    :extensions ("git-closed")     :fallback (propertize "+ " 'face 'treemacs-term-node-face))
    (treemacs-create-icon :file "svgrepo/dir-git-open.png"      :extensions ("git-open")       :fallback (propertize "- " 'face 'treemacs-term-node-face))
    (treemacs-create-icon :file "vsc/dir-github-closed.png"     :extensions ("github-closed")  :fallback (propertize "+ " 'face 'treemacs-term-node-face))
    (treemacs-create-icon :file "vsc/dir-github-open.png"       :extensions ("github-open")    :fallback (propertize "- " 'face 'treemacs-term-node-face))
    (treemacs-create-icon :file "vsc/dir-public-closed.png"     :extensions ("public-closed")  :fallback (propertize "+ " 'face 'treemacs-term-node-face))
    (treemacs-create-icon :file "vsc/dir-public-open.png"       :extensions ("public-open")    :fallback (propertize "- " 'face 'treemacs-term-node-face))
    (treemacs-create-icon :file "vsc/dir-private-closed.png"    :extensions ("private-closed") :fallback (propertize "+ " 'face 'treemacs-term-node-face))
    (treemacs-create-icon :file "vsc/dir-private-open.png"      :extensions ("private-open")   :fallback (propertize "- " 'face 'treemacs-term-node-face))
    (treemacs-create-icon
     :file "vsc/dir-temp-closed.png" :extensions ("temp-closed" "tmp-closed")
     :fallback (propertize "+ " 'face 'treemacs-term-node-face))
    (treemacs-create-icon
     :file "vsc/dir-temp-open.png" :extensions ("temp-open" "tmp-open")
     :fallback (propertize "- " 'face 'treemacs-term-node-face))
    (treemacs-create-icon
     :file "vsc/dir-docs-closed.png" :extensions ("readme-closed" "docs-closed")
     :fallback (propertize "+ " 'face 'treemacs-term-node-face))
    (treemacs-create-icon
     :file "vsc/dir-docs-open.png" :extensions ("readme-open" "docs-open")
     :fallback (propertize "- " 'face 'treemacs-term-node-face))
    (treemacs-create-icon
     :file "vsc/dir-images-closed.png" :extensions ("screenshots-closed" "icons-closed")
     :fallback (propertize "+ " 'face 'treemacs-term-node-face))
    (treemacs-create-icon
     :file "vsc/dir-images-open.png" :extensions ("screenshots-open" "icons-open")
     :fallback (propertize "- " 'face 'treemacs-term-node-face))

    ;; file icons
    (treemacs-create-icon :file "txt.png"           :extensions (fallback))
    (treemacs-create-icon :file "emacs.png"         :extensions ("el" "elc" "eln"))
    (treemacs-create-icon :file "ledger.png"        :extensions ("ledger" "beancount"))
    (treemacs-create-icon :file "yaml.png"          :extensions ("yml" "yaml" "travis.yml"))
    (treemacs-create-icon
     :file "shell.png"
     :extensions ("sh" "zsh" "zshrc" "zshenv" "fish" "zprofile" "zlogin" "zlogout" "bash"
                  "bash_profile" "bashrc" "bash_login" "profile" "bash_aliases"))
    (treemacs-create-icon :file "pdf.png"           :extensions ("pdf"))
    (treemacs-create-icon :file "c.png"             :extensions ("c" "h"))
    (treemacs-create-icon :file "haskell.png"       :extensions ("hs" "lhs"))
    (treemacs-create-icon :file "cabal.png"         :extensions ("cabal"))
    (treemacs-create-icon :file "lock.png"          :extensions ("lock"))
    (treemacs-create-icon :file "python.png"        :extensions ("py" "pyc"))
    (treemacs-create-icon :file "markdown.png"      :extensions ("md"))
    (treemacs-create-icon :file "asciidoc.png"      :extensions ("adoc" "asciidoc"))
    (treemacs-create-icon :file "rust.png"          :extensions ("rs"))
    (treemacs-create-icon :file "image.png"         :extensions ("jpg" "jpeg" "bmp" "svg" "png" "xpm" "gif"))
    (treemacs-create-icon :file "clojure.png"       :extensions ("clj" "cljs" "cljc" "edn"))
    (treemacs-create-icon :file "ts.png"            :extensions ("ts" "tsx"))
    (treemacs-create-icon :file "vue.png"           :extensions ("vue"))
    (treemacs-create-icon :file "css.png"           :extensions ("css"))
    (treemacs-create-icon :file "conf.png"          :extensions ("properties" "conf" "config" "cfg" "ini" "xdefaults" "xresources" "terminalrc" "ledgerrc"))
    (treemacs-create-icon :file "html.png"          :extensions ("html" "htm"))
    (treemacs-create-icon :file "git.png"           :extensions ("git" "gitignore" "gitconfig" "gitmodules" "gitattributes"))
    (treemacs-create-icon :file "dart.png"          :extensions ("dart"))
    (treemacs-create-icon :file "jar.png"           :extensions ("jar"))
    (treemacs-create-icon :file "kotlin.png"        :extensions ("kt"))
    (treemacs-create-icon :file "scala.png"         :extensions ("scala"))
    (treemacs-create-icon :file "gradle.png"        :extensions ("gradle"))
    (treemacs-create-icon :file "sbt.png"           :extensions ("sbt"))
    (treemacs-create-icon :file "go.png"            :extensions ("go"))
    (treemacs-create-icon :file "systemd.png"       :extensions ("service" "timer"))
    (treemacs-create-icon :file "php.png"           :extensions ("php"))
    (treemacs-create-icon :file "js.png"            :extensions ("js" "jsx"))
    (treemacs-create-icon :file "babel.png"         :extensions ("babel"))
    (treemacs-create-icon :file "hy.png"            :extensions ("hy"))
    (treemacs-create-icon :file "json.png"          :extensions ("json"))
    (treemacs-create-icon :file "julia.png"         :extensions ("jl"))
    (treemacs-create-icon :file "elx.png"           :extensions ("ex"))
    (treemacs-create-icon :file "elx-light.png"     :extensions ("exs" "eex" "leex" "heex"))
    (treemacs-create-icon :file "ocaml.png"         :extensions ("ml" "mli" "merlin" "ocaml"))
    (treemacs-create-icon :file "direnv.png"        :extensions ("envrc"))
    (treemacs-create-icon :file "puppet.png"        :extensions ("pp"))
    (treemacs-create-icon :file "docker.png"        :extensions ("dockerfile" "docker-compose.yml"))
    (treemacs-create-icon :file "vagrant.png"       :extensions ("vagrantfile"))
    (treemacs-create-icon :file "jinja2.png"        :extensions ("j2" "jinja2"))
    (treemacs-create-icon :file "video.png"         :extensions ("webm" "mp4" "avi" "mkv" "flv" "mov" "wmv" "mpg" "mpeg" "mpv"))
    (treemacs-create-icon :file "audio.png"         :extensions ("mp3" "ogg" "oga" "wav" "flac"))
    (treemacs-create-icon :file "tex.png"           :extensions ("tex"))
    (treemacs-create-icon :file "racket.png"        :extensions ("racket" "rkt" "rktl" "rktd" "scrbl" "scribble" "plt"))
    (treemacs-create-icon :file "erlang.png"        :extensions ("erl" "hrl"))
    (treemacs-create-icon :file "purescript.png"    :extensions ("purs"))
    (treemacs-create-icon :file "nix.png"           :extensions ("nix"))
    (treemacs-create-icon :file "project.png"       :extensions ("project"))
    (treemacs-create-icon :file "scons.png"         :extensions ("sconstruct" "sconstript"))
    (treemacs-create-icon :file "vsc/make.png"      :extensions ("makefile"))
    (treemacs-create-icon :file "vsc/license.png"   :extensions ("license"))
    (treemacs-create-icon :file "vsc/zip.png"       :extensions ("zip" "7z" "tar" "gz" "rar" "tar.gz"))
    (treemacs-create-icon :file "vsc/elm.png"       :extensions ("elm"))
    (treemacs-create-icon :file "vsc/xml.png"       :extensions ("xml" "xsl"))
    (treemacs-create-icon :file "vsc/access.png"    :extensions ("accdb" "accdt" "accdt"))
    (treemacs-create-icon :file "vsc/ascript.png"   :extensions ("actionscript"))
    (treemacs-create-icon :file "vsc/ai.png"        :extensions ("ai"))
    (treemacs-create-icon :file "vsc/alaw.png"      :extensions ("al"))
    (treemacs-create-icon :file "vsc/angular.png"   :extensions ("angular-cli.json" "angular.json"))
    (treemacs-create-icon :file "vsc/ansible.png"   :extensions ("ansible"))
    (treemacs-create-icon :file "vsc/antlr.png"     :extensions ("antlr"))
    (treemacs-create-icon :file "vsc/any.png"       :extensions ("anyscript"))
    (treemacs-create-icon :file "vsc/apache.png"    :extensions ("apacheconf"))
    (treemacs-create-icon :file "vsc/apple.png"     :extensions ("applescript"))
    (treemacs-create-icon :file "vsc/appveyor.png"  :extensions ("appveyor.yml"))
    (treemacs-create-icon :file "vsc/arduino.png"   :extensions ("ino" "pde"))
    (treemacs-create-icon :file "vsc/asp.png"       :extensions ("asp"))
    (treemacs-create-icon :file "vsc/asm.png"       :extensions ("asm" "arm"))
    (treemacs-create-icon :file "vsc/autohk.png"    :extensions ("ahk"))
    (treemacs-create-icon :file "vsc/babel.png"     :extensions ("babelrc" "babelignore" "babelrc.js" "babelrc.json" "babel.config.js"))
    (treemacs-create-icon :file "vsc/bat.png"       :extensions ("bat"))
    (treemacs-create-icon :file "vsc/binary.png"    :extensions ("exe" "dll" "obj" "so" "o"))
    (treemacs-create-icon :file "vsc/bazel.png"     :extensions ("bazelrc" "bazel"))
    (treemacs-create-icon :file "vsc/bower.png"     :extensions ("bowerrc" "bower.json"))
    (treemacs-create-icon :file "vsc/bundler.png"   :extensions ("gemfile" "gemfile.lock"))
    (treemacs-create-icon :file "vsc/cargo.png"     :extensions ("cargo.toml" "cargo.lock"))
    (treemacs-create-icon :file "vsc/cert.png"      :extensions ("csr" "crt" "cer" "der" "pfx" "p12" "p7b" "p7r" "src" "crl" "sst" "stl"))
    (treemacs-create-icon :file "vsc/class.png"     :extensions ("class"))
    (treemacs-create-icon :file "vsc/cmake.png"     :extensions ("cmake" "cmake-cache"))
    (treemacs-create-icon :file "vsc/cobol.png"     :extensions ("cobol"))
    (treemacs-create-icon :file "vsc/cfscript.png"  :extensions ("coffeescript"))
    (treemacs-create-icon :file "vsc/cpp.png"       :extensions ("cpp" "cxx" "tpp" "cc"))
    (treemacs-create-icon :file "vsc/cpph.png"      :extensions ("hpp" "hxx" "hh"))
    (treemacs-create-icon :file "vsc/cucumber.png"  :extensions ("feature"))
    (treemacs-create-icon :file "vsc/cython.png"    :extensions ("cython"))
    (treemacs-create-icon :file "vsc/delphi.png"    :extensions ("pascal" "objectpascal"))
    (treemacs-create-icon :file "vsc/django.png"    :extensions ("djt" "django-html" "django-txt"))
    (treemacs-create-icon :file "vsc/dlang.png"     :extensions ("d" "dscript" "dml" "diet"))
    (treemacs-create-icon :file "vsc/diff.png"      :extensions ("diff"))
    (treemacs-create-icon :file "vsc/editorcfg.png" :extensions ("editorconfig"))
    (treemacs-create-icon :file "vsc/erb.png"       :extensions ("erb"))
    (treemacs-create-icon :file "vsc/eslint.png"    :extensions ("eslintrc" "eslintignore" "eslintcache"))
    (treemacs-create-icon :file "vsc/excel.png"     :extensions ("xls" "xlsx" "xlsm" "ods" "fods"))
    (treemacs-create-icon :file "vsc/font.png"      :extensions ("woff" "woff2" "ttf" "otf" "eot" "pfa" "pfb" "sfd"))
    (treemacs-create-icon :file "vsc/fortran.png"   :extensions ("fortran" "fortran-modern" "fortranfreeform"))
    (treemacs-create-icon :file "vsc/fsharp.png"    :extensions ("fsharp"))
    (treemacs-create-icon :file "vsc/fsproj.png"    :extensions ("fsproj"))
    (treemacs-create-icon :file "vsc/godot.png"     :extensions ("gdscript"))
    (treemacs-create-icon :file "vsc/graphql.png"   :extensions ("graphql"))
    (treemacs-create-icon :file "vsc/helm.png"      :extensions ("helm"))
    (treemacs-create-icon :file "vsc/java.png"      :extensions ("java"))
    (treemacs-create-icon :file "vsc/jenkins.png"   :extensions ("jenkins"))
    (treemacs-create-icon :file "vsc/jupyter.png"   :extensions ("ipynb"))
    (treemacs-create-icon :file "vsc/key.png"       :extensions ("key" "pem"))
    (treemacs-create-icon :file "vsc/less.png"      :extensions ("less"))
    (treemacs-create-icon :file "vsc/locale.png"    :extensions ("locale"))
    (treemacs-create-icon :file "vsc/manifest.png"  :extensions ("manifest"))
    (treemacs-create-icon :file "vsc/maven.png"     :extensions ("pom.xml" "maven.config" "extensions.xml" "settings.xml"))
    (treemacs-create-icon :file "vsc/meson.png"     :extensions ("meson" "meson.build"))
    (treemacs-create-icon :file "vsc/nginx.png"     :extensions ("nginx.conf" "nginx"))
    (treemacs-create-icon :file "vsc/npm.png"       :extensions ("npmignore" "npmrc" "package.json" "package-lock.json" "npm-shrinwrap.json"))
    (treemacs-create-icon :file "vsc/wasm.png"      :extensions ("wasm" "wat"))
    (treemacs-create-icon :file "vsc/yarn.png"      :extensions ("yarn.lock" "yarnrc" "yarnclean" "yarn-integrity" "yarn-metadata.json" "yarnignore"))
    (treemacs-create-icon :file "vsc/pkg.png"       :extensions ("pkg"))
    (treemacs-create-icon :file "vsc/patch.png"     :extensions ("patch"))
    (treemacs-create-icon :file "vsc/perl6.png"     :extensions ("perl6"))
    (treemacs-create-icon :file "vsc/pgsql.png"     :extensions ("pgsql"))
    (treemacs-create-icon :file "vsc/phpunit.png"   :extensions ("phpunit" "phpunit.xml"))
    (treemacs-create-icon :file "vsc/pip.png"       :extensions ("pipfile" "pipfile.lock" "pip-requirements"))
    (treemacs-create-icon :file "vsc/plsql.png"     :extensions ("plsql" "orcale"))
    (treemacs-create-icon :file "vsc/pp.png"        :extensions ("pot" "potx" "potm" "pps" "ppsx" "ppsm" "ppt" "pptx" "pptm" "pa" "ppa" "ppam" "sldm" "sldx"))
    (treemacs-create-icon :file "vsc/prettier.png"  :extensions ("prettier.config.js" "prettierrc.js" "prettierrc.json" "prettierrc.yml" "prettierrc.yaml"))
    (treemacs-create-icon :file "vsc/prolog.png"    :extensions ("pro" "prolog"))
    (treemacs-create-icon :file "vsc/protobuf.png"  :extensions ("proto" "proto3"))
    (treemacs-create-icon :file "vsc/rake.png"      :extensions ("rake" "rakefile"))
    (treemacs-create-icon :file "vsc/sqlite.png"    :extensions ("sqlite" "db3" "sqlite3"))
    (treemacs-create-icon :file "vsc/swagger.png"   :extensions ("swagger"))
    (treemacs-create-icon :file "vsc/swift.png"     :extensions ("swift"))
    (treemacs-create-icon :file "vsc/ruby.png"      :extensions ("rb"))
    (treemacs-create-icon :file "vsc/scss.png"      :extensions ("scss"))
    (treemacs-create-icon :file "vsc/lua.png"       :extensions ("lua"))
    (treemacs-create-icon :file "vsc/log.png"       :extensions ("log"))
    (treemacs-create-icon :file "vsc/lisp.png"      :extensions ("lisp"))
    (treemacs-create-icon :file "vsc/sql.png"       :extensions ("sql"))
    (treemacs-create-icon :file "vsc/toml.png"      :extensions ("toml"))
    (treemacs-create-icon :file "vsc/nim.png"       :extensions ("nim"))
    (treemacs-create-icon :file "vsc/org.png"       :extensions ("org" "org_archive"))
    (treemacs-create-icon :file "vsc/perl.png"      :extensions ("pl" "pm" "perl"))
    (treemacs-create-icon :file "vsc/vim.png"       :extensions ("vimrc" "tridactylrc" "vimperatorrc" "ideavimrc" "vrapperrc"))
    (treemacs-create-icon :file "vsc/deps.png"      :extensions ("cask"))
    (treemacs-create-icon :file "vsc/r.png"         :extensions ("r"))
    (treemacs-create-icon :file "vsc/terraform.png" :extensions ("tf" "terraform"))
    (treemacs-create-icon :file "vsc/reason.png"    :extensions ("re" "rei"))))

(define-inline treemacs-icon-for-file (file)
  "Retrieve an icon for FILE from `treemacs-icons' based on its extension.
Works only with files, not directories.
Uses `treemacs-icon-fallback' as fallback."
  (declare (side-effect-free t))
  (inline-letevals (file)
    (inline-quote
     (let ((file-downcased (-> ,file (treemacs--filename) (downcase))))
       (or (ht-get treemacs-icons file-downcased)
           (ht-get treemacs-icons
                   (treemacs--file-extension file-downcased)
                   (with-no-warnings treemacs-icon-fallback)))))))

(define-inline treemacs-icon-for-dir (dir state)
  "Retrieve an icon for DIR from `treemacs-icons' in given STATE.
STATE must be either `open' or `closed'.

Works only with directories, not files.
Uses the `dir-open' and `dir-closed' icons as fallback."
  (declare (side-effect-free t))
  (inline-letevals (dir state)
    (inline-quote
     (let ((name-downcased (-> ,dir (treemacs--filename) (downcase))))
       (when (eq ?. (aref name-downcased 0))
         (setf name-downcased (substring name-downcased 1)))
       (pcase-exhaustive ,state
         (`open
          (let ((name (format "%s-%s" name-downcased "open")))
            (or (ht-get treemacs-icons name)
                (ht-get treemacs-icons 'dir-open))))
         (`closed
          (let ((name (format "%s-%s" name-downcased "closed")))
            (or (ht-get treemacs-icons name)
                (ht-get treemacs-icons 'dir-closed)))))))))

;;;###autoload
(defun treemacs-resize-icons (size)
  "Resize the current theme's icons to the given SIZE.

If SIZE is \\='nil' the icons are not resized and will retain their default size
of 22 pixels.

There is only one size, the icons are square and the aspect ratio will be
preserved when resizing them therefore width and height are the same.

Resizing the icons only works if Emacs was built with ImageMagick support, or if
using Emacs >= 27.1,which has native image resizing support.  If this is not the
case this function will not have any effect.

Custom icons are not taken into account, only the size of treemacs' own icons
png are changed."
  (interactive "nIcon size in pixels: ")
  (if (not (or  (and (functionp 'image-transforms-p) (member 'scale (image-transforms-p)))
                (image-type-available-p 'imagemagick)))
      (treemacs-log-failure "Icons cannot be resized without image transforms or imagemagick support.")
    (setq treemacs--icon-size size)
    (treemacs--maphash (treemacs-theme->gui-icons treemacs--current-theme) (_ icon)
      (let ((display (get-text-property 0 'display icon))
            (width   treemacs--icon-size)
            (height  treemacs--icon-size))
        (when (eq 'image (car-safe display))
          (when (s-ends-with? "root.png" (plist-get (cdr display) :file))
            (treemacs--root-icon-size-adjust width height))
          (plist-put (cdr display) :height height)
          (plist-put (cdr display) :width width))))))

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

(define-inline treemacs-current-icons (&optional tui)
  "Return the current theme's icons.
Return the fallback icons if TUI is non-nil."
  (inline-letevals (tui)
    (inline-quote
     (if ,tui
         (treemacs-theme->tui-icons treemacs--current-theme)
       (treemacs-theme->gui-icons treemacs--current-theme)))))

;;;###autoload
(defun treemacs-define-custom-icon (icon &rest file-extensions)
  "Define a custom ICON for the current theme to use for FILE-EXTENSIONS.

Note that treemacs has a very loose definition of what constitutes a file
extension - it's either everything past the last period, or just the file's full
name if there is no period.  This makes it possible to match file names like
\\='.gitignore' and \\='Makefile'.

Additionally FILE-EXTENSIONS are also not case sensitive and will be stored in a
down-cased state."
  (unless icon
    (user-error "Custom icon cannot be nil"))
  (dolist (ext file-extensions)
    (ht-set! (treemacs-theme->gui-icons treemacs--current-theme)
             (downcase ext)
             (concat icon " "))))

;;;###autoload
(defun treemacs-define-custom-image-icon (file &rest file-extensions)
  "Same as `treemacs-define-custom-icon' but for image icons instead of strings.
FILE is the path to an icon image (and not the actual icon string).
FILE-EXTENSIONS are all the (not case-sensitive) file extensions the icon
should be used for."
  (unless file
    (user-error "Custom icon cannot be nil"))
  (-let [icon (car (treemacs--create-icon-strings file " "))]
    (dolist (ext file-extensions)
      (ht-set! (treemacs-theme->gui-icons treemacs--current-theme)
               (downcase ext)
               icon))))

;;;###autoload
(defun treemacs-map-icons-with-auto-mode-alist (extensions mode-icon-alist)
  "Remaps icons for EXTENSIONS according to `auto-mode-alist'.
EXTENSIONS should be a list of file extensions such that they match the regex
stored in `auto-mode-alist', for example \\='(\".cc\").
MODE-ICON-ALIST is an alist that maps which mode from `auto-mode-alist' should
be assigned which treemacs icon, for example
`((c-mode . ,(treemacs-get-icon-value \"c\"))
  (c++-mode . ,(treemacs-get-icon-value \"cpp\")))"
  (dolist (extension extensions)
    (-when-let* ((mode (cdr (--first (s-matches? (car it) extension) auto-mode-alist)))
                 (icon (cdr (assq mode mode-icon-alist))))
      (ht-set! (treemacs-theme->gui-icons treemacs--current-theme)
               (substring extension 1)
               icon))))

(treemacs-only-during-init
  (treemacs-load-theme "Default"))

(provide 'treemacs-icons)

;;; treemacs-icons.el ends here
