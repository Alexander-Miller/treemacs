;;; treemacs-all-the-icons.el --- all-the-icons integration for treemacs -*- lexical-binding: t -*-

;; Copyright (C) 2022 Alexander Miller

;; Author: Eric Dallo <ercdll1337@gmail.com>
;; Package-Requires: ((emacs "26.1") (all-the-icons "4.0.1") (treemacs "0.0"))
;; Version: 0
;; Homepage: https://github.com/Alexander-Miller/treemacs

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
;;; all-the-icons integration.
;;
;;; Code:

(require 'all-the-icons)
(require 'treemacs)

(defface treemacs-all-the-icons-root-face
  '((t (:inherit font-lock-string-face)))
  "Face used for the root icon in all-the-icons theme."
  :group 'treemacs-faces)

(defface treemacs-all-the-icons-file-face
  '((t (:inherit font-lock-doc-face)))
  "Face used for the directory and file icons in all-the-icons theme."
  :group 'treemacs-faces)

(defvar treemacs-all-the-icons-tab (if (bound-and-true-p treemacs-all-the-icons-tab-font)
                                       (propertize "\t" 'face `((:family ,treemacs-all-the-icons-tab-font)))
                                     "\t"))

(treemacs-create-theme "all-the-icons"
  :config
  (progn
    (dolist (item all-the-icons-extension-icon-alist)
      (let ((extensions (list (nth 0 item)))
            (fn (nth 1 item))
            (key (nth 2 item))
            (plist (nthcdr 3 item)))
        (treemacs-create-icon
         :icon (format "  %s%s" (apply fn key plist) treemacs-all-the-icons-tab)
         :extensions extensions)))

    ;; directory and other icons
    (treemacs-create-icon :icon (format "%s%s" (all-the-icons-octicon "repo" :height 1.2 :v-adjust -0.1 :face 'treemacs-all-the-icons-root-face) treemacs-all-the-icons-tab)
                          :extensions (root-closed root-open)
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s%s%s" (all-the-icons-octicon "chevron-down" :height 0.75 :v-adjust 0.1 :face 'treemacs-all-the-icons-file-face) treemacs-all-the-icons-tab (all-the-icons-octicon "file-directory" :v-adjust 0 :face 'treemacs-all-the-icons-file-face) treemacs-all-the-icons-tab)
                          :extensions (dir-open)
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s%s%s" (all-the-icons-octicon "chevron-right" :height 0.75 :v-adjust 0.1 :face 'treemacs-all-the-icons-file-face) treemacs-all-the-icons-tab (all-the-icons-octicon "file-directory" :v-adjust 0 :face 'treemacs-all-the-icons-file-face) treemacs-all-the-icons-tab)
                          :extensions (dir-closed)
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s%s%s" (all-the-icons-octicon "chevron-down" :height 0.75 :v-adjust 0.1 :face 'treemacs-all-the-icons-file-face) treemacs-all-the-icons-tab (all-the-icons-octicon "package" :v-adjust 0 :face 'treemacs-all-the-icons-file-face) treemacs-all-the-icons-tab)
                          :extensions (tag-open)
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s%s%s" (all-the-icons-octicon "chevron-right" :height 0.75 :v-adjust 0.1 :face 'treemacs-all-the-icons-file-face) treemacs-all-the-icons-tab (all-the-icons-octicon "package" :v-adjust 0 :face 'treemacs-all-the-icons-file-face) treemacs-all-the-icons-tab)
                          :extensions (tag-closed)
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s" (all-the-icons-octicon "tag" :height 0.9 :v-adjust 0 :face 'treemacs-all-the-icons-file-face) treemacs-all-the-icons-tab)
                          :extensions (tag-leaf)
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s" (all-the-icons-octicon "flame" :v-adjust 0 :face 'all-the-icons-red) treemacs-all-the-icons-tab)
                          :extensions (error)
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s" (all-the-icons-octicon "stop" :v-adjust 0 :face 'all-the-icons-yellow) treemacs-all-the-icons-tab)
                          :extensions (warning)
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s" (all-the-icons-octicon "info" :height 0.75 :v-adjust 0.1 :face 'all-the-icons-blue) treemacs-all-the-icons-tab)
                          :extensions (info)
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s" (all-the-icons-material "mail" :height 0.75 :v-adjust 0.1 :face 'treemacs-all-the-icons-file-face) treemacs-all-the-icons-tab)
                          :extensions (mail)
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s" (all-the-icons-octicon "bookmark" :height 0.75 :v-adjust 0.1 :face 'treemacs-all-the-icons-file-face) treemacs-all-the-icons-tab)
                          :extensions (bookmark)
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s" (all-the-icons-material "computer" :height 0.75 :v-adjust 0.1 :face 'treemacs-all-the-icons-file-face) treemacs-all-the-icons-tab)
                          :extensions (screen)
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s" (all-the-icons-material "home" :height 0.75 :v-adjust 0.1 :face 'treemacs-all-the-icons-file-face) treemacs-all-the-icons-tab)
                          :extensions (house)
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s" (all-the-icons-material "menu" :height 0.75 :v-adjust 0.1 :face 'treemacs-all-the-icons-file-face) treemacs-all-the-icons-tab)
                          :extensions (list)
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s" (all-the-icons-material "repeat" :height 0.75 :v-adjust 0.1 :face 'treemacs-all-the-icons-file-face) treemacs-all-the-icons-tab)
                          :extensions (repeat)
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s" (all-the-icons-faicon "suitcase" :height 0.75 :v-adjust 0.1 :face 'treemacs-all-the-icons-file-face) treemacs-all-the-icons-tab)
                          :extensions (suitcase)
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s" (all-the-icons-material "close" :height 0.75 :v-adjust 0.1 :face 'treemacs-all-the-icons-file-face) treemacs-all-the-icons-tab)
                          :extensions (close)
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s" (all-the-icons-octicon "calendar" :height 0.75 :v-adjust 0.1 :face 'treemacs-all-the-icons-file-face) treemacs-all-the-icons-tab)
                          :extensions (calendar)
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s" (all-the-icons-faicon "briefcase" :height 0.75 :v-adjust 0.1 :face 'treemacs-all-the-icons-file-face) treemacs-all-the-icons-tab)
                          :extensions (briefcase)
                          :fallback 'same-as-icon)

    ;; file icons - remove icon if the extension appears in all-the-icons-icon-extension-alist
    (treemacs-create-icon :icon (format "  %s%s" (all-the-icons-fileicon "access" :v-adjust 0 :face 'all-the-icons-red) treemacs-all-the-icons-tab) :extensions ("accdb" "accdt") :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "  %s%s" (all-the-icons-fileicon "actionscript" :v-adjust 0 :face 'all-the-icons-red) treemacs-all-the-icons-tab) :extensions ("actionscript") :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "  %s%s" (all-the-icons-fileicon "ansible" :v-adjust 0 :face 'all-the-icons-dsilver) treemacs-all-the-icons-tab) :extensions ("ansible") :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "  %s%s" (all-the-icons-fileicon "antlr" :v-adjust 0 :face 'all-the-icons-orange) treemacs-all-the-icons-tab) :extensions ("antlr") :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "  %s%s" (all-the-icons-fileicon "apple" :v-adjust 0 :face 'all-the-icons-lsilver) treemacs-all-the-icons-tab) :extensions ("applescript") :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "  %s%s" (all-the-icons-fileicon "appveyor" :v-adjust 0 :face 'all-the-icons-lblue) treemacs-all-the-icons-tab) :extensions ("appveyor.yml") :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "  %s%s" (all-the-icons-fileicon "arduino" :v-adjust 0 :face 'all-the-icons-dgreen) treemacs-all-the-icons-tab) :extensions ("ino" "pde") :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "  %s%s" (all-the-icons-material "audiotrack" :v-adjust 0 :face 'all-the-icons-lblue) treemacs-all-the-icons-tab) :extensions ("midi") :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "  %s%s" (all-the-icons-fileicon "babel" :v-adjust 0 :face 'all-the-icons-yellow) treemacs-all-the-icons-tab) :extensions ("babelignore" "babelrc.js" "babelrc.json" "babel.config.js") :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "  %s%s" (all-the-icons-fileicon "bazel" :v-adjust 0 :face 'all-the-icons-green) treemacs-all-the-icons-tab) :extensions ("bazelrc" "bazel") :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "  %s%s" (all-the-icons-alltheicon "bower" :v-adjust 0 :face 'all-the-icons-orange) treemacs-all-the-icons-tab) :extensions ("bower.json") :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "  %s%s" (all-the-icons-fileicon "bundler" :v-adjust 0 :face 'all-the-icons-lblue) treemacs-all-the-icons-tab) :extensions ("gemfile" "gemfile.lock") :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "  %s%s" (all-the-icons-octicon "gear" :v-adjust 0 :face 'all-the-icons-lsilver) treemacs-all-the-icons-tab) :extensions ("bat") :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "  %s%s" (all-the-icons-alltheicon "cplusplus" :v-adjust 0 :face 'all-the-icons-purple) treemacs-all-the-icons-tab) :extensions ("tpp") :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "  %s%s" (all-the-icons-alltheicon "clojure" :v-adjust 0 :face 'all-the-icons-blue) treemacs-all-the-icons-tab) :extensions ("edn") :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "  %s%s" (all-the-icons-fileicon "cmake" :v-adjust 0 :face 'all-the-icons-green) treemacs-all-the-icons-tab) :extensions ("cmake-cache") :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "  %s%s" (all-the-icons-fileicon "cobol" :v-adjust 0 :face 'all-the-icons-dblue) treemacs-all-the-icons-tab) :extensions ("cobol") :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "  %s%s" (all-the-icons-alltheicon "coffeescript" :v-adjust 0 :face 'all-the-icons-dorange) treemacs-all-the-icons-tab) :extensions ("coffeescript") :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "  %s%s" (all-the-icons-faicon "certificate" :v-adjust 0 :face 'all-the-icons-yellow) treemacs-all-the-icons-tab) :extensions ("csr" "crt" "cer" "der" "pfx" "p7b" "p7r" "src" "crl" "sst" "stl") :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "  %s%s" (all-the-icons-octicon "gear" :v-adjust 0 :face 'all-the-icons-lsilver) treemacs-all-the-icons-tab) :extensions ("conf" "properties" "config" "cfg" "xdefaults" "xresources" "terminalrc" "ledgerrc") :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "  %s%s" (all-the-icons-fileicon "cython" :v-adjust 0 :face 'all-the-icons-dblue) treemacs-all-the-icons-tab) :extensions ("cython") :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "  %s%s" (all-the-icons-alltheicon "dlang" :v-adjust 0 :face 'all-the-icons-red) treemacs-all-the-icons-tab) :extensions ("d" "dscript" "dml" "diet") :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "  %s%s" (all-the-icons-octicon "diff" :v-adjust 0 :face 'all-the-icons-lsilver) treemacs-all-the-icons-tab) :extensions ("diff") :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "  %s%s" (all-the-icons-fileicon "dockerfile" :v-adjust 0 :face 'all-the-icons-blue) treemacs-all-the-icons-tab) :extensions ("docker-compose.yml") :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "  %s%s" (all-the-icons-fileicon "editorconfig" :v-adjust 0 :face 'all-the-icons-lsilver) treemacs-all-the-icons-tab) :extensions ("editorconfig") :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "  %s%s" (all-the-icons-faicon "wrench" :v-adjust 0 :face 'all-the-icons-lpurple) treemacs-all-the-icons-tab) :extensions ("envrc") :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "  %s%s" (all-the-icons-fileicon "eslint" :v-adjust 0 :face 'all-the-icons-lpurple) treemacs-all-the-icons-tab) :extensions ("eslintrc" "eslintcache") :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "  %s%s" (all-the-icons-octicon "file-binary" :v-adjust 0 :face 'all-the-icons-lsilver) treemacs-all-the-icons-tab) :extensions ("exe" "obj" "so" "o") :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "  %s%s" (all-the-icons-alltheicon "elixir" :v-adjust 0 :face 'all-the-icons-dpurple) treemacs-all-the-icons-tab) :extensions ("heex") :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "  %s%s" (all-the-icons-fileicon "cucumber" :v-adjust 0 :face 'all-the-icons-dgreen) treemacs-all-the-icons-tab) :extensions ("feature") :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "  %s%s" (all-the-icons-fileicon "fortran" :v-adjust 0 :face 'all-the-icons-lsilver) treemacs-all-the-icons-tab) :extensions ("fortran" "fortran-modern" "fortranfreeform") :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "  %s%s" (all-the-icons-fileicon "fsharp" :v-adjust 0 :face 'all-the-icons-lblue) treemacs-all-the-icons-tab) :extensions ("fsharp") :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "  %s%s" (all-the-icons-fileicon "godot" :v-adjust 0 :face 'all-the-icons-dblue) treemacs-all-the-icons-tab) :extensions ("gdscript") :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "  %s%s" (all-the-icons-alltheicon "git" :height 0.85 :v-adjust 0 :face 'all-the-icons-red) treemacs-all-the-icons-tab) :extensions ("gitmodules" "gitconfig" "gitignore" "gitattributes") :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "  %s%s" (all-the-icons-fileicon "graphql" :v-adjust 0 :face 'all-the-icons-pink) treemacs-all-the-icons-tab) :extensions ("graphql") :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "  %s%s" (all-the-icons-fileicon "jenkins" :v-adjust 0 :face 'all-the-icons-lsilver) treemacs-all-the-icons-tab) :extensions ("jenkins") :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "  %s%s" (all-the-icons-alltheicon "java" :v-adjust 0 :face 'all-the-icons-orange) treemacs-all-the-icons-tab) :extensions ("jar") :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "  %s%s" (all-the-icons-faicon "balance-scale" :v-adjust 0 :face 'all-the-icons-purple) treemacs-all-the-icons-tab) :extensions ("ledger") :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "  %s%s" (all-the-icons-octicon "key" :v-adjust 0 :face 'all-the-icons-yellow) treemacs-all-the-icons-tab) :extensions ("license") :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "  %s%s" (all-the-icons-material "translate" :v-adjust 0 :face 'all-the-icons-blue) treemacs-all-the-icons-tab) :extensions ("locale") :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "  %s%s" (all-the-icons-material "lock" :v-adjust 0 :face 'all-the-icons-dred) treemacs-all-the-icons-tab) :extensions ("lock") :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "  %s%s" (all-the-icons-fileicon "gnu" :v-adjust 0 :face 'all-the-icons-dsilver) treemacs-all-the-icons-tab) :extensions ("makefile") :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "  %s%s" (all-the-icons-fileicon "meson" :v-adjust 0 :face 'all-the-icons-green) treemacs-all-the-icons-tab) :extensions ("meson") :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "  %s%s" (all-the-icons-fileicon "ocaml" :v-adjust 0 :face 'all-the-icons-lorange) treemacs-all-the-icons-tab) :extensions ("merlin" "ocaml") :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "  %s%s" (all-the-icons-faicon "file-video-o" :v-adjust 0 :face 'all-the-icons-lgreen) treemacs-all-the-icons-tab) :extensions ("avi") :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "  %s%s" (all-the-icons-fileicon "nginx" :v-adjust 0 :face 'all-the-icons-green) treemacs-all-the-icons-tab) :extensions ("nginx.conf") :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "  %s%s" (all-the-icons-fileicon "npm" :v-adjust 0 :face 'all-the-icons-red) treemacs-all-the-icons-tab) :extensions ("npmignore" "npmrc" "package.json" "package-lock.json" "npm-shrinwrap.json") :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "  %s%s" (all-the-icons-fileicon "delphi" :v-adjust 0 :face 'all-the-icons-red) treemacs-all-the-icons-tab) :extensions ("pascal" "objectpascal") :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "  %s%s" (all-the-icons-fileicon "patch" :v-adjust 0 :face 'all-the-icons-orange) treemacs-all-the-icons-tab) :extensions ("patch") :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "  %s%s" (all-the-icons-alltheicon "perl" :v-adjust 0 :face 'all-the-icons-dblue) treemacs-all-the-icons-tab) :extensions ("perl") :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "  %s%s" (all-the-icons-fileicon "phpunit" :v-adjust 0 :face 'all-the-icons-blue) treemacs-all-the-icons-tab) :extensions ("phpunit" "phpunit.xml") :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "  %s%s" (all-the-icons-alltheicon "postgresql" :v-adjust 0 :face 'all-the-icons-dblue) treemacs-all-the-icons-tab) :extensions ("pgsql") :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "  %s%s" (all-the-icons-fileicon "powerpoint" :v-adjust 0 :face 'all-the-icons-orange) treemacs-all-the-icons-tab) :extensions ("pot" "potx" "potm" "ppsx" "ppsm" "pptx" "pptm" "pa" "ppa" "ppam" "sldm" "sldx") :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "  %s%s" (all-the-icons-alltheicon "prolog" :v-adjust 0 :face 'all-the-icons-orange) treemacs-all-the-icons-tab) :extensions ("prolog") :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "  %s%s" (all-the-icons-fileicon "purescript" :v-adjust 0 :face 'all-the-icons-lblue) treemacs-all-the-icons-tab) :extensions ("purs") :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "  %s%s" (all-the-icons-fileicon "racket" :v-adjust 0 :face 'all-the-icons-dred) treemacs-all-the-icons-tab) :extensions ("racket" "rktd" "rktl" "scrbl" "scribble" "plt") :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "  %s%s" (all-the-icons-fileicon "sqlite" :v-adjust 0 :face 'all-the-icons-blue) treemacs-all-the-icons-tab) :extensions ("sqlite" "db3" "sqlite3") :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "  %s%s" (all-the-icons-fileicon "swagger" :v-adjust 0 :face 'all-the-icons-green) treemacs-all-the-icons-tab) :extensions ("swagger") :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "  %s%s" (all-the-icons-alltheicon "terminal" :v-adjust 0 :face 'all-the-icons-dgreen) treemacs-all-the-icons-tab)
                          :extensions ("zshrc" "zshenv" "zprofile" "zlogin" "zlogout" "bash"
                                       "bash_profile" "bash_login" "profile" "bash_aliases")
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "  %s%s" (all-the-icons-fileicon "tsx-alt" :v-adjust 0 :face 'all-the-icons-lgreen) treemacs-all-the-icons-tab) :extensions ("tsx") :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "  %s%s" (all-the-icons-fileicon "excel" :v-adjust 0 :face 'all-the-icons-green) treemacs-all-the-icons-tab) :extensions ("ods" "fods") :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "  %s%s" (all-the-icons-fileicon "wasm" :v-adjust 0 :face 'all-the-icons-lpurple) treemacs-all-the-icons-tab) :extensions ("wasm" "wat") :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "  %s%s" (all-the-icons-faicon "font" :v-adjust 0 :face 'all-the-icons-lsilver) treemacs-all-the-icons-tab) :extensions ("ttf" "otf" "eot" "pfa" "pfb" "sfd") :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "  %s%s" (all-the-icons-material "code" :v-adjust 0 :face 'all-the-icons-lpurple) treemacs-all-the-icons-tab) :extensions ("xsl") :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "  %s%s" (all-the-icons-fileicon "yarn" :v-adjust 0 :face 'all-the-icons-dblue) treemacs-all-the-icons-tab) :extensions ("yarn.lock" "yarnrc" "yarnclean" "yarn-integrity" "yarn-metadata.json" "yarnignore") :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "  %s%s" (all-the-icons-octicon "file-media" :v-adjust 0 :face 'all-the-icons-orange) treemacs-all-the-icons-tab)
                          :extensions ("tif" "tiff" "bmp" "psd" "ai" "eps" "indd") :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "  %s%s" (all-the-icons-octicon "file-code" :v-adjust 0 :face 'treemacs-all-the-icons-file-face) treemacs-all-the-icons-tab)
                          :extensions ("cask" "ideavimrc" "inputrc"
                                       "toml" "tridactylrc" "vh" "vimperatorrc"
                                       "vimrc" "vrapperrc")
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "  %s%s" (all-the-icons-octicon "book" :v-adjust 0 :face 'treemacs-all-the-icons-file-face) treemacs-all-the-icons-tab)
                          :extensions ("lrf" "lrx" "cbr" "cbz" "cb7" "cbt" "cba" "chm" "djvu"
                                       "doc" "docx" "pdb" "pdb" "fb2" "xeb" "ceb" "inf" "azw"
                                       "azw3" "kf8" "kfx" "lit" "prc" "mobi" "exe" "or"
                                       "pkg" "opf" "txt" "pdb" "ps" "rtf" "pdg" "xml" "tr2"
                                       "tr3" "oxps" "xps")
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "  %s%s" (all-the-icons-octicon "file-text" :v-adjust 0 :face 'treemacs-all-the-icons-file-face) treemacs-all-the-icons-tab)
                          :extensions ("markdown" "rst" "CONTRIBUTE" "LICENSE" "README" "CHANGELOG")
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "  %s%s" (all-the-icons-octicon "file-binary" :v-adjust 0 :face 'treemacs-all-the-icons-file-face) treemacs-all-the-icons-tab)
                          :extensions ("class" "exe" "obj" "so" "o" "out" "pyc")
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "  %s%s" (all-the-icons-octicon "file-zip" :v-adjust 0 :face 'treemacs-all-the-icons-file-face) treemacs-all-the-icons-tab)
                          :extensions ("tar" "rar" "tgz")
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "  %s%s" (all-the-icons-octicon "file-text" :v-adjust 0 :face 'treemacs-all-the-icons-file-face) treemacs-all-the-icons-tab)
                          :extensions (fallback)
                          :fallback 'same-as-icon)))

(provide 'treemacs-all-the-icons)

;;; treemacs-all-the-icons.el ends here
