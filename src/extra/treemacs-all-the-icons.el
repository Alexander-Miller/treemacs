;;; treemacs-all-the-icons.el --- all-the-icons integration for treemacs -*- lexical-binding: t -*-

;; Copyright (C) 2020 Alexander Miller

;; Author: Eric Dallo <ercdll1337@gmail.com>
;; Package-Requires: ((emacs "25") (all-the-icons "4.0.1") (treemacs "0.0"))
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

(treemacs-create-theme "all-the-icons"
  :config
  (progn
    ;; directory and other icons
    (treemacs-create-icon :icon (format " %s\t" (all-the-icons-octicon "repo" :height 1.2 :v-adjust -0.1 :face 'treemacs-all-the-icons-root-face))
                          :extensions (root)
                          :fallback "")
    (treemacs-create-icon :icon (format "%s\t%s\t"
                                        (all-the-icons-octicon "chevron-down" :height 0.75 :v-adjust 0.1 :face 'treemacs-all-the-icons-file-face)
                                        (all-the-icons-octicon "file-directory" :v-adjust 0 :face 'treemacs-all-the-icons-file-face))
                          :extensions (dir-open)
                          :fallback (propertize "+ " 'face 'treemacs-term-node-face))
    (treemacs-create-icon :icon (format "%s\t%s\t"
                                        (all-the-icons-octicon "chevron-right" :height 0.75 :v-adjust 0.1 :face 'treemacs-all-the-icons-file-face)
                                        (all-the-icons-octicon "file-directory" :v-adjust 0 :face 'treemacs-all-the-icons-file-face))
                          :extensions (dir-closed)
                          :fallback (propertize "- " 'face 'treemacs-term-node-face))
    (treemacs-create-icon :icon (format "%s\t%s\t"
                                        (all-the-icons-octicon "chevron-down" :height 0.75 :v-adjust 0.1 :face 'treemacs-all-the-icons-file-face)
                                        (all-the-icons-octicon "package" :v-adjust 0 :face 'treemacs-all-the-icons-file-face))
                          :extensions (tag-open)
                          :fallback (propertize "▸ " 'face 'font-lock-string-face))
    (treemacs-create-icon :icon (format "%s\t%s\t"
                                        (all-the-icons-octicon "chevron-right" :height 0.75 :v-adjust 0.1 :face 'treemacs-all-the-icons-file-face)
                                        (all-the-icons-octicon "package" :v-adjust 0 :face 'treemacs-all-the-icons-file-face))
                          :extensions (tag-closed)
                          :fallback (propertize "▾ " 'face 'font-lock-string-face))
    (treemacs-create-icon :icon (format "%s\t" (all-the-icons-octicon "tag" :height 0.9 :v-adjust 0 :face 'treemacs-all-the-icons-file-face))
                          :extensions (tag-leaf)
                          :fallback (propertize "• " 'face 'font-lock-constant-face))
    (treemacs-create-icon :icon (format "%s\t" (all-the-icons-octicon "flame" :v-adjust 0 :face 'all-the-icons-red))
                          :extensions (error)
                          :fallback (propertize "• " 'face 'all-the-icons-red))
    (treemacs-create-icon :icon (format "%s\t" (all-the-icons-octicon "stop" :v-adjust 0 :face 'all-the-icons-yellow))
                          :extensions (warning)
                          :fallback (propertize "• " 'face 'all-the-icons-yellow))
    (treemacs-create-icon :icon (format "%s\t" (all-the-icons-octicon "info" :height 0.75 :v-adjust 0.1 :face 'all-the-icons-blue))
                          :extensions (info)
                          :fallback (propertize "• " 'face 'all-the-icons-blue))
    (treemacs-create-icon :icon (format "%s\t" (all-the-icons-material "mail" :height 0.75 :v-adjust 0.1 :face 'treemacs-all-the-icons-file-face))
                          :extensions (mail)
                          :fallback " ")
    (treemacs-create-icon :icon (format "%s\t" (all-the-icons-octicon "bookmark" :height 0.75 :v-adjust 0.1 :face 'treemacs-all-the-icons-file-face))
                          :extensions (bookmark)
                          :fallback " ")
    (treemacs-create-icon :icon (format "%s\t" (all-the-icons-material "computer" :height 0.75 :v-adjust 0.1 :face 'treemacs-all-the-icons-file-face))
                          :extensions (screen)
                          :fallback " ")
    (treemacs-create-icon :icon (format "%s\t" (all-the-icons-material "home" :height 0.75 :v-adjust 0.1 :face 'treemacs-all-the-icons-file-face))
                          :extensions (house)
                          :fallback " ")
    (treemacs-create-icon :icon (format "%s\t" (all-the-icons-material "menu" :height 0.75 :v-adjust 0.1 :face 'treemacs-all-the-icons-file-face))
                          :extensions (list)
                          :fallback " ")
    (treemacs-create-icon :icon (format "%s\t" (all-the-icons-material "repeat" :height 0.75 :v-adjust 0.1 :face 'treemacs-all-the-icons-file-face))
                          :extensions (repeat)
                          :fallback " ")
    (treemacs-create-icon :icon (format "%s\t" (all-the-icons-faicon "suitcase" :height 0.75 :v-adjust 0.1 :face 'treemacs-all-the-icons-file-face))
                          :extensions (suitcase)
                          :fallback " ")
    (treemacs-create-icon :icon (format "%s\t" (all-the-icons-material "close" :height 0.75 :v-adjust 0.1 :face 'treemacs-all-the-icons-file-face))
                          :extensions (close)
                          :fallback " ")
    (treemacs-create-icon :icon (format "%s\t" (all-the-icons-octicon "calendar" :height 0.75 :v-adjust 0.1 :face 'treemacs-all-the-icons-file-face))
                          :extensions (calendar)
                          :fallback " ")
    (treemacs-create-icon :icon (format "%s\t" (all-the-icons-faicon "briefcase" :height 0.75 :v-adjust 0.1 :face 'treemacs-all-the-icons-file-face))
                          :extensions (briefcase)
                          :fallback " ")

    ;; file icons
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-fileicon "access" :v-adjust 0 :face 'all-the-icons-red)) :extensions ("accdb" "accdt" "accdt"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-fileicon "actionscript" :v-adjust 0 :face 'all-the-icons-red)) :extensions ("actionscript"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-fileicon "ansible" :v-adjust 0 :face 'all-the-icons-dsilver)) :extensions ("ansible"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-fileicon "antlr" :v-adjust 0 :face 'all-the-icons-orange)) :extensions ("antlr"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-fileicon "asciidoc" :v-adjust 0 :face 'all-the-icons-lsilver)) :extensions ("asciidoc" "adoc"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-fileicon "apple" :v-adjust 0 :face 'all-the-icons-lsilver)) :extensions ("applescript"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-fileicon "appveyor" :v-adjust 0 :face 'all-the-icons-lblue)) :extensions ("appveyor.yml"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-fileicon "arduino" :v-adjust 0 :face 'all-the-icons-dgreen)) :extensions ("ino" "pde"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-material "audiotrack" :v-adjust 0 :face 'all-the-icons-lblue)) :extensions ("mp3" "ogg" "midi" "flac" "aiff"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-fileicon "babel" :v-adjust 0 :face 'all-the-icons-yellow)) :extensions ("babelrc" "babelignore" "babelrc.js" "babelrc.json" "babel.config.js"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-fileicon "bazel" :v-adjust 0 :face 'all-the-icons-green)) :extensions ("bazelrc" "bazel"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-alltheicon "bower" :v-adjust 0 :face 'all-the-icons-orange)) :extensions ("bowerrc" "bower.json"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-fileicon "bundler" :v-adjust 0 :face 'all-the-icons-lblue)) :extensions ("gemfile" "gemfile.lock"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-octicon "gear" :v-adjust 0 :face 'all-the-icons-lsilver)) :extensions ("bat"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-alltheicon "c" :v-adjust 0 :face 'all-the-icons-purple)) :extensions ("c" "h"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-fileicon "cabal" :v-adjust 0 :face 'all-the-icons-lsilver)) :extensions ("cabal"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-alltheicon "cplusplus" :v-adjust 0 :face 'all-the-icons-purple)) :extensions ("cc" "cpp" "hh" "hpp" "tpp"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-alltheicon "clojure" :v-adjust 0 :face 'all-the-icons-blue)) :extensions ("clj" "cljc" "cljs" "edn"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-fileicon "cmake" :v-adjust 0 :face 'all-the-icons-green)) :extensions ("cmake" "cmake-cache"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-fileicon "cobol" :v-adjust 0 :face 'all-the-icons-dblue)) :extensions ("cobol"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-alltheicon "coffeescript" :v-adjust 0 :face 'all-the-icons-dorange)) :extensions ("coffeescript"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-alltheicon "css3" :v-adjust 0 :face 'all-the-icons-yellow)) :extensions ("css"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-faicon "certificate" :v-adjust 0 :face 'all-the-icons-yellow)) :extensions ("csr" "crt" "cer" "der" "pfx" "p12" "p7b" "p7r" "src" "crl" "sst" "stl"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-octicon "gear" :v-adjust 0 :face 'all-the-icons-lsilver)) :extensions ("conf" "properties" "config" "cfg" "ini" "xdefaults" "xresources" "terminalrc" "ledgerrc"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-fileicon "cython" :v-adjust 0 :face 'all-the-icons-dblue)) :extensions ("cython"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-fileicon "dart" :v-adjust 0 :face 'all-the-icons-blue-alt)) :extensions ("dart"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-alltheicon "dlang" :v-adjust 0 :face 'all-the-icons-red)) :extensions ("d" "dscript" "dml" "diet"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-octicon "diff" :v-adjust 0 :face 'all-the-icons-lsilver)) :extensions ("diff"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-fileicon "dockerfile" :v-adjust 0 :face 'all-the-icons-blue)) :extensions ("dockerfile" "docker-compose.yml"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-fileicon "editorconfig" :v-adjust 0 :face 'all-the-icons-lsilver)) :extensions ("editorconfig"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-fileicon "elisp" :v-adjust 0 :face 'all-the-icons-purple)) :extensions ("el"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-fileicon "elm" :v-adjust 0 :face 'all-the-icons-lgreen)) :extensions ("elm"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-faicon "wrench" :v-adjust 0 :face 'all-the-icons-lpurple)) :extensions ("envrc"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-alltheicon "erlang" :v-adjust 0 :face 'all-the-icons-red)) :extensions ("erl" "hrl"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-fileicon "eslint" :v-adjust 0 :face 'all-the-icons-lpurple)) :extensions ("eslintrc" "eslintignore" "eslintcache"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-alltheicon "elixir" :v-adjust 0 :face 'all-the-icons-dpurple)) :extensions ("ex" "exs" "eex" "leex"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-octicon "file-binary" :v-adjust 0 :face 'all-the-icons-lsilver)) :extensions ("exe" "dll" "obj" "so" "o"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-fileicon "cucumber" :v-adjust 0 :face 'all-the-icons-dgreen)) :extensions ("feature"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-fileicon "fortran" :v-adjust 0 :face 'all-the-icons-lsilver)) :extensions ("fortran" "fortran-modern" "fortranfreeform"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-fileicon "fsharp" :v-adjust 0 :face 'all-the-icons-lblue)) :extensions ("fsharp"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-fileicon "godot" :v-adjust 0 :face 'all-the-icons-dblue)) :extensions ("gdscript"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-material "gif" :v-adjust 0 :face 'all-the-icons-lblue)) :extensions ("gif"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-alltheicon "git" :height 0.85 :v-adjust 0 :face 'all-the-icons-red)) :extensions ("git" "gitmodules" "gitconfig" "gitignore"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-fileicon "go" :v-adjust 0 :face 'all-the-icons-lblue)) :extensions ("go"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-fileicon "gradle" :v-adjust 0 :face 'all-the-icons-dblue)) :extensions ("gradle"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-fileicon "graphql" :v-adjust 0 :face 'all-the-icons-pink)) :extensions ("graphql"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-alltheicon "haskell" :v-adjust 0 :face 'all-the-icons-silver)) :extensions ("hs" "lhs"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-alltheicon "html5" :v-adjust 0 :face 'all-the-icons-orange)) :extensions ("htm" "html"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-fileicon "hy" :v-adjust 0 :face 'all-the-icons-dblue)) :extensions ("hy"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-fileicon "jupyter" :v-adjust 0 :face 'all-the-icons-orange)) :extensions ("ipynb"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-fileicon "jenkins" :v-adjust 0 :face 'all-the-icons-lsilver)) :extensions ("jenkins"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-fileicon "jinja" :v-adjust 0 :face 'all-the-icons-dred)) :extensions ("j2" "jinja2"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-alltheicon "java" :v-adjust 0 :face 'all-the-icons-orange)) :extensions ("java" "jar"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-fileicon "julia" :v-adjust 0 :face 'all-the-icons-green)) :extensions ("jl"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-alltheicon "javascript-badge" :v-adjust 0 :face 'all-the-icons-yellow)) :extensions ("js"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-material "code" :v-adjust 0 :face 'all-the-icons-green)) :extensions ("json"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-fileicon "jsx" :v-adjust 0 :face 'all-the-icons-lblue)) :extensions ("jsx"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-material "vpn_key" :v-adjust 0 :face 'all-the-icons-yellow)) :extensions ("key" "pem"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-fileicon "kotlin" :v-adjust 0 :face 'all-the-icons-lpurple)) :extensions ("kt" "kts"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-faicon "balance-scale" :v-adjust 0 :face 'all-the-icons-purple)) :extensions ("ledger"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-alltheicon "less" :v-adjust 0 :face 'all-the-icons-dblue)) :extensions ("less"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-fileicon "lisp" :v-adjust 0 :face 'all-the-icons-red)) :extensions ("lisp"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-octicon "key" :v-adjust 0 :face 'all-the-icons-yellow)) :extensions ("license"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-material "translate" :v-adjust 0 :face 'all-the-icons-blue)) :extensions ("locale"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-material "lock" :v-adjust 0 :face 'all-the-icons-dred)) :extensions ("lock"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-fileicon "lua" :v-adjust 0 :face 'all-the-icons-dblue)) :extensions ("lua"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-fileicon "gnu" :v-adjust 0 :face 'all-the-icons-dsilver)) :extensions ("makefile"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-fileicon "meson" :v-adjust 0 :face 'all-the-icons-green)) :extensions ("meson"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-fileicon "ocaml" :v-adjust 0 :face 'all-the-icons-lorange)) :extensions ("ml" "mli" "merlin" "ocaml"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-faicon "file-video-o" :v-adjust 0 :face 'all-the-icons-lgreen)) :extensions ("mov" "mp4" "wav" "avi" "mkv" "webm"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-fileicon "nimrod" :v-adjust 0 :face 'all-the-icons-dsilver)) :extensions ("nim" "nims"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-fileicon "nginx" :v-adjust 0 :face 'all-the-icons-green)) :extensions ("nginx.conf" "nginx"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-fileicon "nix" :v-adjust 0 :face 'all-the-icons-lblue)) :extensions ("nix"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-fileicon "npm" :v-adjust 0 :face 'all-the-icons-red)) :extensions ("npmignore" "npmrc" "package.json" "package-lock.json" "npm-shrinwrap.json"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-fileicon "delphi" :v-adjust 0 :face 'all-the-icons-red)) :extensions ("pascal" "objectpascal"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-fileicon "patch" :v-adjust 0 :face 'all-the-icons-orange)) :extensions ("patch"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-alltheicon "perl" :v-adjust 0 :face 'all-the-icons-dblue)) :extensions ("perl" "pl"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-fileicon "php" :v-adjust 0 :face 'all-the-icons-dblue)) :extensions ("php"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-fileicon "phpunit" :v-adjust 0 :face 'all-the-icons-blue)) :extensions ("phpunit" "phpunit.xml"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-alltheicon "postgresql" :v-adjust 0 :face 'all-the-icons-dblue)) :extensions ("pgsql"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-fileicon "powerpoint" :v-adjust 0 :face 'all-the-icons-orange)) :extensions ("pot" "potx" "potm" "pps" "ppsx" "ppsm" "ppt" "pptx" "pptm" "pa" "ppa" "ppam" "sldm" "sldx"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-fileicon "sketch" :v-adjust 0 :face 'all-the-icons-lsilver)) :extensions ("pp"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-alltheicon "prolog" :v-adjust 0 :face 'all-the-icons-orange)) :extensions ("pro" "prolog"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-fileicon "purescript" :v-adjust 0 :face 'all-the-icons-lblue)) :extensions ("purs"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-alltheicon "python" :v-adjust 0 :face 'all-the-icons-dblue)) :extensions ("py"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-fileicon "R" :v-adjust 0 :face 'all-the-icons-dblue)) :extensions ("r"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-fileicon "racket" :v-adjust 0 :face 'all-the-icons-dred)) :extensions ("racket" "rkt" "rktd" "rktl" "scrbl" "scribble" "plt"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-alltheicon "ruby-alt" :v-adjust 0 :face 'all-the-icons-red)) :extensions ("rb"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-fileicon "reason" :v-adjust 0 :face 'all-the-icons-red)) :extensions ("re" "rei"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-alltheicon "rust" :v-adjust 0 :face 'all-the-icons-red)) :extensions ("rs"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-fileicon "sbt" :v-adjust 0 :face 'all-the-icons-dsilver)) :extensions ("sbt"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-alltheicon "sass" :v-adjust 0 :face 'all-the-icons-lpink)) :extensions ("scss" "sass"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-alltheicon "scala" :v-adjust 0 :face 'all-the-icons-red)) :extensions ("scala"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-faicon "database" :v-adjust 0 :face 'all-the-icons-yellow)) :extensions ("sql"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-fileicon "sqlite" :v-adjust 0 :face 'all-the-icons-blue)) :extensions ("sqlite" "db3" "sqlite3"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-alltheicon "svg" :v-adjust 0 :face 'all-the-icons-orange)) :extensions ("svg"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-fileicon "swagger" :v-adjust 0 :face 'all-the-icons-green)) :extensions ("swagger"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-alltheicon "swift" :v-adjust 0 :face 'all-the-icons-orange)) :extensions ("swift"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-alltheicon "terminal" :v-adjust 0 :face 'all-the-icons-dgreen)) :extensions ("sh" "bash" "zsh" "fish" "zshrc" "bashrc"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-fileicon "verilog" :v-adjust 0 :face 'all-the-icons-dblue)) :extensions ("sv" "v"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-fileicon "tex" :v-adjust 0 :face 'all-the-icons-dsilver)) :extensions ("tex"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-fileicon "typescript-alt" :v-adjust 0 :face 'all-the-icons-blue)) :extensions ("ts"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-fileicon "tsx-alt" :v-adjust 0 :face 'all-the-icons-lgreen)) :extensions ("tsx"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-fileicon "vagrant" :v-adjust 0 :face 'all-the-icons-blue)) :extensions ("vagrantfile"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-fileicon "vue" :v-adjust 0 :face 'all-the-icons-green)) :extensions ("vue"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-fileicon "excel" :v-adjust 0 :face 'all-the-icons-green)) :extensions ("xls" "xlsx" "xlsm" "ods" "fods"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-fileicon "wasm" :v-adjust 0 :face 'all-the-icons-lpurple)) :extensions ("wasm" "wat"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-faicon "font" :v-adjust 0 :face 'all-the-icons-lsilver)) :extensions ("woff" "woff2" "ttf" "otf" "eot" "pfa" "pfb" "sfd"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-material "code" :v-adjust 0 :face 'all-the-icons-lpurple)) :extensions ("xml" "xsl"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-material "code" :v-adjust 0 :face 'all-the-icons-yellow)) :extensions ("yaml" "yml"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-fileicon "yarn" :v-adjust 0 :face 'all-the-icons-dblue)) :extensions ("yarn.lock" "yarnrc" "yarnclean" "yarn-integrity" "yarn-metadata.json" "yarnignore"))

    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-octicon "file-media" :v-adjust 0 :face 'all-the-icons-orange))
                          :extensions ("png" "jpg" "jpeg" "ico" "tif" "tiff" "bmp" "psd" "ai" "eps" "indd" "webp"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-octicon "file-code" :v-adjust 0 :face 'treemacs-all-the-icons-file-face))
                          :extensions ("cask" "csv" "cxx" "ideavimrc" "inputrc" "pm"
                                       "pm6" "scm " "sql" "styles" "toml" "tridactylrc" "vh" "vimperatorrc"
                                       "vimrc" "vrapperrc"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-octicon "book" :v-adjust 0 :face 'treemacs-all-the-icons-file-face))
                          :extensions ("lrf" "lrx" "cbr" "cbz" "cb7" "cbt" "cba" "chm" "djvu"
                                       "doc" "docx" "pdb" "pdb" "fb2" "xeb" "ceb" "inf" "azw"
                                       "azw3" "kf8" "kfx" "lit" "prc" "mobi" "exe" "or"
                                       "pkg" "opf" "txt" "pdb" "ps" "rtf" "pdg" "xml" "tr2"
                                       "tr3" "oxps" "xps"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-octicon "file-text" :v-adjust 0 :face 'treemacs-all-the-icons-file-face))
                          :extensions ("md" "markdown" "rst" "log" "org" "txt"
                                       "CONTRIBUTE" "LICENSE" "README" "CHANGELOG"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-octicon "file-binary" :v-adjust 0 :face 'treemacs-all-the-icons-file-face))
                          :extensions ("class" "exe" "dll" "obj" "so" "o" "out" "elc" "pyc"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-octicon "file-pdf" :v-adjust 0 :face 'all-the-icons-orange))
                          :extensions ("pdf"))
    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-octicon "file-zip" :v-adjust 0 :face 'treemacs-all-the-icons-file-face))
                          :extensions ("zip" "7z" "tar" "gz" "rar" "tgz"))

    (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-octicon "file-text" :v-adjust 0 :face 'treemacs-all-the-icons-file-face))
                          :extensions (fallback))))

(provide 'treemacs-all-the-icons)

;;; treemacs-all-the-icons.el ends here
