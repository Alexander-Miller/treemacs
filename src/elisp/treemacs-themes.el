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
;;; Definitions for the theme type, their creation, and, eventually, the means
;;; to change themes.

;;; Code:

(require 'f)
(require 'dash)
(require 'ht)
(require 'cl-lib)
(require 'inline)
(require 'treemacs-macros)
(require 'treemacs-core-utils)

(treemacs--defstruct treemacs-theme
  name path gui-icons tui-icons)

(defvar treemacs--current-theme nil "The currently used theme.")

(defvar treemacs--themes nil "List of all known themes.")

(define-inline treemacs--find-theme (name)
  "Find theme with the given NAME."
  (declare (side-effect-free t))
  (inline-letevals (name)
    (inline-quote
     (--first (string= (treemacs-theme->name it) ,name) treemacs--themes))))

(cl-defmacro treemacs-create-theme (name &key icon-directory extends config)
  "Create a new (bare) theme with the given NAME.
- ICON-DIRECTORY is the (mandatory) theme's location.
- BASED-ON is the name of a theme whose icons this one should start with.
- CONFIG is a code block to fill the created theme with icons via
  `treemacs-create-icon'."
  (declare (indent 1))
  `(let* ((gui-icons (make-hash-table :size 300 :test 'equal))
          (tui-icons (make-hash-table :size 300 :test 'equal))
          (theme (make-treemacs-theme
                  :name ,name
                  :path ,icon-directory
                  :gui-icons gui-icons
                  :tui-icons tui-icons)))
     (add-to-list 'treemacs--themes theme)
     (when ,extends
       (treemacs-unless-let (base-theme (treemacs--find-theme ,extends))
           (treemacs-log "Could not find base theme %s when creating theme %s." ,extends ,name)
         (treemacs--maphash (treemacs-theme->gui-icons base-theme) (ext icon)
           (ht-set! gui-icons ext icon))
         (treemacs--maphash (treemacs-theme->tui-icons base-theme) (ext icon)
           (ht-set! tui-icons ext icon))))
     (-let [treemacs--current-theme theme]
       ,config)
     theme))

(defun treemacs-load-theme (name)
  "Enable the theme with the given NAME."
  (treemacs-unless-let (theme (treemacs--find-theme name))
      (treemacs-log "Cannot find theme '%s'." name)
    (setq treemacs--current-theme theme)))

(provide 'treemacs-themes)

;;; treemacs-themes.el ends here
