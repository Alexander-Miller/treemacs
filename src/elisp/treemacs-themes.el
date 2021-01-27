;;; treemacs.el --- A tree style file viewer package -*- lexical-binding: t -*-

;; Copyright (C) 2021 Alexander Miller

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
;;; Definitions for the theme type, their creation, and, the means to change themes.

;;; Code:

(require 'dash)
(require 'ht)
(require 'treemacs-core-utils)
(require 'treemacs-logging)

(eval-when-compile
  (require 'inline)
  (require 'treemacs-macros)
  (require 'cl-lib))

(eval-when-compile
  (cl-declaim (optimize (speed 3) (safety 0))))

(treemacs-import-functions-from "treemacs-icons"
  treemacs--select-icon-set)

(cl-defstruct (treemacs-theme
               (:conc-name treemacs-theme->)
               (:constructor treemacs-theme->create!)
               (:named t))
  name path gui-icons tui-icons)

(defvar treemacs--current-theme nil "The currently used theme.")

(defvar treemacs--themes nil "List of all known themes.")

(define-inline treemacs-current-theme ()
  "Get the current theme."
  (declare (side-effect-free t))
  (inline-quote treemacs--current-theme))

(define-inline treemacs--find-theme (name)
  "Find theme with the given NAME."
  (declare (side-effect-free t))
  (inline-letevals (name)
    (inline-quote
     (--first (string= (treemacs-theme->name it) ,name) treemacs--themes))))

(cl-defmacro treemacs-create-theme (name &key icon-directory extends config)
  "Create a new (bare) theme with the given NAME.
- ICON-DIRECTORY is the (mandatory) theme's location.
- EXTENDS is the theme to be extended.
- BASED-ON is the name of a theme whose icons this one should start with.
- CONFIG is a code block to fill the created theme with icons via
  `treemacs-create-icon'."
  (declare (indent 1))
  `(let* ((gui-icons (make-hash-table :size 300 :test 'equal))
          (tui-icons (make-hash-table :size 300 :test 'equal))
          (theme (treemacs-theme->create!
                  :name ,name
                  :path ,icon-directory
                  :gui-icons gui-icons
                  :tui-icons tui-icons)))
     (add-to-list 'treemacs--themes theme)
     ,(when extends
        `(treemacs-unless-let (base-theme (treemacs--find-theme ,extends))
             (treemacs-log-failure "Could not find base theme %s when creating theme %s." ,extends ,name)
           (treemacs--maphash (treemacs-theme->gui-icons base-theme) (ext icon)
             (ht-set! gui-icons ext icon))
           (treemacs--maphash (treemacs-theme->tui-icons base-theme) (ext icon)
             (ht-set! tui-icons ext icon))))
     (-let [treemacs--current-theme theme]
       ,config
       (treemacs--propagate-new-icons theme))
     ,name))

(cl-defmacro treemacs-modify-theme (theme &key icon-directory config)
  "Modify an existing THEME.
- THEME can either be a treemacs-theme object or the name of a theme.
- For the scope of the modification an alternative ICON-DIRECTORY can also be
  used.
- CONFIG will be applied to the THEME in the same manner as in
  `treemacs-create-theme'."
  (declare (indent 1))
  (treemacs-static-assert (not (null theme))
    "Theme may not be null.")
  `(treemacs-unless-let (theme (if (stringp ,theme) (treemacs--find-theme ,theme) ,theme))
       (user-error "Theme '%s' does not exist" ,theme)
     (let* ((treemacs--current-theme theme)
            (original-icon-dir (treemacs-theme->path theme))
            (new-icon-dir (if ,icon-directory ,icon-directory original-icon-dir)))
       (unwind-protect
           (progn
             (setf (treemacs-theme->path theme) new-icon-dir)
             ,config
             (treemacs--propagate-new-icons theme))
         (setf (treemacs-theme->path theme) original-icon-dir))
       nil)))

(defun treemacs--propagate-new-icons (theme)
  "Add THEME's new icons to the other themes."
  (unless (string= (treemacs-theme->name theme) "Default")
    (dolist (other-theme (delete theme treemacs--themes))
      (pcase-dolist (`(,current-icons . ,other-icons)
                     `(,(cons (treemacs-theme->gui-icons theme)
                              (treemacs-theme->gui-icons other-theme))
                       ,(cons (treemacs-theme->tui-icons theme)
                              (treemacs-theme->tui-icons other-theme))))
        (treemacs--maphash current-icons (ext icon)
          (unless (ht-get other-icons ext)
            (ht-set! other-icons ext icon)))))))

(defun treemacs-load-theme (name)
  "Load the theme with the given NAME.
Note that some changes will only take effect after a treemacs buffer was killed
and restored."
  (interactive
   (list (completing-read "Theme: " (-map #'treemacs-theme->name treemacs--themes))))
  (treemacs-unless-let (theme (treemacs--find-theme name))
      (treemacs-log-failure "Cannot find theme '%s'." name)
    (setq treemacs--current-theme theme)
    (dolist (buffer (buffer-list))
      (when (memq (buffer-local-value 'major-mode buffer) '(treemacs-mode dired-mode))
        (with-current-buffer buffer
          (treemacs--select-icon-set))))))

(provide 'treemacs-themes)

;;; treemacs-themes.el ends here
