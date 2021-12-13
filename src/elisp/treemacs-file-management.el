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

;; Everything related to file management.

;; NOTE: This module is lazy-loaded.


;;; Code:

(require 'dash)
(require 'treemacs-core-utils)
(require 'treemacs-visuals)
(require 'treemacs-filewatch-mode)
(require 'treemacs-logging)
(require 'treemacs-rendering)

(eval-when-compile
  (require 'treemacs-macros))

(defconst treemacs--file-node-states
  '(file-node-open file-node-closed dir-node-open dir-node-closed)
  "List of node states treemacs is able to rename/delete etc.")

(define-inline treemacs--is-node-file-manageable? (btn)
  "Determines whether BTN is a file node treemacs can rename/delete."
  (declare (side-effect-free t))
  (inline-letevals (btn)
    (inline-quote
     (memq (treemacs-button-get ,btn :state)
           treemacs--file-node-states))))

;;;###autoload
(defun treemacs-delete-file (&optional arg)
  "Delete node at point.
A delete action must always be confirmed.  Directories are deleted recursively.
By default files are deleted by moving them to the trash.  With a prefix ARG
they will instead be wiped irreversibly."
  (interactive "P")
  (treemacs-block
   (treemacs-unless-let (btn (treemacs-current-button))
       (treemacs-pulse-on-failure "Nothing to delete here.")
     (treemacs-error-return-if (not (memq (treemacs-button-get btn :state)
                                          '(file-node-open file-node-closed dir-node-open dir-node-closed)))
       "Only files and directories can be deleted.")
     (treemacs--without-filewatch
      (let* ((delete-by-moving-to-trash (not arg))
             (path (treemacs-button-get btn :path))
             (file-name (propertize (treemacs--filename path) 'face 'font-lock-string-face)))
        (cond
         ((file-symlink-p path)
          (if (yes-or-no-p (format "Remove link '%s -> %s' ? "
                                   file-name
                                   (propertize (file-symlink-p path) 'face 'font-lock-face)))
              (delete-file path delete-by-moving-to-trash)
            (treemacs-return (treemacs-log "Cancelled."))))
         ((file-regular-p path)
          (if (yes-or-no-p (format "Delete '%s' ? " file-name))
              (delete-file path delete-by-moving-to-trash)
            (treemacs-return (treemacs-log "Cancelled."))))
         ((file-directory-p path)
          (if (yes-or-no-p (format "Recursively delete '%s' ? " file-name))
              (delete-directory path t delete-by-moving-to-trash)
            (treemacs-return (treemacs-log "Cancelled."))))
         (t
          (treemacs-error-return
              (treemacs-pulse-on-failure
                  "Item is neither a file, a link or a directory - treemacs does not know how to delete it. (Maybe it no longer exists?)"))))
        (treemacs--on-file-deletion path)
        (treemacs-without-messages
         (treemacs-run-in-every-buffer
          (treemacs-delete-single-node path)))
        (run-hook-with-args 'treemacs-delete-file-functions path)
        (treemacs-log "Deleted %s."
          (propertize path 'face 'font-lock-string-face))))
     (treemacs--evade-image))))
(defalias 'treemacs-delete #'treemacs-delete-file)
(make-obsolete #'treemacs-delete #'treemacs-delete-file "v2.9.3")

;;;###autoload
(defun treemacs-move-file ()
  "Move file (or directory) at point.
Destination may also be a filename, in which case the moved file will also
be renamed."
  (interactive)
  (treemacs--copy-or-move :move))

;;;###autoload
(defun treemacs-copy-file ()
  "Copy file (or directory) at point.
Destination may also be a filename, in which case the copied file will also
be renamed."
  (interactive)
  (treemacs--copy-or-move :copy))

(defun treemacs--copy-or-move (action)
  "Internal implementation for copying and moving files.
ACTION will be either `:copy' or `:move', depending on whether we are calling
from `treemacs-copy-file' or `treemacs-move-file'."
  (let ((no-node-msg)
        (wrong-type-msg)
        (prompt)
        (action-function)
        (finish-msg))
    (pcase action
      (:copy
       (setf no-node-msg     "There is nothing to copy here."
             wrong-type-msg  "Only files and directories can be copied."
             prompt          "Copy to: "
             action-function (lambda (from to)
                               (if (file-directory-p from)
                                   (copy-directory from to)
                                 (copy-file from to)))
             finish-msg      "Copied %s to %s"))
      (:move
       (setf no-node-msg     "There is nothing to move here."
             wrong-type-msg  "Only files and directories can be moved."
             prompt          "Move to: "
             action-function #'rename-file
             finish-msg      "Moved %s to %s")))
    (treemacs-block
     (treemacs-unless-let (node (treemacs-node-at-point))
         (treemacs-error-return no-node-msg)
       (treemacs-error-return-if (not (treemacs-is-node-file-or-dir? node))
         wrong-type-msg)
       (let* ((source (treemacs-button-get node :path))
              (source-name (treemacs--filename source))
              (destination (treemacs--unslash (read-file-name prompt nil default-directory)))
              (target-is-dir? (file-directory-p destination))
              (target-name (if target-is-dir? (treemacs--filename source) (treemacs--filename destination)))
              (destination-dir (if target-is-dir? destination (treemacs--parent-dir destination)))
              (target (treemacs--find-repeated-file-name (treemacs-join-path destination-dir target-name))))
         (unless (file-exists-p destination-dir)
           (make-directory destination-dir :parents))
         (when (eq action :move)
           ;; do the deletion *before* moving the file, otherwise it will no longer exist and treemacs will
           ;; not recognize it as a file path
           (treemacs-do-delete-single-node source))
         (treemacs--without-filewatch
          (funcall action-function source target))
         ;; no waiting for filewatch, if we copied to an expanded directory refresh it immediately
         (-let [parent (treemacs--parent target)]
           (when (treemacs-is-path-visible? parent)
             (treemacs-do-update-node parent)))
         (treemacs-goto-file-node target)
         (run-hook-with-args
          (pcase action
            (:copy 'treemacs-copy-file-functions)
            (:move 'treemacs-move-file-functions))
          source target)
         (treemacs-pulse-on-success finish-msg
           (propertize source-name 'face 'font-lock-string-face)
           (propertize destination 'face 'font-lock-string-face)))))))

;;;###autoload
(cl-defun treemacs-rename-file ()
  "Rename the file/directory at point.

Buffers visiting the renamed file or visiting a file inside the renamed
directory and windows showing them will be reloaded.  The list of recent files
will likewise be updated."
  (interactive)
  (treemacs-block
   (treemacs-unless-let (btn (treemacs-current-button))
       (treemacs-pulse-on-failure "Nothing to rename here.")
     (-let [old-path (treemacs-button-get btn :path)]
       (treemacs-error-return-if (null old-path)
         "Found nothing to rename here.")
       (treemacs-error-return-if (not (treemacs--is-node-file-manageable? btn))
         "Only files and directories can be deleted.")
       (treemacs-error-return-if (not (file-exists-p old-path))
         "The file to be renamed does not exist.")
       (let* ((old-name  (treemacs--filename old-path))
              (new-name  (treemacs--read-string
                          "New name: " (file-name-nondirectory old-path)))
              (dir       (treemacs--parent-dir old-path))
              (new-path  (treemacs-join-path dir new-name))
              (expanded? (treemacs-is-node-expanded? btn)))
         (treemacs-error-return-if
             (and (file-exists-p new-path)
                  (or (not (eq 'darwin system-type))
                      (not (string= old-name new-name))))
           "A file named %s already exists."
           (propertize new-name 'face font-lock-string-face))
         (rename-file old-path new-path)
         (treemacs--replace-recentf-entry old-path new-path)
         (-let [treemacs-silent-refresh t]
           (treemacs-run-in-every-buffer
            (treemacs--on-rename old-path new-path treemacs-filewatch-mode)
            (when (treemacs-is-path-visible? old-path)
              (treemacs-log "Update Node %s" dir)
              (treemacs-update-node dir)
              (treemacs-goto-file-node new-path)
              (when expanded?
                (with-no-warnings
                  (treemacs-toggle-node))))))
         (treemacs--reload-buffers-after-rename old-path new-path)
         (run-hook-with-args
          'treemacs-rename-file-functions
          old-path new-path)
         (treemacs-pulse-on-success "Renamed %s to %s."
           (propertize (treemacs--filename old-path) 'face font-lock-string-face)
           (propertize new-name 'face font-lock-string-face)))))))

(defalias 'treemacs-rename #'treemacs-rename-file)
(make-obsolete #'treemacs-rename #'treemacs-rename-file "v2.9.3")

;;;###autoload
(defun treemacs-create-file ()
  "Create a new file.
Enter first the directory to create the new file in, then the new file's name.
The pre-selection for what directory to create in is based on the \"nearest\"
path to point - the containing directory for tags and files or the directory
itself, using $HOME when there is no path at or near point to grab."
  (interactive)
  (treemacs--create-file/dir t))

;;;###autoload
(defun treemacs-create-dir ()
  "Create a new directory.
Enter first the directory to create the new dir in, then the new dir's name.
The pre-selection for what directory to create in is based on the \"nearest\"
path to point - the containing directory for tags and files or the directory
itself, using $HOME when there is no path at or near point to grab."
  (interactive)
  (treemacs--create-file/dir nil))

(defun treemacs--create-file/dir (is-file?)
  "Interactively create either a file or directory, depending on IS-FILE.

IS-FILE?: Bool"
  (interactive)
  (let* ((curr-path (--if-let (treemacs-current-button)
                        (treemacs--nearest-path it)
                      (expand-file-name "~")))
         (path-to-create (read-file-name
                          (if is-file? "Create File: " "Create Directory: ")
                          (treemacs--add-trailing-slash
                           (if (file-directory-p curr-path)
                               curr-path
                             (treemacs--parent-dir curr-path))))))
    (treemacs-block
     (treemacs-error-return-if (file-exists-p path-to-create)
       "%s already exists." (propertize path-to-create 'face 'font-lock-string-face))
     (treemacs--without-filewatch
      (if is-file?
          (-let [dir (treemacs--parent-dir path-to-create)]
            (unless (file-exists-p dir)
              (make-directory dir t))
            (write-region "" nil path-to-create nil 0))
        (make-directory path-to-create t))
      (run-hook-with-args 'treemacs-create-file-functions path-to-create))
     (-when-let (project (treemacs--find-project-for-path path-to-create))
       (-when-let* ((created-under (treemacs--parent path-to-create))
                    (created-under-pos (treemacs-find-visible-node created-under)))
         ;; update only the part that changed to keep things smooth
         ;; for files that's just their parent, for directories we have to take
         ;; flattening into account
         (if (and (treemacs-button-get created-under-pos :parent)
                  (or (treemacs-button-get created-under-pos :collapsed)
                      ;; count includes "." "..", so it'll be flattened
                      (= 3 (length (directory-files created-under)))))
             (treemacs-do-update-node (-> created-under-pos
                                          (treemacs-button-get :parent)
                                          (treemacs-button-get :path)))
           (treemacs-do-update-node created-under)))
       (treemacs-goto-file-node (treemacs-canonical-path path-to-create) project)
       (recenter))
     (treemacs-pulse-on-success
         "Created %s." (propertize path-to-create 'face 'font-lock-string-face)))))

(provide 'treemacs-file-management)

;;; treemacs-file-management.el ends here
