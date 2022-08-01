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
(require 'hydra)
(require 'treemacs-core-utils)
(require 'treemacs-visuals)
(require 'treemacs-filewatch-mode)
(require 'treemacs-logging)
(require 'treemacs-rendering)
(require 'treemacs-annotations)

(eval-when-compile
  (require 'inline)
  (require 'treemacs-macros))

(declare-function string-join "subr-x.el")

(defconst treemacs--mark-annotation-source "treemacs-marked-paths")

(defvar-local treemacs--marked-paths nil)

(with-eval-after-load 'recentf

  (declare-function recentf-remove-if-non-kept "recentf")
  (declare-function treemacs--remove-from-recentf-after-move/rename "treemacs-file-management")

  (defun treemacs--remove-from-recentf-after-move/rename (path _)
    "Remove PATH from recentf after the file was moved or renamed."
    (recentf-remove-if-non-kept path))

  (add-hook 'treemacs-rename-file-functions #'treemacs--remove-from-recentf-after-move/rename)
  (add-hook 'treemacs-move-file-functions   #'treemacs--remove-from-recentf-after-move/rename)
  (add-hook 'treemacs-delete-file-functions #'recentf-remove-if-non-kept))

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
             (path (treemacs--select-file-from-btn btn "Delete: "))
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
(defun treemacs-delete-marked-files (&optional arg)
  "Delete all marked files.

A delete action must always be confirmed.  Directories are deleted recursively.
By default files are deleted by moving them to the trash.  With a prefix ARG
they will instead be wiped irreversibly.

For marking files see `treemacs-bulk-file-actions'."
  (interactive "P")
  (treemacs-block
   (let ((delete-by-moving-to-trash (not arg))
         (to-delete (-filter #'file-exists-p treemacs--marked-paths)))

     (treemacs-error-return-if (null treemacs--marked-paths)
       "There are no marked files")

     (unless (yes-or-no-p (format "Really delete %s marked files?"
                                  (length to-delete)))
       (treemacs-return (treemacs-log "Cancelled.")))

     (treemacs--without-filewatch
      (dolist (path to-delete)
        ;; 2nd check in case of recursive deletes
        (when (file-exists-p path)
          (cond
           ((or (file-symlink-p path) (file-regular-p path))
            (delete-file path delete-by-moving-to-trash))
           ((file-directory-p path)
            (delete-directory path t delete-by-moving-to-trash))))
        (treemacs--on-file-deletion path)
        (treemacs-without-messages
         (treemacs-run-in-every-buffer
          (treemacs-delete-single-node path)))
        (run-hook-with-args 'treemacs-delete-file-functions path))
      (treemacs--evade-image)
      (setf treemacs--marked-paths (-difference treemacs--marked-paths to-delete))
      (treemacs-log "Deleted %s files." (length to-delete))))))

;;;###autoload
(defun treemacs-move-file ()
  "Move file (or directory) at point.

If the selected target is an existing directory the source file will be directly
moved into this directory.  If the given target instead does not exist then it
will be treated as the moved file's new name, meaning the original source file
will be both moved and renamed."
  (interactive)
  (treemacs--copy-or-move
   :action 'move
   :no-node-msg "There is nothing to move here."
   :wrong-type-msg "Only files and directories can be moved."
   :action-fn #'rename-file
   :prompt "Move to: "
   :flat-prompt "File to copy: "
   :finish-verb "Moved"))

;;;###autoload
(defun treemacs-copy-file ()
  "Copy file (or directory) at point.

If the selected target is an existing directory the source file will be directly
copied into this directory.  If the given target instead does not exist then it
will be treated as the copied file's new name, meaning the original source file
will be both copied and renamed."
  (interactive)
  (treemacs--copy-or-move
   :action 'copy
   :no-node-msg "There is nothing to move here."
   :wrong-type-msg "Only files and directories can be copied."
   :action-fn (lambda (from to)
                (if (file-directory-p from)
                    (copy-directory from to)
                  (copy-file from to)))
   :prompt "Copy to: "
   :flat-prompt "File to copy: "
   :finish-verb "Copied"))

(cl-defun treemacs--copy-or-move
    (&key
     action
     no-node-msg
     wrong-type-msg
     action-fn
     prompt
     flat-prompt
     finish-verb)
  "Internal implementation for copying and moving files.

ACTION: either `copy' or `move'
NO-NODE-MSG: error message in case there is no node in the current line
WRONG-TYPE-MSG: error message in case current node is not a file
ACTION-FN: function to actually copy or move a file
PROMPT: prompt to read the target directory
FLAT-PROMPT: prompt to select source file when node is flattened
FINISH-VERB: finisher for the success message."
  (treemacs-block
   (let ((btn (treemacs-current-button)))
     (treemacs-error-return-if (null btn)
       no-node-msg)
     (treemacs-error-return-if
         (not (memq (treemacs-button-get btn :state)
                    '(file-node-open file-node-closed dir-node-open dir-node-closed)))
       wrong-type-msg)
     (let* ((source (treemacs--select-file-from-btn btn flat-prompt))
            (destination (treemacs--canonical-path
                          (read-directory-name prompt nil default-directory)))
            (destination-dir (if (file-directory-p destination)
                                 destination
                               (treemacs--parent-dir destination)))
            (target-name (treemacs--filename
                          (if (file-directory-p destination)
                              source
                            destination)))
            (target (->> target-name
                         (treemacs-join-path destination-dir)
                         (treemacs--find-repeated-file-name))))
       (unless (file-exists-p destination-dir)
         (make-directory destination-dir :parents))
       (when (eq action 'move)
         ;; do the deletion *before* moving the file, otherwise it will
         ;; no longer exist and treemacs will not recognize it as a file path
         (treemacs-do-delete-single-node source))
       (treemacs--without-filewatch
        (funcall action-fn source target))
       (pcase action
         ('move
          (run-hook-with-args 'treemacs-copy-file-functions source target)
          (treemacs--on-file-deletion source))
         ('copy
          (run-hook-with-args 'treemacs-move-file-functions source target)
          (treemacs-remove-annotation-face source "treemacs-marked-paths")))

       (treemacs-update-node destination-dir)

       (when (treemacs-is-path target :in-workspace)
         (treemacs-goto-file-node target))

       (treemacs-pulse-on-success "%s %s to %s"
         finish-verb
         (propertize (treemacs--filename target) 'face 'font-lock-string-face)
         (propertize destination-dir 'face 'font-lock-string-face))))))

;;;###autoload
(defun treemacs-move-marked-files ()
  "Move all marked files.

For marking files see `treemacs-bulk-file-actions'."
  (interactive)
  (treemacs--bulk-copy-or-move
   :action 'move
   :action-fn #'rename-file
   :prompt "Move to: "
   :finish-verb "Moved"))

;;;###autoload
(defun treemacs-copy-marked-files ()
  "Copy all marked files.

For marking files see `treemacs-bulk-file-actions'."
  (interactive)
  (treemacs--bulk-copy-or-move
   :action 'copy
   :action-fn (lambda (from to)
                (if (file-directory-p from)
                    (copy-directory from to)
                  (copy-file from to)))
   :prompt "Copy to: "
   :finish-verb "Copied"))

(cl-defun treemacs--bulk-copy-or-move
    (&key
     action
     action-fn
     prompt
     finish-verb)
  "Internal implementation for bulk-copying and -moving files.
ACTION: either `copy' or `move'
ACTION-FN: function to actually copy or move a file
PROMPT: prompt to read the target directory
FINISH-VERB: finisher for the success message."
  (treemacs-block
   (let* ((to-move (-filter #'file-exists-p treemacs--marked-paths))
          (destination-dir (treemacs--canonical-path
                            (read-directory-name prompt nil default-directory)))
          (projects (->> to-move
                         (-map #'treemacs--find-project-for-path)
                         (cl-remove-duplicates)
                         (-filter #'identity))))
     (treemacs-save-position
      (dolist (source to-move)
        (let ((target (->> source
                           (treemacs--filename)
                           (treemacs-join-path destination-dir)
                           (treemacs--find-repeated-file-name))))
          (unless (string= source target)
            (unless (file-exists-p destination-dir)
              (make-directory destination-dir :parents))
            (when (eq action 'move)
              ;; do the deletion *before* moving the file, otherwise it will
              ;; no longer exist and treemacs will not recognize it as a file path
              (treemacs-do-delete-single-node source))
            (treemacs--without-filewatch
             (funcall action-fn source target))
            (pcase action
              ('move
               (run-hook-with-args 'treemacs-copy-file-functions source target)
               (treemacs--on-file-deletion source))
              ('copy
               (run-hook-with-args 'treemacs-move-file-functions source target)
               (treemacs-remove-annotation-face source "treemacs-marked-paths"))))))

      (dolist (project projects)
        (treemacs-project->refresh! project)))

     (when (treemacs-is-path destination-dir :in-workspace)
       (treemacs-goto-file-node destination-dir))

     (setf treemacs--marked-paths (-difference treemacs--marked-paths to-move))

     (treemacs-pulse-on-success "%s %s files to %s"
       finish-verb
       (propertize (number-to-string (length to-move)) 'face 'font-lock-constant-face)
       (propertize destination-dir 'face 'font-lock-string-face)))))

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
     (-let [old-path (treemacs--select-file-from-btn btn "Rename: ")]
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
              (parent    (treemacs-button-get btn :parent)))
         (treemacs-error-return-if
             (and (file-exists-p new-path)
                  (or (not (eq 'darwin system-type))
                      (not (string= old-name new-name))))
           "A file named %s already exists."
           (propertize new-name 'face font-lock-string-face))
         (treemacs--without-filewatch
          (rename-file old-path new-path)
          (treemacs--replace-recentf-entry old-path new-path)
          (-let [treemacs-silent-refresh t]
            (treemacs-run-in-every-buffer
             (treemacs--on-rename old-path new-path treemacs-filewatch-mode)
             ;; save-excursion does not work for whatever reason
             (-let [p (point)]
               (treemacs-do-update-node (treemacs-button-get parent :path))
               (goto-char p)))))
         (treemacs--reload-buffers-after-rename old-path new-path)
         (run-hook-with-args
          'treemacs-rename-file-functions
          old-path new-path)
         (treemacs-pulse-on-success "Renamed %s to %s."
           (propertize (treemacs--filename old-path) 'face font-lock-string-face)
           (propertize new-name 'face font-lock-string-face)))))))

(defalias 'treemacs-rename #'treemacs-rename-file)
(make-obsolete #'treemacs-rename #'treemacs-rename-file "v2.9.3")

;;; Bulk Actions

;;;###autoload
(defun treemacs-show-marked-files ()
  "Print a list of all files marked by treemacs."
  (interactive)
  (let* ((len (length treemacs--marked-paths))
         (message
          (pcase len
           (0 "There are currently no marked files.")
           (1 (format "There is currently 1 marked file:\n%s"
                      (car treemacs--marked-paths)))
           (_ (format "There are currently %s marked files:\n%s"
                      len
                      (string-join treemacs--marked-paths "\n"))))))
    (treemacs-log message)))

;;;###autoload
(defun treemacs-mark-or-unmark-path-at-point ()
  "Mark or unmark the absolute path of the node at point."
  (interactive)
  (treemacs-block
   (-let [path (treemacs--prop-at-point :path)]
     (treemacs-error-return-if (null path)
       "There is nothing to mark here")
     (treemacs-error-return-if
         (or (not (stringp path)) (not (file-exists-p path)))
       "Path at point is not a file or directory.")
     (if (member path treemacs--marked-paths)
         (progn
           (setq treemacs--marked-paths
                 (remove path treemacs--marked-paths))
           (treemacs-log "Unmarked path: %s" (propertize path 'face 'font-lock-string-face))
           (treemacs-remove-annotation-face path "treemacs-marked-paths"))
       (progn
         (setq treemacs--marked-paths
               (append treemacs--marked-paths (list path)))
         (treemacs-log "Marked path: %s" (propertize path 'face 'font-lock-string-face))
         (treemacs-set-annotation-face path 'treemacs-marked-file-face "treemacs-marked-paths")))
     (treemacs-apply-annotations (treemacs--parent-dir path)))))

;;;###autoload
(defun treemacs-reset-marks ()
  "Unmark all previously marked files in the current buffer."
  (interactive)
  (let ((count (length treemacs--marked-paths))
        (projects))
    (dolist (path treemacs--marked-paths)
      (treemacs-remove-annotation-face path treemacs--mark-annotation-source)
      (push (treemacs--find-project-for-path path) projects))
    (setf treemacs--marked-paths nil)
    (dolist (project (-uniq projects))
      (treemacs-apply-annotations (treemacs-project->path project)))
    (treemacs-pulse-on-success "Unmarked %s file(s)." count)))

;;;###autoload
(defun treemacs-delete-marked-paths ()
  "Delete all previously marked files."
  (interactive)
  (treemacs-save-position
   (when (yes-or-no-p
          (format "Really delete %s marked file(s)?"
                  (length treemacs--marked-paths)))
     (-let [count (length treemacs--marked-paths)]
       (dolist (path treemacs--marked-paths)
         (if (file-directory-p path)
             (delete-directory path t)
           (delete-file path))
         (treemacs-do-delete-single-node path)
         (treemacs-remove-annotation-face path treemacs--mark-annotation-source))
       (setf treemacs--marked-paths nil)
       (hl-line-highlight)
       (treemacs-log "Deleted %s files." count)))))

;; shut down docstring width warnings
(with-no-warnings
  (defhydra treemacs-bulk-file-actions-hydra (:exit t :hint nil)
    ("m" #'treemacs-mark-or-unmark-path-at-point "(un)mark")
    ("u" #'treemacs-reset-marks "unmark all")
    ("s" #'treemacs-show-marked-files "show")
    ("d" #'treemacs-delete-marked-files "delete")
    ("c" #'treemacs-copy-marked-files "copy")
    ("o" #'treemacs-move-marked-files "move")
    ("q" nil "cancel")))

;;;###autoload
(defun treemacs-bulk-file-actions ()
  "Activate the bulk file actions hydra.
This interface allows to quickly (unmark) files, so as to copy, move or delete
them in bulk.

Note that marking files is *permanent*, files will stay marked until they are
either manually unmarked or deleted.  You can show a list of all currently
marked files with `treemacs-show-marked-files' or `s' in the hydra."
  (interactive)
  (treemacs-bulk-file-actions-hydra/body))

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
  (let* ((curr-path (treemacs--select-file-from-btn
                     (treemacs-current-button)
                     "Create in: " :dir-only))
         (path-to-create (treemacs-canonical-path
                          (read-file-name
                           (if is-file? "Create File: " "Create Directory: ")
                           (treemacs--add-trailing-slash
                            (if (file-directory-p curr-path)
                                curr-path
                              (treemacs--parent-dir curr-path)))))))
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
                    (created-under-btn (treemacs-find-visible-node created-under)))
         ;; update only the part that changed to keep things smooth
         ;; for files that's just their parent, for directories we have to take
         ;; flattening into account
         (if (treemacs-button-get created-under-btn :collapsed)
             (treemacs-update-node (treemacs-button-get (treemacs-button-get created-under-btn :parent) :path))
           (treemacs-update-node (treemacs-button-get created-under-btn :path))))
       (treemacs-goto-file-node path-to-create project)
       (recenter))
     (treemacs-pulse-on-success
         "Created %s." (propertize path-to-create 'face 'font-lock-string-face)))))

(defun treemacs--select-file-from-btn (btn prompt &optional dir-only)
  "Select the file at BTN for file management.
Offer a specifying dialogue with PROMPT when the button is flattened.
Pick only directories when DIR-ONLY is non-nil."
  (declare (side-effect-free t))
  (let* ((path          (and btn (treemacs-button-get btn :path)))
         (collapse-info (and btn (treemacs-button-get btn :collapsed)))
         (is-str        (and path (stringp path)))
         (is-dir        (and is-str (file-directory-p path)))
         (is-file       (and is-str (file-regular-p path))))
    (cond
     (collapse-info
      (completing-read prompt collapse-info nil :require-match))
     (is-dir
      path)
     ((and is-file dir-only)
      (treemacs--parent-dir path))
     (is-file
      path)
     (t
      (expand-file-name "~")))))

(provide 'treemacs-file-management)

;;; treemacs-file-management.el ends here
