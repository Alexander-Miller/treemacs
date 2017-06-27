;;; treemacs.el --- A tree style file viewer package -*- lexical-binding: t -*-

;; Copyright (C) 2017 Alexander Miller

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
;;; Code in this file is considered performance critical.
;;; The usual restrictions w.r.t quality, readability and maintainability are
;;; lifted here.

;;; Code:

(require 'cl-lib)
(require 'treemacs-impl)

(defsubst treemacs--button-put (button prop val)
  "Set BUTTON's PROP property to VAL and return BUTTON."
  (put-text-property
   (or (previous-single-property-change (1+ button) 'button)
       (point-min))
   (or (next-single-property-change button 'button)
       (point-max))
   prop val)
  button)

(defsubst treemacs--button-at (pos)
  "Return the button at position POS in the current buffer, or nil.
If the button at POS is a text property button, the return value
is a marker pointing to POS."
  (copy-marker pos t))

(defsubst treemacs--sort-alphabetic-asc (f1 f2)
  "Sort F1 and F2 alphabetically asc."
  (string-lessp f2 f1))

(defsubst treemacs--sort-alphabetic-desc (f1 f2)
  "Sort F1 and F2 alphabetically desc."
  (string-lessp f1 f2))

(defsubst treemacs--sort-size-asc (f1 f2)
  "Sort F1 and F2 by size asc."
  (>= (file-attribute-size (file-attributes f1))
      (file-attribute-size (file-attributes f2))))

(defsubst treemacs--sort-size-desc (f1 f2)
  "Sort F1 and F2 by size desc."
  (< (file-attribute-size (file-attributes f1))
     (file-attribute-size (file-attributes f2))))

(defsubst treemacs--sort-mod-time-asc (f1 f2)
  "Sort F1 and F2 by modification time asc."
  (file-newer-than-file-p f1 f2))

(defsubst treemacs--sort-mod-time-desc (f1 f2)
  "Sort F1 and F2 by modification time desc."
  (file-newer-than-file-p f2 f1))

(defsubst treemacs--insert-button (label &rest properties)
  "Insert a button with LABEL and given PROPERTIES."
  (let ((beg (point)))
    (insert label)
    (add-text-properties beg (point) (append '(button (t) category default-button) properties))
    beg))

(defsubst treemacs--get-dir-content (dir)
  "Get the content of DIR, separated into sublists of first dirs, then files."
  (let* ((sort-func
          (pcase treemacs-sorting
            ('alphabetic-asc  #'treemacs--sort-alphabetic-asc)
            ('alphabetic-desc #'treemacs--sort-alphabetic-desc)
            ('size-asc        #'treemacs--sort-size-asc)
            ('size-desc       #'treemacs--sort-size-desc)
            ('mod-time-asc    #'treemacs--sort-mod-time-asc)
            ('mod-time-desc   #'treemacs--sort-mod-time-desc)
            (_               (user-error "Unknown treemacs-sorting value '%s'" treemacs-sorting))))
         (entries (-> dir (directory-files t nil t) (treemacs--filter-files-to-be-shown)))
         (dirs-files (-separate #'file-directory-p entries)))
    (list (sort (cl-first dirs-files) sort-func)
          (sort (cl-second dirs-files) sort-func))))

(defsubst treemacs--insert-file-image-txt (_ prefix __)
  "Insert PREFIX for a file, minus the space that would be used for the icon."
  (end-of-line)
  (insert (substring prefix 0 -2)))

(defsubst treemacs--insert-dir-image-txt (prefix _)
  "Insert text icon for a directory given PREFIX."
  (end-of-line)
  (insert (substring prefix 0 -2) (with-no-warnings treemacs-icon-closed-text)))

(defsubst treemacs--insert-file-image-png (path prefix insert-depth)
  "Insert the appropriate file png icon for PATH given PREFIX and INSERT-DEPTH."
  (end-of-line)
  (let ((start (+ 1 (point) insert-depth)))
    (insert prefix)
    (add-text-properties start (1+ start) `(display ,(gethash (-some-> path (treemacs--file-extension) (downcase))
                                                              treemacs-icons-hash
                                                              (with-no-warnings treemacs-icon-text))))))

(defsubst treemacs--insert-dir-image-png (prefix insert-depth)
  "Insert the appropriate dir png icon for PREFIX and INSERT-DEPTH."
  (end-of-line)
  (let ((start (+ 1 (point) insert-depth)))
    (insert prefix)
    (add-text-properties start (1+ start) `(display ,(with-no-warnings treemacs-icon-closed)))))

(defsubst treemacs--insert-dir-node (path prefix parent depth insert-depth)
  "Insert a directory node for PATH.
PREFIX is a string inserted as indentation.
PARENT is the (optional) button under which this one is inserted.
DEPTH indicates how deep in the filetree the current button is.
INSERT-DEPTH indicates where the icon is to be inserted."
  (funcall treemacs--insert-dir-image-function prefix insert-depth)
  (treemacs--insert-button (f-filename path)
                           'state     'dir-closed
                           'action    #'treemacs--push-button
                           'abs-path  path
                           'parent    parent
                           'depth     depth
                           'face      'treemacs-directory-face))

(defsubst treemacs--insert-file-node (path prefix parent depth insert-depth git-info)
  "Insert a directory node for PATH.
PREFIX is a string inserted as indentation.
PARENT is the (optional) button under which this one is inserted.
DEPTH indicates how deep in the filetree the current button is.
INSERT-DEPTH indicates where the icon is to be inserted.
GIT-INFO (if any) is used to determine the node's face."
  (funcall treemacs--insert-file-image-function path prefix insert-depth)
  (treemacs--insert-button (f-filename path)
                           'state     'file
                           'action    #'treemacs--push-button
                           'abs-path  path
                           'parent    parent
                           'depth     depth
                           'face      (treemacs--get-face path git-info)))

(defun treemacs--create-branch (root indent-depth git-process &optional parent)
  "Create a new treemacs branch under ROOT.
The branch is indented at INDENT-DEPTH and uses the eventual output of
GIT-PROCESS to decide on file nodes' faces. The nodes' parent property is set
to PARENT."
  (save-excursion
           ;; pass down to save recalculating every time an icon is inserted
    (let* ((ins-depth  (* indent-depth treemacs-indentation))
           ;; extra 2 spaces for the icon
           (prefix     (concat "\n" (make-string (+ 2 ins-depth) ?\ )))
           (entries    (treemacs--get-dir-content root))
           (dirs       (cl-first entries))
           (files      (cl-second entries))
           (git-info   (treemacs--parse-git-status git-process))
           (prev-file  nil)
           (prev-dir   nil)
           (first-file nil))
      ;; Do all the work in a single pass through all the dirs and files - insert the icon, the filename
      ;; and set the next/prev node properties
      (when (> (length dirs) 0)
        (setq prev-dir (treemacs--button-at (treemacs--insert-dir-node (cl-first dirs) prefix parent indent-depth ins-depth))))
      (--each (cdr dirs)
        (let ((b (treemacs--button-at (treemacs--insert-dir-node it prefix parent indent-depth ins-depth))))
          (treemacs--button-put prev-dir 'next-node b)
          (setq prev-dir (treemacs--button-put b 'prev-node prev-dir))))
      (when (> (length files) 0)
        (setq prev-file (treemacs--button-at (treemacs--insert-file-node (cl-first files) prefix parent indent-depth ins-depth git-info)))
        (setq first-file prev-file))
      (--each (cdr files)
        (let ((b (treemacs--button-at (treemacs--insert-file-node it prefix parent indent-depth ins-depth git-info))))
          (treemacs--button-put prev-file 'next-node b)
          (setq prev-file (treemacs--button-put b 'prev-node prev-file))))
      (when (and first-file prev-dir)
          (button-put prev-dir 'next-node first-file)
          (button-put first-file 'prev-node prev-dir))
      ;; reopen here only since create-branch is called both when opening a node and
      ;; building the entire tree
      (treemacs--reopen-at root))))

(defun treemacs--check-window-system ()
  "Check if the window system has changed since the last call.
Make the necessary render function changes changes if so and explicitly
return t."
  (let ((current-ui (window-system)))
    (unless (eq current-ui treemacs--in-gui)
      (setq treemacs--in-gui current-ui)
      ;; icon variables are known to exist
      (with-no-warnings
        (if current-ui
            (progn
              (setf treemacs--insert-dir-image-function #'treemacs--insert-dir-image-png)
              (setf treemacs--insert-file-image-function #'treemacs--insert-file-image-png)
              (setq treemacs-icon-open treemacs-icon-open-png
                    treemacs-icon-closed treemacs-icon-closed-png))
          (progn
            (setf treemacs--insert-dir-image-function #'treemacs--insert-dir-image-txt)
            (setf treemacs--insert-file-image-function #'treemacs--insert-file-image-txt)
            (setq treemacs-icon-open treemacs-icon-open-text
                  treemacs-icon-closed treemacs-icon-closed-text))))
      t)))

(provide 'treemacs-branch-creation)

;;; treemacs-branch-creation.el ends here
