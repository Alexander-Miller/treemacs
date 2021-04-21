;;; treemacs-bookmarks.el --- A tree style file viewer package -*- lexical-binding: t -*-

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

;; Integrates treemacs with bookmark.el.

;; NOTE: This module is lazy-loaded.

;;; Code:

(require 'bookmark)
(require 'dash)
(require 'treemacs-follow-mode)
(require 'treemacs-interface)
(require 'treemacs-scope)
(require 'treemacs-logging)
(require 'treemacs-tags)
(require 'treemacs-workspaces)

(eval-when-compile
  (require 'cl-lib)
  (require 'treemacs-macros))

(treemacs-import-functions-from "treemacs"
  treemacs-select-window)

;;;###autoload
(defun treemacs-bookmark (&optional arg)
  "Find a bookmark in treemacs.
Only bookmarks marking either a file or a directory are offered for selection.
Treemacs will try to find and focus the given bookmark's location, in a similar
fashion to `treemacs-find-file'.

With a prefix argument ARG treemacs will also open the bookmarked location."
  (interactive "P")
  (treemacs-block
   (bookmark-maybe-load-default-file)
   (-let [bookmarks
          (cl-loop
           for b in bookmark-alist
           for name = (car b)
           for location = (treemacs-canonical-path (bookmark-location b))
           when (or (file-regular-p location) (file-directory-p location))
           collect (propertize name 'location location))]
     (treemacs-error-return-if (null bookmarks)
       "Didn't find any bookmarks pointing to files.")
     (let* ((bookmark (completing-read "Bookmark: " bookmarks))
            (location (treemacs-canonical-path (get-text-property 0 'location (--first (string= it bookmark) bookmarks))))
            (dir (if (file-directory-p location) location (treemacs--parent-dir location)))
            (project (treemacs--find-project-for-path dir)))
       (treemacs-error-return-if (null project)
         "Bookmark at %s does not fall under any project in the workspace."
         (propertize location 'face 'font-lock-string-face))
       (pcase (treemacs-current-visibility)
         ('visible (treemacs--select-visible-window))
         ('exists  (treemacs--select-not-visible-window))
         ('none    (treemacs--init)))
       (treemacs-goto-file-node location project)
       (treemacs-pulse-on-success)
       (when arg (treemacs-visit-node-no-split))))))

;;;###autoload
(defun treemacs--bookmark-handler (record)
  "Open Treemacs into a bookmark RECORD."
  (let ((path (bookmark-prop-get record 'treemacs-bookmark-path)))
    (unless path
      ;; Don't rely on treemacs-pulse-on-failure to display the error, since the
      ;; error must be handled in bookmark.el.
      (user-error "Treemacs--bookmark-handler invoked for a non-Treemacs bookmark"))
    (treemacs-select-window)
    ;; XXX temporary workaround for incorrect move to a saved tag node
    ;; must be fixed after tags were rewritten in new extension api
    (if (and (listp path)
             (stringp (car path))
             (file-regular-p (car path)))
        (treemacs-goto-node (car path))
      (treemacs-goto-node path))
    ;; If the user has bookmarked a directory, they probably want to operate on
    ;; its contents. Expand it, and select the first child.
    (treemacs-with-current-button
     "Could not select the current bookmark"
     (when (eq (treemacs-button-get current-btn :state) 'dir-node-closed)
       (treemacs-TAB-action))
     (when (eq (treemacs-button-get current-btn :state) 'dir-node-open)
       (let ((depth (treemacs-button-get current-btn :depth))
             (next-button (next-button current-btn)))
         (when (and next-button (> (treemacs-button-get next-button :depth) depth))
           (treemacs-next-line 1)))))))

(defun treemacs--format-bookmark-title (btn)
  "Format the bookmark title for BTN with `treemacs-bookmark-title-template'."
  (s-format
   treemacs-bookmark-title-template
   (lambda (pattern)
     (or
      (cond
       ;; ${label} - Label of the current button
       ((string= pattern "label")
        (treemacs--get-label-of btn))

       ;; ${label:1} - Label of Nth parent
       ((s-starts-with? "label:" pattern)
        (let ((depth (string-to-number (s-chop-prefix "label:" pattern)))
              (current-button btn))
          (dotimes (_ depth)
            (setq current-button (when current-button (treemacs-button-get current-button :parent))))
          (when current-button
            (treemacs--get-label-of current-button))))

       ;; ${label-path} and ${label-path:4} - Path of labels, optionally limited by a number.
       ((or (string= pattern "label-path") (s-starts-with? "label-path:" pattern))
        (let ((depth (when (s-starts-with? "label-path:" pattern)
                       (string-to-number (s-chop-prefix "label-path:" pattern))))
              (current-button btn)
              (path))
          (while (and current-button (not (eq 0 depth)))
            (push (treemacs--get-label-of current-button) path)
            (when depth (cl-decf depth))
            (setq current-button (treemacs-button-get current-button :parent)))
          (s-join "/" path)))

       ;; ${project} - Label of the project or top-level extension node.
       ((string= pattern "project")
        ;; Find the root button by iterating - don't use `treemacs-project-of-node`
        ;; to make this work for variadic top-level extensions.
        (let ((current-button btn))
          (while (> (treemacs-button-get current-button :depth) 0)
            (setq current-button (treemacs-button-get current-button :parent)))
          (treemacs--get-label-of current-button)))

       ;; ${file-path} - Filesystem path.
       ((string= pattern "file-path")
        (treemacs--nearest-path btn))

       ;; ${file-path:3} - N components of the file path
       ((s-starts-with? "file-path:" pattern)
        (let ((n (string-to-number (s-chop-prefix "file-path:" pattern))))
          (-when-let (path (treemacs--nearest-path btn))
            (let ((components (last (s-split "/" path) (1+ n))))
              ;; Add the leading slash for absolute paths
              (when (and (> (length components) n) (not (string= "" (car components))))
                (pop components))
              (s-join "/" components)))))

       (t
        ;; Don't rely on treemacs-pulse-on-failure to display the error, since the
        ;; error must be handled in bookmark.el.
        (treemacs-pulse-on-failure)
        (user-error "Bookmark template pattern %s was not recognized" pattern)))
      ""))))

(defun treemacs--make-bookmark-record ()
  "Make a bookmark record for the current Treemacs button.

This function is installed as the `bookmark-make-record-function'."
  (treemacs-unless-let (current-btn (treemacs-current-button))
      (progn
        ;; Don't rely on treemacs-pulse-on-failure to display the error, since the
        ;; error must be handled in bookmark.el.
        (treemacs-pulse-on-failure)
        (user-error "Nothing to bookmark here"))
    (let* ((path (treemacs-button-get current-btn :path)))
      (unless path
        (treemacs-pulse-on-failure)
        (user-error "Could not find the path of the current button"))

      `((defaults . (,(treemacs--format-bookmark-title current-btn)))
        (treemacs-bookmark-path . ,path)
        (handler . treemacs--bookmark-handler)
        ,@(when (stringp path) `((filename . ,path)))))))

;;;###autoload
(defun treemacs-add-bookmark ()
  "Add the current node to Emacs' list of bookmarks.
For file and directory nodes their absolute path is saved.  Tag nodes
additionally also save the tag's position.  A tag can only be bookmarked if the
treemacs node is pointing to a valid buffer position."
  (interactive)
  (treemacs-with-current-button
   "There is nothing to bookmark here."
   (pcase (treemacs-button-get current-btn :state)
     ((or 'file-node-open 'file-node-closed 'dir-node-open 'dir-node-closed)
      (-let [name (treemacs--read-string "Bookmark name: ")]
        (bookmark-store name `((filename . ,(treemacs-button-get current-btn :path))) nil)))
     ('tag-node
      (-let [(tag-buffer . tag-pos)
             (treemacs--extract-position (treemacs-button-get current-btn :marker) nil)]
        (if (buffer-live-p tag-buffer)
            (bookmark-store
             (treemacs--read-string "Bookmark name: ")
             `((filename . ,(buffer-file-name tag-buffer))
               (position . ,tag-pos))
             nil)
          (treemacs-log-failure "Tag info can not be saved because it is not pointing to a live buffer."))))
     ((or 'tag-node-open 'tag-node-closed)
      (treemacs-pulse-on-failure "There is nothing to bookmark here.")))))

(provide 'treemacs-bookmarks)

;;; treemacs-bookmarks.el ends here
