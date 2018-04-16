;;; treemacs.el --- A tree style file explorer package -*- lexical-binding: t -*-

;; Copyright (C) 2018 Alexander Miller

;; Author: Alexander Miller <alexanderm@web.de>
;; Package-Requires: ((emacs "24.4") (cl-lib "0.5") (dash "2.11.0") (s "1.10.0") (f "0.11.0") (ace-window "0.9.0") (pfuture "1.2") (hydra "0.13.2") (ht "2.2"))
;; Homepage: https://github.com/Alexander-Miller/treemacs
;; Version: 1.18.1

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

;;; A powerful and flexible file tree project explorer.

;;; Code:

(require 'dash)
(require 's)
(require 'f)
(require 'bookmark)
(require 'treemacs-customization)
(require 'treemacs-faces)
(require 'treemacs-visuals)
(require 'treemacs-branch-creation)
(require 'treemacs-impl)
(require 'treemacs-follow-mode)
(require 'treemacs-filewatch-mode)
(require 'treemacs-mode)
(require 'treemacs-interface)
(require 'treemacs-mouse-interface)
(require 'treemacs-persistence)
(require 'treemacs-tags)
(require 'treemacs-tag-follow-mode)
(require 'treemacs-async)
(require 'treemacs-compatibility)
(require 'treemacs-workspaces)
(eval-and-compile
  (require 'cl-lib)
  (require 'treemacs-macros))

(defconst treemacs-version "1.18.1")

;;;###autoload
(defun treemacs ()
  "Initialize or toggle treemacs.
* If the treemacs window is visible hide it.
* If a treemacs buffer exists, but is not visible show it.
* If no treemacs buffer exists for the current frame create and show it.
* If the workspace is empty additionally ask for the root path of the first
  project to add."
  (interactive)
  (treemacs--init (when (treemacs-workspace->is-empty?)
             (read-directory-name "Project root: "))))

;;;###autoload
(defun treemacs-bookmark (&optional arg)
  "Find a bookmark in treemacs.
Only bookmarks marking either a file or a directory are offered for selection.
Treemacs will try to find and focus the given bookmark's location, in a similar
fashion to `treemacs-find-file'.

With a prefix argument ARG treemacs will also open the bookmarked location."
  (interactive "P")
  (-let [bookmarks
            (cl-loop
             for b in bookmark-alist
             for name = (car b)
             for location = (bookmark-location b)
             when (or (f-file? location) (f-directory? location))
             collect (propertize name 'location location))]
    (if (null bookmarks)
        (treemacs-log "Didn't find any bookmarks pointing to files.")
      (-let*- [(bookmark (completing-read "Bookmark: " bookmarks))
               (location (f-long (get-text-property 0 'location (--first (string= it bookmark) bookmarks))))
               (dir (if (f-directory? location) location (f-dirname location)))
               (project (treemacs--find-project-for-path dir))]
        (cl-block body
          (unless project
            (cl-return-from body
              (treemacs-pulse-on-failure "Bookmark at %s does not fall under any project in the workspace."
                (propertize location 'face 'font-lock-string-face))))
          (-pcase (treemacs--current-visibility)
            [`visible (treemacs--select-visible)]
            [`exists (treemacs--select-not-visible)]
            [`none (treemacs--init)])
          (treemacs-goto-button location project)
          (treemacs-pulse-on-success)
          (when arg (treemacs-visit-node-no-split)))))))

;;;###autoload
(defun treemacs-find-file ()
  "Find and focus the current file in the treemacs window.
Will show/create a treemacs buffers if it is not visible/does not exist.
Only useful when `treemacs-follow-mode' is not active."
  (interactive)
  (cl-block body
    (-let*- [(buffer-file (buffer-file-name (current-buffer)))
             (project (treemacs--find-project-for-buffer))]
      (unless buffer-file
        (treemacs-pulse-on-failure "Current buffer is not visiting a file.")
        (cl-return-from body))
      (unless project
        (treemacs-pulse-on-failure (format "%s does not fall under any project in the workspace."
                                    (propertize buffer-file 'face 'font-lock-string-face)))
        (cl-return-from body))
      (save-selected-window
        (-pcase (treemacs--current-visibility)
          [`visible (treemacs--select-visible)]
          [`exists (treemacs--select-not-visible)]
          [`none (treemacs--init)])
        (treemacs-goto-button buffer-file project)))))

;;;###autoload
(defun treemacs-find-tag ()
  "Find and move point to the tag at point in the treemacs view.
Most likley to be useful when `treemacs-tag-follow-mode' is not active.

Will ask to change the treemacs root if the file to find is not under the
root. If no treemacs buffer exists it will be created with the current file's
containing directory as root. Will do nothing if the current buffer is not
visiting a file or Emacs cannot find any tags for the current file."
  (interactive)
  (cl-block body
    (-let*- [(buffer (current-buffer))
             (buffer-file (when buffer (buffer-file-name buffer)))
             (project (treemacs--find-project-for-buffer))
             (index (when buffer-file (treemacs--flatten&sort-imenu-index)))
             (treemacs-window nil)]
      (unless buffer-file
        (treemacs-pulse-on-failure "Current buffer is not visiting a file.")
        (cl-return-from body))
      (unless index
        (treemacs-pulse-on-failure "Current buffer has no tags.")
        (cl-return-from body))
      (unless project
        (treemacs-pulse-on-failure (format "%s does not fall under any project in the workspace."
                                    (propertize buffer-file 'face 'font-lock-string-face)))
        (cl-return-from body))
      (save-selected-window
        (-pcase (treemacs--current-visibility)
          [`visible (treemacs--select-visible)]
          [`exists (treemacs--select-not-visible)]
          [`none (treemacs--init)])
        (setq treemacs-window (selected-window)))
      (treemacs--do-follow-tag index treemacs-window buffer-file project))))

;;;###autoload
(defun treemacs-select-window ()
  "Select the treemacs window if it is visible.
Call `treemacs' if it is not."
  (interactive)
  (force-mode-line-update)
  (--if-let (treemacs--is-visible?)
      (select-window it t)
    (treemacs)))

(provide 'treemacs)

;;; treemacs.el ends here
