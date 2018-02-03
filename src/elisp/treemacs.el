;;; treemacs.el --- A tree style file explorer package -*- lexical-binding: t -*-

;; Copyright (C) 2018 Alexander Miller

;; Author: Alexander Miller <alexanderm@web.de>
;; Package-Requires: ((emacs "24.4") (cl-lib "0.5") (dash "2.11.0") (s "1.10.0") (f "0.11.0") (ace-window "0.9.0") (pfuture "1.2") (hydra "0.13.2") (ht "2.3"))
;; Homepage: https://github.com/Alexander-Miller/treemacs
;; Version: 1.18

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
(require 'treemacs-structure)
(require 'treemacs-visuals)
(require 'treemacs-branch-creation)
(require 'treemacs-impl)
(require 'treemacs-follow-mode)
(require 'treemacs-filewatch-mode)
(require 'treemacs-mode)
(require 'treemacs-interface)
(require 'treemacs-mouse-interface)
(require 'treemacs-persist)
(require 'treemacs-tags)
(require 'treemacs-tag-follow-mode)
(require 'treemacs-async)
(require 'treemacs-compatibility)
(eval-and-compile
  (require 'cl-lib)
  (require 'treemacs-macros))

(defconst treemacs-version "1.18")

;;;###autoload
(defun treemacs-toggle ()
  "If a treemacs buffer exists and is visible hide it.
If a treemacs buffer exists, but is not visible bring it to the foreground
and select it.
If no treemacs buffer exists call `treemacs'."
  (interactive)
  (-pcase (treemacs--current-visibility)
    [`visible
     (treemacs--select-visible)
     (if (one-window-p)
         (switch-to-buffer (other-buffer))
       (bury-buffer))]
    [`exists
     (treemacs--select-not-visible)]
    [`none
     (treemacs)]
    [_ (error "[Treemacs] Invalid visibility value: %s" (treemacs--current-visibility))]))

;;;###autoload
(defun treemacs (&optional arg)
  "Open treemacs with current buffer's directory as root.
If the current buffer's `default-directory' is nil, use $HOME as fallback.
If a prefix argument ARG is given manually select the root directory."
  (interactive "P")
  (treemacs--init (cond
                   (arg (read-directory-name "Treemacs root: "))
                   (default-directory default-directory)
                   (t (getenv "HOME")))))

;;;###autoload
(defun treemacs-bookmark (&optional arg)
  "Find a bookmark in treemacs.
Only bookmarks marking either a file or a directory are offered for selection.
Treemacs will try to find and focus the given bookmark's location. If it cannot
do that it will instead rebuild its view with the bookmark's location as
its root.

With a prefix argument ARG treemacs will also open the bookmarked location."
  (interactive "P")
  (-if-let (bookmarks
            (cl-loop
             for b in bookmark-alist
             for name = (car b)
             for location = (bookmark-location b)
             when (or (f-file? location) (f-directory? location))
             collect (propertize name 'location location)))
      (let* ((bookmark (completing-read "Bookmark: " bookmarks))
             (location (f-long (get-text-property 0 'location (--first (string= it bookmark) bookmarks))))
             (dir (if (f-directory? location) location (f-dirname location))))
        (if (treemacs-buffer-exists?)
            (progn
              (if (treemacs--is-visible?)
                  (treemacs--select-visible)
                (treemacs-toggle))
              (treemacs-select-window)
              (if (treemacs--is-path-in-dir? location (treemacs--current-root))
                (treemacs--init dir)))
          (treemacs--init dir))
        (treemacs--goto-button-at location)
        (when arg
          (treemacs-RET-action)))))

;;;###autoload
(defun treemacs-refresh ()
  "Refresh and rebuild treemacs buffer."
  (interactive)
  (-if-let (b (treemacs-buffer-exists?))
      (treemacs--do-refresh b)
    (treemacs-log "There is nothing to refresh.")))

;;;###autoload
(defun treemacs-find-file ()
  "Find and focus the current file in the treemacs window.
Most likley to be useful when `treemacs-follow-mode' is not active.

Will ask to change the treemacs root if the file to find is not under the
root. If no treemacs buffer exists it will be created with the current file's
containing directory as root. Will do nothing if the current buffer is not
visiting a file."
  (interactive)
  (-let [path (buffer-file-name (current-buffer))]
    (when (and path (f-exists? path))
      (save-selected-window
        (treemacs-without-following
         (-let*- [(visibility (treemacs--current-visibility))
                  (path       (f-long path))
                  (is-dir?    (f-directory? path))]
           (if (eq visibility 'none)
               ;; finding a file without an existing treemacs is just an init
               ;; nothing else to do here
               (treemacs--init (if is-dir? path (f-dirname path)))
             (-pcase visibility
               [`exists  (treemacs-toggle)]
               [`visible (treemacs--select-visible)]
               [other    (error "Unkown treemacs buffer visibility '%s'" other)])
             ;; find the file given whether is a directory and if it can be found below
             ;; the current root or not
             (-let [root (treemacs--current-root)]
               (if (treemacs--is-path-in-dir? path root)
                   (treemacs--do-follow path root)
                 (when (or treemacs-change-root-without-asking
                           (y-or-n-p "Change the treemacs root to find the file? "))
                   (if is-dir?
                       (treemacs--init path)
                     (progn
                       (treemacs--init (f-dirname path))
                       (treemacs--goto-button-at path)
                       (hl-line-highlight)))))))))))))

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
    (let* ((buffer (current-buffer))
           (buffer-file (when buffer (buffer-file-name buffer)))
           (index (when buffer-file (treemacs--flatten&sort-imenu-index)))
           (treemacs-window))
      (unless buffer-file
        (treemacs-log "Nothing to find - current buffer is not visiting a file.")
        (cl-return-from body))
      (unless index
        (treemacs-log "Nothing to find - current buffer has no tags.")
        (cl-return-from body))
      (save-selected-window
        (-pcase (treemacs--current-visibility)
          [`none
           (treemacs-toggle)]
          [visibility
           (if (eq 'exists visibility)
               (treemacs--select-not-visible)
             (treemacs--select-visible))
           (unless (treemacs--is-path-in-dir? buffer-file (treemacs--current-root))
             (if (y-or-n-p "Change the root to find current tag? ")
                 (treemacs--init (f-dirname buffer-file))
               (treemacs-log "Root not changed, tag not followed.")
               (cl-return-from body)))])
        (setq treemacs-window (selected-window)))
      (treemacs--do-follow-tag index treemacs-window buffer-file))))

;;;###autoload
(defun treemacs-select-window ()
  "Select the treemacs window if it is visible.
Call `treemacs-toggle' if it is not."
  (interactive)
  (force-mode-line-update)
  (-if-let (w (treemacs--is-visible?))
      (select-window w t)
    (treemacs-toggle)))

(provide 'treemacs)

;;; treemacs.el ends here
