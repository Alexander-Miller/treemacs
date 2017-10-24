;;; treemacs.el --- A tree style file explorer package -*- lexical-binding: t -*-

;; Copyright (C) 2017 Alexander Miller

;; Author: Alexander Miller <alexanderm@web.de>
;; Package-Requires: ((emacs "24.4") (cl-lib "0.5") (dash "2.11.0") (s "1.10.0") (f "0.11.0") (ace-window "0.9.0") (pfuture "1.2") (hydra "0.13.2"))
;; Homepage: https://github.com/Alexander-Miller/treemacs
;; Version: 1.12.1

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

;;; Code:

(require 'dash)
(require 's)
(require 'f)
(require 'cl-lib)
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
(require 'treemacs-persist)
(require 'treemacs-tags)
(require 'treemacs-tag-follow-mode)
(require 'treemacs-async)
(require 'treemacs-compatibility)

(defconst treemacs-version "1.12.1")

;;;###autoload
(defun treemacs-toggle ()
  "If a treemacs buffer exists and is visible hide it.
If a treemacs buffer exists, but is not visible bring it to the foreground
and select it.
If no treemacs buffer exists call `treemacs'."
  (interactive)
  (cond
   ((treemacs--is-visible?)
    (treemacs--select-visible)
    (treemacs--refresh-on-ui-change)
    (if (one-window-p)
        (switch-to-buffer (other-buffer))
      (bury-buffer)))
   ((treemacs--buffer-exists?)
    (treemacs--select-not-visible)
    (treemacs--refresh-on-ui-change))
   (t
    (treemacs))))

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
        (if (treemacs--buffer-exists?)
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
          (treemacs-visit-node-default-action)))))

;;;###autoload
(defun treemacs-refresh ()
  "Refresh and rebuild treemacs buffer."
  (interactive)
  (-if-let (treemacs-buffer (get-buffer treemacs--buffer-name))
      (treemacs--without-following
       (with-selected-window (get-buffer-window treemacs-buffer)
         (let* ((curr-line    (line-number-at-pos))
                (curr-btn     (treemacs--current-button))
                (curr-state   (when curr-btn (button-get curr-btn 'state)))
                (curr-file    (when curr-btn (treemacs--nearest-path curr-btn)))
                (curr-tagpath (when curr-btn (treemacs--tags-path-of curr-btn)))
                (win-start    (window-start (get-buffer-window)))
                (root         (treemacs--current-root)))
           (treemacs--build-tree root)
           ;; move point to the same file it was with before the refresh if the file
           ;; still exists and is visible, stay in the same line otherwise
           (pcase curr-state
             ((or `dir-node-open `dir-node-closed `file-node-open `file-node-closed)
              (if (and (f-exists? curr-file)
                       (or treemacs-show-hidden-files
                           (not (s-matches? treemacs-dotfiles-regex (f-filename curr-file)))))
                  (treemacs--goto-button-at curr-file)
                ;; not pretty, but there can still be some off by one jitter when
                ;; using forwald-line
                (treemacs--without-messages (with-no-warnings (goto-line curr-line)))))
             ((or `tag-node-open `tag-node-closed `tag-node)
              (treemacs--goto-tag-button-at curr-tagpath curr-file win-start))
             ((pred null)
              (with-no-warnings (goto-line 1)))
             (_ (treemacs--log "Refresh doesn't yet know how to deal with '%s'" curr-state)))
           (treemacs--evade-image)
           (set-window-start (get-buffer-window) win-start)
           ;; needs to be turned on again when refresh is called from outside the
           ;; treemacs window, otherwise it looks like the selection disappears
           (hl-line-mode t)
           (unless treemacs-silent-refresh
             (treemacs--log "Refresh complete.")))))
    (treemacs--log "There is nothing to refresh.")))

(provide 'treemacs)

;;; treemacs.el ends here
