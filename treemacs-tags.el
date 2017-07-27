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
;;; Tags display functionality.

;;; Code:

(require 'imenu)
(require 'dash)
(require 'cl-lib)
(require 'f)
(require 'treemacs-impl)
(require 'treemacs-branch-creation)
(require 'treemacs-customization)

(defconst treemacs-icon-tag-leaf        (create-image (f-join treemacs-dir "icons/" "tags-leaf.xpm")   'xpm nil :ascent 'center))
(defconst treemacs-icon-tag-node-closed (create-image (f-join treemacs-dir "icons/" "tags-closed.xpm") 'xpm nil :ascent 'center))
(defconst treemacs-icon-tag-node-open   (create-image (f-join treemacs-dir "icons/" "tags-open.xpm")   'xpm nil :ascent 'center))

(defvar treemacs--tags-cache (make-hash-table :test #'equal :size 100)
  "Cache storing all opened tags in treemacs view.
The cache has 2 levels. The 1st is this has table, its keys are the absolute
paths of files whose tags are open, its values are the 2nd level, another hash
table mapping node's tag path (as given by `treemacs--tags-path-of') to a list
of tag paths of its open direct children.")

(defsubst treemacs--clear-tags-cache ()
  "Clear out `treemacs--tags-cache'."
  (clrhash treemacs--tags-cache))

(defsubst treemacs--tags-path-of (btn)
  "Return the path of tag labels leading to BTN.

The car of the returned list is the label of BTN while its cdr is the top down
path starting at the first non-file button.

These paths are used to uniquely identify nodes not part of the filesystem, e.g.
in `treemacs--tags-cache'.
This is also why if the button is not a tag node its 'abs-path' property is
returned as a singleton list instead."
  (-if-let (path (button-get btn 'abs-path))
      (list path)
    (let ((lbl (treemacs--get-label-of btn))
          (btn (button-get btn 'parent))
          (ret))
      (while (and btn (null (button-get btn 'abs-path)))
        (push (treemacs--get-label-of btn) ret)
        (setq btn (button-get btn 'parent)))
      (cons lbl ret))))

(defsubst treemacs--partition-imenu-index (index)
  "Make sure that top level items in INDEX are put under a 'Functions' sublist.
Make it look like helm-imenu."
  (let ((ret)
        (rest index))
    (while rest
      (let ((item (car rest)))
        (if (imenu--subalist-p item)
            (progn
              (setq ret (cons item ret))
              (setq rest (cdr rest)))
          (progn
            (setq ret (cons (cons "Functions" rest) ret))
            (setq rest nil)))))
    (nreverse ret)))

(defun treemacs--get-imenu-index (file)
  "Fetch imenu index of FILE."
  (let ((buff)
        (result)
        (existing-buffer (get-file-buffer file)))
    (if existing-buffer
        (setq buff existing-buffer)
      (setq buff (find-file-noselect file)))
    (with-current-buffer buff
      (setq result (imenu--make-index-alist t)))
    (unless existing-buffer (kill-buffer buff))
    (when result
      (when (string= "*Rescan*" (car (car result)))
        (setq result (cdr result)))
      (unless (equal result '(nil))
        (treemacs--partition-imenu-index result)))))

(defun treemacs--add-to-tags-cache (btn)
  "Add BTN's path to the cache of open nodes."
  (let* ((file            (treemacs--nearest-path btn))
         (cache-table     (gethash file treemacs--tags-cache))
         (cache-key       (treemacs--tags-path-of (button-get btn 'parent)))
         (new-cache-entry (treemacs--tags-path-of btn)))
    (unless cache-table
      (setq cache-table (make-hash-table :test #'equal :size 20))
      (puthash file cache-table treemacs--tags-cache))
    (let* ((present-cache   (gethash cache-key cache-table))
           (new-cache-value (cons new-cache-entry present-cache)))
      (puthash cache-key new-cache-value cache-table))))

(defun treemacs--remove-from-tags-cache (btn)
  "Remove BTN's path from the cache of open nodes."
  (let* ((file           (treemacs--nearest-path btn))
         (cache-table    (gethash file treemacs--tags-cache))
         (cache-key      (-if-let (path (button-get (button-get btn 'parent) 'abs-path))
                             (list path)
                           (treemacs--tags-path-of btn)))
         (present-cache   (gethash cache-key cache-table))
         (entry-to-delete (treemacs--tags-path-of btn))
         (new-cache-value (delete entry-to-delete present-cache)))
    (if new-cache-value
        (puthash cache-key new-cache-value cache-table)
      (remhash cache-key cache-table))))

(defun treemacs--open-tags-for-file (btn &optional noadd)
  "Open tag items for file BTN.
Do not add the file to the open file cache when NOADD is given. NOADD is given
during a reopen process."
  (let ((path (button-get btn 'abs-path)))
    (-if-let (index (treemacs--get-imenu-index path))
          (treemacs--button-open
           :button btn
           :new-state 'file-open
           :post-open-action
           (progn
             (unless noadd (treemacs--add-to-cache (treemacs--parent path) path))
             (treemacs--reopen-tags-under btn))
           :open-action
           (treemacs--create-buttons
            :nodes index
            :indent-depth (1+ (button-get btn 'depth))
            :node-name item
            :node-action
            (if (imenu--subalist-p item)
                (treemacs--insert-tag-node item prefix treemacs-icon-tag-node-closed btn indent-depth insert-depth)
              (treemacs--insert-tag-leaf item prefix treemacs-icon-tag-leaf btn indent-depth insert-depth))))
      (treemacs--log "No imenu index found for %s" (propertize path 'face 'font-lock-string-face)))))

(defun treemacs--close-tags-for-file (btn)
  "Close node given by BTN."
  (treemacs--button-close
   :button btn
   :new-state 'file-closed
   :post-close-action
   (treemacs--clear-from-cache (button-get btn 'abs-path))))

(defun treemacs--insert-tag-node (node prefix icon parent depth insert-depth)
  "Insert tags NODE.
Use PREFIX for indentation.
Use ICON for the node.
Set PARENT and DEPTH button properties.
Insert ICON at INSERT-DEPTH."
  (end-of-line)
  (let ((start (+ 1 (point) insert-depth)))
    (insert prefix)
    (add-text-properties start (1+ start) `(display ,icon)))
  (treemacs--insert-button (car node)
                           'face 'font-lock-builtin-face
                           'state 'node-closed
                           'action 'treemacs--push-button
                           'parent parent
                           'depth depth
                           'index (cdr node)))

(defun treemacs--open-tag-node (btn &optional noadd)
  "Open tags node items for BTN.
Do not add the node the open file cache when NOADD is given.
NOADD is usually given during a reopen process."
  (let ((index (button-get btn 'index)))
    (treemacs--button-open
        :button btn
        :new-state 'node-open
        :new-icon treemacs-icon-tag-node-open
        :post-open-action
        (unless noadd (treemacs--add-to-tags-cache btn))
        :open-action
        (treemacs--create-buttons
         :nodes index
         :indent-depth (1+ (button-get btn 'depth))
         :node-name item
         :node-action
         (if (imenu--subalist-p item)
             (treemacs--insert-tag-node item prefix treemacs-icon-tag-node-closed btn indent-depth insert-depth)
           (treemacs--insert-tag-leaf item prefix treemacs-icon-tag-leaf btn indent-depth insert-depth))))
    (treemacs--reopen-tags-under btn)))

(defun treemacs--insert-tag-leaf (item prefix icon parent depth insert-depth)
  "Insert tag node ITEM.
Use PREFIX for indentation.
Use ICON for the item.
Set PARENT and DEPTH button properties.
Insert ICON at INSERT-DEPTH."
  (end-of-line)
  (let ((start (+ 1 (point) insert-depth)))
    (insert prefix)
    (add-text-properties start (1+ start) `(display ,icon)))
  (treemacs--insert-button (car item)
                           'face 'font-lock-builtin-face
                           'state 'tag
                           'action 'treemacs--push-button
                           'parent parent
                           'depth depth
                           'marker (cdr item)))

(defun treemacs--close-tag-node (btn)
 "Close tags node at BTN."
 (treemacs--with-writable-buffer
  (treemacs--node-symbol-switch treemacs-icon-tag-node-closed))
 (treemacs--button-close
  :button btn
  :new-state 'node-closed
  :post-close-action
  (treemacs--remove-from-tags-cache btn)))

(defsubst treemacs--pos-from-marker (m)
  "Extract the tag position stored in marker M.
The position can be stored in M in 2 ways:

* M is a marker pointing to a tag provided by imenu
* M is an overlay pointing to a tag provided by imenu with semantic mode

Either way the return value is a 2 element list consisting of the buffer and the
position of the tag. They might also be nil if the pointed-to buffer does not
exist."
  (if (overlayp m)
      (list (overlay-buffer m) (overlay-start m))
    (list (marker-buffer m) (marker-position m))))

(defsubst treemacs--call-imenu-and-goto-tag (file tag-path window)
  "Call the imenu index of FILE to go to position of TAG-PATH in WINDOW."
  (let ((current-window (selected-window))
        (tag (car tag-path))
        (path (cdr tag-path)))
    (condition-case e
        (progn
          (select-window window)
          (find-file-existing file)
          (let ((index (treemacs--get-imenu-index file)))
            (dolist (path-item path)
              (setq index (cdr (assoc path-item index))))
            (-let [(_ pos) (treemacs--pos-from-marker (cdr (--first (equal (car it) tag) index)))]
              (goto-char pos))))
      (error
       (progn
         (select-window current-window)
         (treemacs--log "Something went run when finding tag '%s': %s"
                        (propertize tag 'face font-lock-type-face)
                        e))))))

(defun treemacs--goto-tag (btn)
  "Go to the tag at BTN."
  (let* ((marker     (button-get btn 'marker))
         (pos-info   (treemacs--pos-from-marker marker))
         (tag-buffer (car pos-info))
         (tag-pos    (cadr pos-info))
         (tag-name   (treemacs--get-label-of btn)))
    (if tag-buffer
        (progn
          (-if-let (buf-window (get-buffer-window tag-buffer (selected-frame)))
              (select-window buf-window)
            (select-window (next-window (selected-window) nil (selected-frame)))
            (switch-to-buffer tag-buffer nil t))
          (goto-char tag-pos)))
    (progn
      (pcase treemacs-goto-tag-strategy
        ('refetch-index
         (treemacs--call-imenu-and-goto-tag
          (treemacs--nearest-path btn)
          (treemacs--tags-path-of btn)
          (next-window (selected-window) nil (selected-frame))))
        ('call-xref
         (select-window (next-window (selected-window) nil (selected-frame)))
         (xref-find-definitions tag-name))
        ('issue-warning
         (treemacs--log "Tag '%s' is located in a buffer that does not exist."
                        (propertize tag-name 'face 'font-lock-type-face)))
        (_ (error "[Treemacs] '%s' is an invalid value for treemacs-goto-tag-strategy" treemacs-goto-tag-strategy))))))

(defun treemacs--goto-tag-button-at (tag-path file &optional start)
  "Goto tag given by TAG-PATH for button of FILE.
Start the search at START."
  (let ((tag (car tag-path))
        (path (cdr tag-path)))
    (-when-let (btn (treemacs--goto-button-at file start))
      (dolist (tag-path-item path)
        (-if-let (tag-path-btn (--first
                                (string= (treemacs--get-label-of it) tag-path-item)
                                (treemacs--get-children-of btn)))
            (setq btn tag-path-btn)
          (error "[Treemacs] Couldn't go to tag button %s in path %s" tag-path-item tag-path)))
      (let ((pos (button-start (--first (string= (treemacs--get-label-of it) tag)
                                        (treemacs--get-children-of btn)))))
        (goto-char pos)
        (treemacs--button-at pos)))))

(defun treemacs--reopen-tags-under (btn)
  "Reopen previously openeded tags under BTN."
  (save-excursion
    (let* ((file (treemacs--nearest-path btn))
           (cache-table (gethash file treemacs--tags-cache)))
      (when cache-table
        (let* ((cache-key (treemacs--tags-path-of btn))
               (cache (gethash cache-key cache-table))
               (rejects))
          (dolist (item cache)
            (-if-let (node-btn (--first (equal item (treemacs--tags-path-of it))
                                        (treemacs--get-children-of btn)))
                (when (eq 'node-closed (button-get node-btn 'state))
                  (goto-char (button-start node-btn))
                  (treemacs--open-tag-node node-btn t))
              (remhash item cache-table)
              (push item rejects)))
          ;; nodes that could not be moved to - probably due to those nodes
          ;; being deleted, but still remaining in the cache
          ;; if theyre not accessible we just remove them from the cache
          (when rejects
            (let ((new-cache (--reject (member it rejects) cache)))
              (if new-cache
                  (puthash cache-key new-cache cache-table)
                (remhash cache-key cache-table)))))))))

(provide 'treemacs-tags)

;;; treemacs-tags.el ends here
