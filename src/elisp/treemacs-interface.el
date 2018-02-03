;;; treemacs.el --- A tree style file viewer package -*- lexical-binding: t -*-

;; Copyright (C) 2018 Alexander Miller

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
;;; Not autoloaded, but user-facing functions.

;;; Code:

(require 'hl-line)
(require 'bookmark)
(require 'f)
(require 's)
(require 'dash)
(require 'treemacs-impl)
(require 'treemacs-filewatch-mode)
(require 'treemacs-branch-creation)
(require 'treemacs-follow-mode)
(require 'treemacs-tag-follow-mode)
(require 'treemacs-mouse-interface)
(require 'treemacs-customization)
(eval-and-compile
  (require 'cl-lib)
  (require 'treemacs-macros))

(treemacs-import-functions-from "treemacs"
  treemacs-refresh
  treemacs-toggle)

(defconst treemacs-valid-button-states
  '(dir-node-open
    dir-node-closed
    file-node-open
    file-node-closed
    tag-node-open
    tag-node-closed
    tag-node)
  "List of all valid values for treemacs buttons' state property.")

(defun treemacs-next-line (&optional count)
  "Goto next line.
A COUNT argument, moves COUNT lines down."
  (interactive "p")
  (forward-line count)
  (treemacs--evade-image))

(defun treemacs-previous-line (&optional count)
  "Goto previous line.
A COUNT argument, moves COUNT lines up."
  (interactive "p")
  (forward-line (- count))
  (treemacs--evade-image))

(defun treemacs-push-button (&optional arg)
  "Push the button in the current line.
For directories, files and tag sections expand/close the button.
For tags go to the tag definition via `treemacs-visit-node-no-split'.

With a prefix ARG expanding and closing of nodes is recursive."
  (interactive "P")
  (save-excursion
    (-when-let (b (treemacs-current-button))
      (treemacs--push-button b arg)))
  (treemacs--evade-image))
(make-obsolete 'treemacs-push-button 'treemacs-toggle-node "Treemacs v1.18")

(defun treemacs-toggle-node (&optional arg)
  "Expand or close the current node.
If a prefix ARG is provided the open/close process is done recursively. When
opening directories that means that all sub-directories are opened as well. When
opening files all their tag sections will be opened.
Recursively closing any kind of node means that treemacs will forget about
everything that was expanded below that node.

Since tags cannot be opened or closed a goto definition action will called on
them instead."
  (treemacs-do-for-button-state
   :on-dir-node-open    (treemacs--collapse-dir-node btn arg)
   :on-dir-node-closed  (treemacs--expand-dir-node btn :recursive arg)
   :on-file-node-open   (treemacs--collapse-tags-for-file btn arg)
   :on-file-node-closed (treemacs--expand-tags-for-file btn arg)
   :on-tag-node-open    (treemacs--collapse-tag-node btn arg)
   :on-tag-node-closed  (treemacs--expand-tag-node btn arg)
   :on-tag-node-leaf    (progn (other-window 1) (treemacs--goto-tag btn))
   :on-nil              (treemacs-pulse-on-failure "There is nothing to do here.")))

(defun treemacs-TAB-action (&optional arg)
  "Run the appropriate TAB action for the current node.

In the default configuration this usually means to expand or close the content
of the currently selected node. A potential prefix ARG is passed on to the
executed action, if possible.

This function's exact configuration is stored in `treemacs-TAB-actions-config'."
  (interactive "P")
  (-when-let (state (treemacs--prop-at-point :state))
    (funcall (cdr (assq state treemacs-TAB-actions-config)) arg)
    (treemacs--evade-image)))

(defun treemacs-click-mouse1 (event)
  "Do the same as `treemacs-toggle-node' when mouse1 clicking on a line.
Must be bound to a mouse click, or EVENT will not be supplied."
  (interactive "e")
  (when (eq 'mouse-1 (elt event 0))
    (goto-char (posn-point (cadr event)))
    (beginning-of-line)
    (treemacs-toggle-node)))
(make-obsolete 'treemacs-click-mouse1 #'treemacs-leftclick-action "Treemacs v1.18")

(defun treemacs-uproot ()
  "Switch treemacs' root directory to current root's parent, if possible."
  (interactive)
  (let* ((root      (treemacs--current-root))
         (new-root  (treemacs--parent root)))
    (unless (s-equals? root new-root)
      (treemacs--reroot-up root new-root)
      (treemacs--build-tree new-root)
      (treemacs--goto-button-at root)
      (treemacs--evade-image))))

(defun treemacs-goto-parent-node ()
  "Select parent of selected node, if possible.
If there is no parent to go up to call `treemacs-uproot' instead."
  (interactive)
  (--if-let (-some-> (treemacs-current-button) (button-get :parent))
      (goto-char it)
    (treemacs-uproot)))

(defun treemacs-next-neighbour ()
  "Select next node at the same depth as currently selected node, if possible."
  (interactive)
  (-when-let- [next (treemacs--next-neighbour (treemacs-current-button))]
    (goto-char next)))

(defun treemacs-previous-neighbour ()
  "Select previous node at the same depth as currently selected node, if possible."
  (interactive)
  (-when-let- [prev (treemacs--prev-neighbour (treemacs-current-button))]
    (goto-char prev)))

(defun treemacs-change-root ()
  "Use currently selected directory as new root.
Do nothing for other node types."
  (interactive)
  (-if-let- [btn (treemacs-current-button)]
    (-pcase (button-get btn :state)
      [(or `dir-node-open `dir-node-closed)
       (-let [new-root (button-get btn :path)]
         (treemacs--reroot-down (treemacs--current-root) new-root)
         (treemacs--build-tree new-root))]
      [_
       (treemacs-log "Button in current line is not a directory.")])
    (treemacs-log "There is no directory to move into here.")))

(defun treemacs-visit-node-vertical-split (&optional arg)
  "Open current file or tag by vertically splitting `next-window'.
Stay in current window with a prefix argument ARG."
  (interactive "P")
  (treemacs--execute-button-action
   :split-function #'split-window-vertically
   :file-action (find-file (treemacs-safe-button-get btn :path))
   :dir-action (dired (treemacs-safe-button-get btn :path))
   :tag-action (treemacs--goto-tag btn)
   :save-window arg
   :no-match-explanation "Node is neither a file, a directory or a tag - nothing to do here."))

(defun treemacs-visit-node-horizontal-split (&optional arg)
  "Open current file or tag by horizontally splitting `next-window'.
Stay in current window with a prefix argument ARG."
  (interactive "P")
  (treemacs--execute-button-action
   :split-function #'split-window-horizontally
   :file-action (find-file (treemacs-safe-button-get btn :path))
   :dir-action (dired (treemacs-safe-button-get btn :path))
   :tag-action (treemacs--goto-tag btn)
   :save-window arg
   :no-match-explanation "Node is neither a file, a directory or a tag - nothing to do here."))

(defun treemacs-visit-node-no-split (&optional arg)
  "Open current file or tag within `next-window'.
Stay in current window with a prefix argument ARG."
  (interactive "P")
  (treemacs--execute-button-action
   :file-action (find-file (treemacs-safe-button-get btn :path))
   :dir-action (dired (treemacs-safe-button-get btn :path))
   :tag-action (treemacs--goto-tag btn)
   :save-window arg
   :ensure-window-split t
   :no-match-explanation "Node is neither a file, a directory or a tag - nothing to do here."))

(defun treemacs-visit-node-ace (&optional arg)
  "Open current file or tag in window selected by `ace-window'.
Stay in current window with a prefix argument ARG."
  (interactive "P")
  (treemacs--execute-button-action
   :window (aw-select "Select window")
   :file-action (find-file (treemacs-safe-button-get btn :path))
   :dir-action (dired (treemacs-safe-button-get btn :path))
   :tag-action (treemacs--goto-tag btn)
   :save-window arg
   :ensure-window-split t
   :no-match-explanation "Node is neither a file, a directory or a tag - nothing to do here."))

(defun treemacs-visit-node-ace-horizontal-split (&optional arg)
  "Open current file by horizontally splitting window selected by `ace-window'.
Stay in current window with a prefix argument ARG."
  (interactive "P")
  (treemacs--execute-button-action
   :split-function #'split-window-horizontally
   :window (aw-select "Select window")
   :file-action (find-file (treemacs-safe-button-get btn :path))
   :dir-action (dired (treemacs-safe-button-get btn :path))
   :tag-action (treemacs--goto-tag btn)
   :save-window arg
   :no-match-explanation "Node is neither a file, a directory or a tag - nothing to do here."))

(defun treemacs-visit-node-ace-vertical-split (&optional arg)
  "Open current file by vertically splitting window selected by `ace-window'.
Stay in current window with a prefix argument ARG."
  (interactive "P")
  (treemacs--execute-button-action
   :split-function #'split-window-vertically
   :window (aw-select "Select window")
   :file-action (find-file (treemacs-safe-button-get btn :path))
   :dir-action (dired (treemacs-safe-button-get btn :path))
   :tag-action (treemacs--goto-tag btn)
   :save-window arg
   :no-match-explanation "Node is neither a file, a directory or a tag - nothing to do here."))

(defun treemacs-RET-action (&optional arg)
  "Run the appropriate RET action for the current button.

In the default configuration this usually means to open the content of the
currently selected node. A potential prefix ARG is passed on to the executed
action, if possible.

This function's exact configuration is stored in `treemacs-RET-actions-config'."
  (interactive "P")
  (-when-let (state (treemacs--prop-at-point :state))
    (funcall (cdr (assq state treemacs-RET-actions-config)) arg)))

(defun treemacs-define-RET-action (state action)
  "Define the behaviour of `treemacs-RET-action'.
Determines that a button with a given STATE should lead to the execution of
ACTION.

First deletes the previous entry with key STATE from
`treemacs-RET-actions-config'and then inserts the new tuple."
  (setq treemacs-RET-actions-config (assq-delete-all state treemacs-RET-actions-config))
  (push (cons state action) treemacs-RET-actions-config))

(defun treemacs-define-TAB-action (state action)
  "Define the behaviour of `treemacs-TAB-action'.
Determines that a button with a given STATE should lead to the execution of
ACTION.

First deletes the previous entry with key STATE from
`treemacs-TAB-actions-config' and then inserts the new tuple."
  (setq treemacs-TAB-actions-config (assq-delete-all state treemacs-TAB-actions-config))
  (push (cons state action) treemacs-TAB-actions-config))

(defun treemacs-xdg-open ()
  "Open current file, using the `xdg-open' shell command."
  (interactive)
  (-when-let (path (treemacs--prop-at-point :path))
    (when (f-exists? path)
      (call-process-shell-command (format "xdg-open \"%s\" &" path)))))
(make-obsolete 'treemacs-xdg-open #'treemacs-visit-node-in-external-application "Treemacs v1.11.2")

(defun treemacs-visit-node-in-external-application ()
  "Open current file according to its mime type in an external application.
Treemacs knows how to open files on linux, windows and macos."
  (interactive)
  ;; code adapted from ranger.el
  (-if-let- [path (treemacs--prop-at-point :path)]
      (-pcase system-type
       [`windows-nt
        (declare-function w32-shell-execute "w32fns.c")
        (w32-shell-execute "open" (replace-regexp-in-string "/" "\\" path t t))]
       [`darwin
        (shell-command (format "open \"%s\"" path))]
       [`gnu/linux
        (let ((process-connection-type nil))
          (start-process "" nil "xdg-open" path))]
       [_ (treemacs-pulse-on-failure "Don't know how to open files on %s."
                         (propertize (symbol-name system-type) 'face 'font-lock-string-face))])
    (treemacs-pulse-on-failure "Nothing to open here.")))

(defun treemacs-kill-buffer ()
  "Kill the treemacs buffer."
  (interactive)
  (when (eq 'treemacs-mode major-mode)
    ;; teardown logic handled in kill hook
    (if (one-window-p)
        (kill-this-buffer)
      (kill-buffer-and-window))))

(defun treemacs-delete ()
  "Delete node at point.
A delete action must always be confirmed. Directories are deleted recursively."
  (interactive)
  (-if-let (btn (treemacs-current-button))
      (if (not (memq (button-get btn :state) '(file-node-open file-node-closed dir-node-open dir-node-closed)))
          (treemacs-log "Only files and directories can be deleted.")
        (let* ((path      (button-get btn :path))
               (file-name (f-filename path)))
          (when
              (cond
               ((f-file? path)
                (when (y-or-n-p (format "Delete %s ? " file-name))
                  (treemacs--without-filewatch (f-delete path))
                  t))
               ((f-directory? path)
                (when (y-or-n-p (format "Recursively delete %s ? " file-name))
                  (treemacs--without-filewatch (f-delete path t))
                  t))
               (t (progn
                    (treemacs-log "Item is neither a file, nor a directory - treemacs does not know how to delete it. (Maybe it no longer exists?)")
                    nil)))
            (treemacs--on-file-deletion path)
            (treemacs-without-messages (treemacs-refresh)))))
    (treemacs-pulse-on-failure "Nothing to delete here."))
  (treemacs--evade-image))

(defun treemacs-create-file ()
  "Create a new file.
Enter first the directory to create the new file in, then the new file's name.
The preselection for what directory to create in is based on the \"nearest\"
path to point - the containing directory for tags and files or the directory
itself, using the root directory when point is on the header line."
  (interactive)
  (treemacs--create-file/dir "File name: " #'f-touch))

(cl-defun treemacs-rename ()
  "Rename the currently selected node.
Buffers visiting the renamed file or visiting a file inside a renamed directory
and windows showing them will be reloaded. The list of recent files will
likewise be updated."
  (interactive)
  (cl-block nil
    (-let [btn (treemacs-current-button)]
      (unless btn
        (cl-return
         (treemacs-pulse-on-failure "Found nothing to rename here.")))
      (-let*- [(old-path (button-get btn :path))
               (new-path)
               (new-name)
               (dir)]
        (unless old-path
          (cl-return
           (treemacs-pulse-on-failure "Found nothing to rename here.")))
        (unless (file-exists-p old-path)
          (cl-return
           (treemacs-pulse-on-failure "The file to be renamed does not exist.")))
        (setq new-name (read-string "New name: ")
              dir      (f-dirname old-path)
              new-path (f-join dir new-name))
        (when (file-exists-p new-path)
          (cl-return
           (treemacs-pulse-on-failure "A file named %s already exists."
             (propertize new-name 'face font-lock-string-face))))
        (treemacs--without-filewatch (rename-file old-path new-path))
        (treemacs--replace-recentf-entry old-path new-path)
        (-let [treemacs-silent-refresh t]
          (treemacs-run-in-every-buffer
           (treemacs--on-rename old-path new-path)
           (when (treemacs--is-path-in-dir? new-path (treemacs--current-root))
             (treemacs--do-refresh (current-buffer)))))
        (treemacs--reload-buffers-after-rename old-path new-path)
        (treemacs--goto-button-at new-path)
        (treemacs-pulse-on-success)
        (cl-return
         (treemacs-pulse-on-success "Renamed %s to %s."
           (propertize (f-filename old-path) 'face font-lock-string-face)
           (propertize new-name 'face font-lock-string-face)))))))

(defun treemacs-create-dir ()
  "Create a new directory.
Enter first the directory to create the new dir in, then the new dir's name.
The preselection for what directory to create in is based on the \"nearest\"
path to point - the containing directory for tags and files or the directory
itself, using the root directory when point is on the header line."
  (interactive)
  (treemacs--create-file/dir "Directory name: " #'f-mkdir))

(defun treemacs-toggle-show-dotfiles ()
  "Toggle the hiding and displaying of dotfiles."
  (interactive)
  (setq treemacs-show-hidden-files (not treemacs-show-hidden-files))
  (-each (-map #'cdr treemacs--buffer-access) #'treemacs--do-refresh)
  (treemacs-log (concat "Dotfiles will now be "
                         (if treemacs-show-hidden-files
                             "displayed." "hidden."))))

(defun treemacs-toggle-fixed-width ()
  "Toggle whether the treemacs buffer should have a fixed width.
See also `treemacs-width.'"
  (interactive)
  (setq treemacs--width-is-locked (not treemacs--width-is-locked))
  (treemacs-log "Window width has been %s."
                 (propertize (if treemacs--width-is-locked "locked" "unlocked")
                             'face 'font-lock-string-face)))

(defun treemacs-reset-width (&optional arg)
  "Reset the width of the treemacs window to `treemacs-buffer-width'.
If a prefix argument ARG is provided read a new value for
`treemacs-buffer-width' first."
  (interactive "P")
  (let ((window-size-fixed nil))
    (when arg
      (setq treemacs-width
            (->> treemacs-width
                 (format "New Width (current = %s): ")
                 (read-number))))
    (treemacs--set-width treemacs-width)))

(defun treemacs-yank-path-at-point ()
  "Copy the absolute path of the node at point."
  (interactive)
  (--if-let (-some-> (treemacs--prop-at-point :path) (f-full) (kill-new))
        (treemacs-pulse-on-success "Yanked path: %s" (propertize it 'face 'font-lock-string-face))
    (treemacs-pulse-on-failure  "There is nothing to copy here")))

(defun treemacs-yank-root ()
  "Copy the absolute path of the current treemacs root."
  (interactive)
  (--if-let (-> default-directory (f-full) (kill-new))
      (treemacs-log "Yanked root: %s" (propertize it 'face 'font-lock-string-face))
    (treemacs-pulse-on-failure "Failed to yank root")))

(defun treemacs-delete-other-windows ()
  "Same as `delete-other-windows', but will not delete the treemacs window."
  (interactive)
  (let ((w (selected-window)))
    (--each (window-list (selected-frame))
      (unless (or (eq it w)
                  (->> it (window-buffer) (buffer-name) (s-starts-with? treemacs--buffer-name-prefix)))
        (delete-window it)))))

(defun treemacs-push-button-select-sort (&optional arg)
  "Same as `treemacs-toggle-node', but the sorting function is chosen manually.
The sort setting is active for only a single push, its effect will be undone on
the next refresh.
Prefix argument ARG has the same effect as in `treemacs-toggle-node' - causing
the open/close process to work recursively."
  (interactive)
  (let* ((sort-options '(alphabetic-desc alphabetic-asc size-asc size-desc mod-time-asc mod-time-desc))
         (treemacs-sorting (intern (completing-read "Sorting: " sort-options))))
    (treemacs-toggle-node arg)))
(make-obsolete 'treemacs-push-button-select-sort
               #'treemacs-resort
               "Treemacs v1.12")

(defun treemacs-temp-resort-root (&optional sort-method)
  "Temporarily resort the entire treemacs buffer.
SORT-METHOD is a cons of a string describing the method and the actual sort
value, as returned by `treemacs--sort-value-selection'. SORT-METHOD will be
provided when this function is called from `treemacs-resort' and will be
interactively read otherwise. This way this function can be bound directly,
without the need to call `treemacs-resort' with a prefix arg."
  (interactive)
  (-let* (((sort-name . sort-method) (or sort-method (treemacs--sort-value-selection)))
          (treemacs-sorting sort-method))
    (treemacs-without-messages (treemacs-refresh))
    (treemacs-log "Temporarily resorted everything with sort method '%s.'"
                   (propertize sort-name 'face 'font-lock-type-face))))

(defun treemacs-temp-resort-current-dir (&optional sort-method)
  "Temporarily resort the current directory.
SORT-METHOD is a cons of a string describing the method and the actual sort
value, as returned by `treemacs--sort-value-selection'. SORT-METHOD will be
provided when this function is called from `treemacs-resort' and will be
interactively read otherwise. This way this function can be bound directly,
without the need to call `treemacs-resort' with a prefix arg."
  (interactive)
  (-let* (((sort-name . sort-method) (or sort-method (treemacs--sort-value-selection)))
          (treemacs-sorting sort-method))
    (-if-let (btn (treemacs-current-button))
             (-pcase (button-get btn :state)
               [`dir-node-closed
                (treemacs--expand-dir-node btn)
                (treemacs-log "Resorted %s with sort method '%s'."
                               (propertize (treemacs--get-label-of btn) 'face 'font-lock-string-face)
                               (propertize sort-name 'face 'font-lock-type-face))]
               [`dir-node-open
                (treemacs--collapse-dir-node btn)
                (goto-char (button-start btn))
                (treemacs--expand-dir-node btn)
                (treemacs-log "Resorted %s with sort method '%s'."
                               (propertize (treemacs--get-label-of btn) 'face 'font-lock-string-face)
                               (propertize sort-name 'face 'font-lock-type-face))]
               [(or `file-node-open `file-node-closed `tag-node-open `tag-node-closed `tag-node)
                (let* ((parent (button-get btn :parent)))
                  (while (and parent
                              (not (-some-> parent (button-get :path) (f-directory?))))
                    (setq parent (button-get parent :parent)))
                  (if parent
                      (let ((line (line-number-at-pos))
                            (window-point (window-point)))
                        (goto-char (button-start parent))
                        (treemacs--collapse-dir-node parent)
                        (goto-char (button-start btn))
                        (treemacs--expand-dir-node parent)
                        (set-window-point (selected-window) window-point)
                        (with-no-warnings (goto-line line))
                        (treemacs-log "Resorted %s with sort method '%s'."
                                       (propertize (treemacs--get-label-of parent) 'face 'font-lock-string-face)
                                       (propertize sort-name 'face 'font-lock-type-face)))
                    ;; a top level file's containing dir is root
                    (treemacs-without-messages (treemacs-refresh))
                    (treemacs-log "Resorted root directory with sort method '%s'."
                                   (propertize sort-name 'face 'font-lock-type-face))))]))))

(defun treemacs-resort (&optional arg)
  "Select a new permanent value for `treemacs-sorting' and refresh.
With a single prefix ARG use the new sort value to *temporarily* resort the
\(closest\) directory at point.
With a double prefix ARG use the new sort value to *temporarily* resort the
entire treemacs view.

Temporary sorting will only stick around until the next refresh, either manual
or automatic via `treemacs-filewatch-mode'.

Instead of calling this with a prefix arg you can also direcrly call
`treemacs-temp-resort-current-dir' and `treemacs-temp-resort-root'."
  (interactive "P")
  (-pcase arg
    ;; Resort current dir only
    [`(4)
     (treemacs-temp-resort-current-dir)]
    ;; Temporarily resort everything
    [`(16)
     (treemacs-temp-resort-root)]
    ;; Set new permanent value
    [_
     (-let (((sort-name . sort-value) (treemacs--sort-value-selection)))
       (setq treemacs-sorting sort-value)
       (treemacs-without-messages (treemacs-refresh))
       (treemacs-log "Sorting method changed to '%s'."
                      (propertize sort-name 'face 'font-lock-type-face)))])
  (treemacs--evade-image))

(defun treemacs-add-bookmark ()
  "Add the current node to Emacs' list of bookmarks.
For file and directory nodes their absolute path is saved. Tag nodes
additionally also save the tag's position. A tag can only be bookmarked if the
treemacs node is pointing to a valid buffer position."
  (interactive)
  (treemacs--with-current-button
   "There is nothing to bookmark here."
   (-pcase (button-get current-btn :state)
     [(or `file-node-open `file-node-closed `dir-node-open `dir-node-closed)
      (-let [name (read-string "Bookmark name: ")]
        (bookmark-store name `((filename . ,(button-get current-btn :path))) nil))]
     [`tag-node
      (-let [(tag-buffer . tag-pos) (treemacs--pos-from-marker (button-get current-btn :marker))]
        (if (buffer-live-p tag-buffer)
            (bookmark-store
             (read-string "Bookmark name: ")
             `((filename . ,(buffer-file-name tag-buffer))
               (position . ,tag-pos))
             nil)
          (treemacs-log "Tag info can not be saved because it is not pointing to a live buffer.")))]
     [(or `tag-node-open `tag-node-closed)
      (treemacs-pulse-on-failure "There is nothing to bookmark here.")])))

(defun treemacs-next-line-other-window (&optional count)
  "Scroll forward COUNT lines in `next-window'."
  (interactive "p")
  (with-selected-window (next-window)
    (scroll-up-line count)))

(defun treemacs-previous-line-other-window (&optional count)
  "Scroll backward COUNT lines in `next-window'."
  (interactive "p")
  (with-selected-window (other-window-for-scrolling)
    (scroll-down-line count)))

(provide 'treemacs-interface)

;;; treemacs-interface.el ends here
