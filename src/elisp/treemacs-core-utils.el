;;; treemacs.el --- A tree style file viewer package -*- lexical-binding: t -*-

;; Copyright (C) 2019 Alexander Miller

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
;;; General implementation details.

;;; Code:

(require 'hl-line)
(require 'dash)
(require 's)
(require 'ht)
(require 'f)
(require 'ace-window)
(require 'vc-hooks)
(require 'pfuture)
(require 'treemacs-customization)
(eval-and-compile
  (require 'inline)
  (require 'cl-lib)
  (require 'treemacs-macros))

(treemacs-import-functions-from "treemacs-tags"
  treemacs--expand-file-node
  treemacs--collapse-file-node
  treemacs--expand-tag-node
  treemacs--collapse-tag-node
  treemacs--extract-position
  treemacs--goto-tag)

(treemacs-import-functions-from "treemacs"
  treemacs-refresh)

(treemacs-import-functions-from "treemacs-rendering"
  treemacs-do-delete-single-node
  treemacs-do-update-node
  treemacs-do-delete-single-node
  treemacs--current-screen-line
  treemacs--add-root-element
  treemacs--expand-root-node
  treemacs--collapse-root-node
  treemacs--expand-dir-node
  treemacs--collapse-dir-node
  treemacs--render-projects)

(treemacs-import-functions-from "treemacs-filewatch-mode"
  treemacs--stop-filewatch-for-current-buffer
  treemacs--stop-watching
  treemacs--cancel-refresh-timer)

(treemacs-import-functions-from "treemacs-follow-mode"
  treemacs--follow)

(treemacs-import-functions-from "treemacs-visuals"
  treemacs-pulse-on-success
  treemacs--tear-down-icon-highlight
  treemacs--forget-previously-follow-tag-btn
  treemacs--forget-last-highlight)

(treemacs-import-functions-from "treemacs-async"
  treemacs--git-status-process
  treemacs--collapsed-dirs-process)

(treemacs-import-functions-from "treemacs-dom"
  treemacs-on-collapse
  treemacs-dom-node->set-position!
  treemacs-find-in-dom
  treemacs-dom-node->key
  treemacs-dom-node->position)

(treemacs-import-functions-from "treemacs-workspaces"
  treemacs--find-workspace
  treemacs-current-workspace
  treemacs-workspace->projects
  treemacs-workspace->is-empty?
  treemacs-do-add-project-to-workspace
  treemacs-project->is-expanded?
  treemacs-project->path
  treemacs-project->name
  treemacs-project->refresh!
  treemacs-project->position
  treemacs-project-p
  treemacs--find-project-for-path)

(treemacs-import-functions-from "treemacs-visuals"
  treemacs-pulse-on-failure)

(treemacs-import-functions-from "treemacs-persistence"
  treemacs--maybe-load-workspaces)

(declare-function treemacs-mode "treemacs-mode")

(defvar treemacs--closed-node-states
  '(root-node-closed
    dir-node-closed
    file-node-closed
    tag-node-closed)
  "States marking a node as closed.
Used in `treemacs-is-node-collapsed?'")

(defvar treemacs--open-node-states
  '(root-node-open
    dir-node-open
    file-node-open
    tag-node-open)
  "States marking a node as open.
Used in `treemacs-is-node-expanded?'")

(defvar treemacs--buffer-access nil
  "Alist mapping treemacs buffers to frames.")

(defvar treemacs--scope-id 0
  "Used as a frame parameter to identify a frame over multiple sessions.
Used to restore the frame -> buffer mapping in `treemacs--buffer-access' with
desktop save mode.")

(defvar treemacs--taken-scopes nil
  "List of already taken scope ids that can no longer be used.
Especially important after a session restore, since the list of used ids may no
longer be contigious.")

(defconst treemacs--buffer-name-prefix " *Treemacs-")

(defconst treemacs-dir
  ;; locally we're in src/elisp, installed from melpa we're at the package root
  (-let [dir (-> (if load-file-name
                     (file-name-directory load-file-name)
                   default-directory)
                 (expand-file-name))]
    (if (s-ends-with? "src/elisp/" dir)
        (-> dir (f-parent) (f-parent))
      dir))
  "The directory treemacs.el is stored in.")

(defvar treemacs--no-messages nil
  "When set to t `treemacs-log' will produce no output.
Not used directly, but as part of `treemacs-without-messages'.")

(defvar-local treemacs--width-is-locked t
  "Keeps track of whether the width of the treemacs window is locked.")

(defvar-local treemacs--in-this-buffer nil
  "Non-nil only in buffers meant to show treemacs.
Used to show an error message if someone mistakenly actives `treemacs-mode'.")

(defvar treemacs--pre-peek-state nil
  "List of window, buffer to restore and buffer to kill treemacs used for peeking.")

(define-inline treemacs--parent-dir (path)
  "Return the parent of PATH is it's a file, or PATH if it is a directory.

PATH: File Path"
  (declare (side-effect-free t) (pure t))
  (inline-letevals (path)
    (inline-quote
     (-> ,path
         (file-name-directory)
         (treemacs--unslash)))))

(define-inline treemacs--remove-trailing-newline (str)
  "Remove final newline in STR."
  (declare (pure t) (side-effect-free t))
  (inline-letevals (str)
    (inline-quote
     (let ((len (1- (length ,str))))
       (if (= 10 (aref ,str len))
           (substring ,str 0 len)
         ,str)))))

(define-inline treemacs--add-trailing-slash (str)
  "Add final slash to STR.
If STR already has a slash return it unchanged."
  (declare (pure t) (side-effect-free t))
  (inline-letevals (str)
    (inline-quote
     (if (eq ?/ (aref ,str (1- (length ,str))))
         ,str
       (concat ,str "/")))))

(define-inline treemacs--delete-line ()
  "Delete the current line.
Unlike `kill-whole-line' this won't pollute the kill ring."
  (inline-quote
     (delete-region (point-at-bol) (min (point-max) (1+ (point-at-eol))))))

(define-inline treemacs-current-button ()
  "Get the button in the current line.
Returns nil when point is between projects."
  (declare (side-effect-free error-free))
  (inline-quote
   (-some->
    (text-property-not-all (point-at-bol) (point-at-eol) 'button nil)
    (copy-marker t))))
(defalias 'treemacs-node-at-point #'treemacs-current-button)

(define-inline treemacs-button-put (button prop val)
  "Set BUTTON's PROP property to VAL.
Same as `button-put', but faster since it's inlined and does not query the
button type on every call."
  (inline-letevals (button prop val)
    (inline-quote
     (put-text-property
      (or (previous-single-property-change (1+ ,button) 'button)
          (point-min))
      (or (next-single-property-change ,button 'button)
          (point-max))
      ,prop ,val))))

(define-inline treemacs-button-get (button prop)
  "Get the property of button BUTTON named PROP
Same as `button-get', but faster since it's inlined and does not query the
button type on every call."
  (declare (side-effect-free t))
  (inline-letevals (button prop)
    (inline-quote
     (get-text-property ,button ,prop))))

(define-inline treemacs-button-start (button)
  "Return the start position of BUTTON.
Same as `button-start', but faster since it's inlined and does not query the
button type on every call."
  (declare (side-effect-free t))
  (inline-letevals (button)
    (inline-quote
     (or (previous-single-property-change (1+ ,button) 'button)
         (point-min)))))

(define-inline treemacs-button-end (button)
  "Return the end position of BUTTON.
Same as `button-end', but faster since it's inlined and does not query the
button type on every call."
  (declare (side-effect-free t))
  (inline-letevals (button)
    (inline-quote
     (or (next-single-property-change ,button 'button)
         (point-max)))))

(define-inline treemacs-is-node-expanded? (btn)
  "Return whether BTN is in an open state."
  (declare (side-effect-free t))
  (inline-quote
   (memq (treemacs-button-get ,btn :state) treemacs--open-node-states)))

(define-inline treemacs-is-node-collapsed? (btn)
  "Return whether BTN is in a closed state."
  (declare (side-effect-free t))
  (inline-quote
   (memq (treemacs-button-get ,btn :state) treemacs--closed-node-states)))

(define-inline treemacs--unslash (path)
  "Remove the final slash in PATH."
  (declare (pure t) (side-effect-free t))
  (inline-letevals (path)
    (inline-quote
     (if (and (> (length ,path) 1)
              (eq ?/ (aref ,path (1- (length ,path)))))
         (substring ,path 0 -1)
       ,path))))

(define-inline treemacs--get-label-of (btn)
  "Return the text label of BTN."
  (declare (side-effect-free t))
  (inline-quote
   (buffer-substring-no-properties (treemacs-button-start ,btn) (treemacs-button-end ,btn))))

(defun treemacs--replace-recentf-entry (old-file new-file)
  "Replace OLD-FILE with NEW-FILE in the recent file list."
  ;; code taken from spacemacs - is-bound check due to being introduced after emacs24?
  ;; better safe than sorry so let's keep it
  (with-no-warnings
    (when (fboundp 'recentf-add-file)
      (recentf-add-file new-file)
      (recentf-remove-if-non-kept old-file))))

(defun treemacs--select-project-by-name ()
  "Interactively choose a project from the current workspace."
  (let* ((projects (--map (cons (treemacs-project->name it) it)
                          (-> (treemacs-current-workspace) (treemacs-workspace->projects))))
         (selection (completing-read "Project: " projects)))
    (cdr (assoc selection projects))))

(defun treemacs-get-local-window ()
  "Return the window displaying the treemacs buffer in the current frame.
Returns nil if no treemacs buffer is visible."
  (declare (side-effect-free error-free))
  (->> (window-list (selected-frame))
       (--first (->> it
                     (window-buffer)
                     (buffer-name)
                     (s-starts-with? treemacs--buffer-name-prefix)))))

(defun treemacs-get-local-buffer ()
  "Return the treemacs buffer local to the current frame.
Returns nil if no such buffer exists.."
  (declare (side-effect-free t))
  (-let [b (->> treemacs--buffer-access
                (assq (selected-frame))
                (cdr))]
    (when (buffer-live-p b) b)))

(defun treemacs--select-visible-window ()
  "Switch to treemacs buffer, given that it is currently visible."
  (->> treemacs--buffer-access
       (assoc (selected-frame))
       (cdr)
       (get-buffer-window)
       (select-window))
  (run-hooks 'treemacs-select-hook))

(define-inline treemacs--select-not-visible-window ()
  "Switch to treemacs buffer, given that it not visible."
  (inline-quote
   (let ((buffer (current-buffer)))
     (treemacs--setup-buffer)
     (when (or treemacs-follow-after-init
               (with-no-warnings treemacs-follow-mode))
       (with-current-buffer buffer (treemacs--follow)))
     (run-hooks 'treemacs-select-hook))))

(define-inline treemacs--button-symbol-switch (new-sym)
  "Replace icon in current line with NEW-SYM."
  (inline-letevals (new-sym)
    (inline-quote
     (save-excursion
       (let ((len (length ,new-sym)))
         (goto-char (- (treemacs-button-start (next-button (point-at-bol) t)) len))
         (insert ,new-sym)
         (delete-char len))))))

(defun treemacs-project-of-node (node)
  "Find the project the given NODE belongs to."
  (declare (side-effect-free t))
  (-let [project (treemacs-button-get node :project)]
    (while (not project)
      (setq node (treemacs-button-get node :parent)
            project (treemacs-button-get node :project)))
    project))

(define-inline treemacs--prop-at-point (prop)
  "Grab property PROP of the button at point.
Returns nil when there is no button at point."
  (declare (side-effect-free t))
  (inline-quote
   (-when-let (b (treemacs-current-button))
     (treemacs-button-get b ,prop))))

(define-inline treemacs--filename (file)
  "Return the name of FILE, same as `f-filename', but inlined."
  (declare (pure t) (side-effect-free t))
  (inline-quote (file-name-nondirectory (directory-file-name ,file))))

(define-inline treemacs--reject-ignored-files (file)
  "Return t if FILE is *not* an ignored file.
FILE here is a list consisting of an absolute path and file attributes."
  (declare (side-effect-free t))
  (inline-letevals (file)
    (inline-quote
     (let ((filename (treemacs--filename ,file)))
       (--none? (funcall it filename ,file) treemacs-ignored-file-predicates)))))

(define-inline treemacs--reject-ignored-and-dotfiles (file)
  "Return t when FILE is neither ignored, nor a dotfile.
FILE here is a list consisting of an absolute path and file attributes."
  (declare (side-effect-free t))
  (inline-letevals (file)
    (inline-quote
     (let ((filename (treemacs--filename ,file)))
       (and (not (s-matches? treemacs-dotfiles-regex filename))
            (--none? (funcall it filename ,file) treemacs-ignored-file-predicates))))))

(define-inline treemacs--file-extension (file)
  "Same as `file-name-extension', but also works with leading periods.

This is something a of workaround to easily allow assigning icons to a FILE with
a name like '.gitignore' without always having to check for both file extensions
and special names like this."
  (declare (pure t) (side-effect-free t))
  (inline-quote
   (let ((filename (treemacs--filename ,file)))
     (save-match-data
       (if (string-match "\\.[^.]*\\'" filename)
           (substring filename (1+ (match-beginning 0)))
         filename)))))

(define-inline treemacs-is-treemacs-window? (window)
  "Return t when WINDOW is showing a treemacs buffer."
  (declare (side-effect-free t))
  (inline-quote
   (->> ,window (window-buffer) (buffer-name) (s-starts-with? treemacs--buffer-name-prefix))))

(define-inline treemacs--get-framelocal-buffer ()
  "Get this frame's local buffer, creating it if necessary.
Will also perform cleanup if the buffer is dead."
  (inline-quote
   (let* ((frame (selected-frame))
          (buf   (assq frame treemacs--buffer-access)))
     (when (or (null buf)
               (not (buffer-live-p buf)))
       (setq treemacs--buffer-access
             (assq-delete-all frame treemacs--buffer-access))
       (unless (frame-parameter frame 'treemacs-id)
         (while (memq (setq treemacs--scope-id (1+ treemacs--scope-id)) treemacs--taken-scopes))
         (push treemacs--scope-id treemacs--taken-scopes)
         (set-frame-parameter frame 'treemacs-id (number-to-string treemacs--scope-id)))
       (setq buf (get-buffer-create (format "%sFramebuffer-%s*" treemacs--buffer-name-prefix (frame-parameter frame 'treemacs-id))))
       (push (cons frame buf) treemacs--buffer-access))
     buf)))

(define-inline treemacs--next-neighbour-of (btn)
  "Get the next same-level neighbour of BTN, if any."
  (declare (side-effect-free t))
  (inline-letevals (btn)
    (inline-quote
     (-let ((depth (treemacs-button-get ,btn :depth))
            (next (next-button (treemacs-button-end ,btn))))
       (while (and next (< depth (treemacs-button-get next :depth)))
         (setq next (next-button (treemacs-button-end next))))
       (when (and next (= depth (treemacs-button-get next :depth))) next)))))

(define-inline treemacs--prev-non-child-button (btn)
  "Get the previous same-level neighbour of BTN, if any."
  (declare (side-effect-free t))
  (inline-letevals (btn)
    (inline-quote
     (let ((depth (treemacs-button-get ,btn :depth))
           (prev (previous-button (treemacs-button-start ,btn))))
       (while (and prev (< depth (treemacs-button-get prev :depth)))
         (setq prev (previous-button (treemacs-button-start prev))))
       (when (and prev (= depth (treemacs-button-get prev :depth))) prev)))))

(define-inline treemacs--next-non-child-button (btn)
  "Return the next button after BTN that is not a child of BTN."
  (declare (side-effect-free t))
  (inline-letevals (btn)
    (inline-quote
     (when ,btn
       (let ((depth (treemacs-button-get ,btn :depth))
             (next (next-button (treemacs-button-end ,btn) t)))
         (while (and next (< depth (treemacs-button-get next :depth)))
           (setq next (next-button (treemacs-button-end next) t)))
         next)))))

(define-inline treemacs--remove-framelocal-buffer ()
  "Remove the frame-local buffer from the current frame.
To be run in the kill buffer hook as it removes the mapping
of the `current-buffer'."
  (inline-quote
   (setq treemacs--buffer-access
         (rassq-delete-all (current-buffer) treemacs--buffer-access))))

(define-inline treemacs--on-file-deletion (path &optional no-buffer-delete)
  "Cleanup to run when treemacs file at PATH was deleted.
Do not try to delete buffers for PATH when NO-BUFFER-DELETE is non-nil. This is
necessary since interacting with magit can cause file delete events for files
being edited to trigger."
  (inline-letevals (path no-buffer-delete)
    (inline-quote
     (progn
       (unless ,no-buffer-delete (treemacs--kill-buffers-after-deletion ,path t))
       (treemacs--stop-watching ,path t)
       ;; filewatch mode needs the node's information to be in the dom
       (unless (with-no-warnings treemacs-filewatch-mode)
         (treemacs-run-in-every-buffer
          (treemacs-on-collapse ,path t)))))))

(define-inline treemacs--refresh-dir (path &optional project)
  "Local refresh for button at PATH and PROJECT.
Simply collapses and re-expands the button (if it has not been closed)."
  (inline-letevals (path project)
    (inline-quote
     (let ((btn (treemacs-goto-file-node ,path ,project)))
       (when (memq (treemacs-button-get btn :state) '(dir-node-open file-node-open root-node-open))
         (goto-char (treemacs-button-start btn))
         (treemacs--push-button btn)
         (goto-char (treemacs-button-start btn))
         (treemacs--push-button btn))))))

(define-inline treemacs--canonical-path (path)
  "The canonical version of PATH for being handled by treemacs.
In practice this means expand PATH and remove its final slash."
  (declare (pure t) (side-effect-free t))
  (inline-letevals (path)
    (inline-quote
     (let (file-name-handler-alist)
       (-> ,path (expand-file-name) (treemacs--unslash))))))

(define-inline treemacs-is-file-git-ignored? (file git-info)
  "Determined if FILE is ignored by git by means of GIT-INFO."
  (declare (side-effect-free t))
  (inline-letevals (file git-info)
    (inline-quote (string= "!" (ht-get ,git-info ,file)))))

(define-inline treemacs-is-treemacs-window-selected? ()
  "Return t when the treemacs window is selected."
  (declare (side-effect-free t))
  (inline-quote (s-starts-with? treemacs--buffer-name-prefix (buffer-name))))

(defun treemacs--reload-buffers-after-rename (old-path new-path)
  "Reload buffers and windows after OLD-PATH was renamed to NEW-PATH."
  ;; first buffers shown in windows
  (dolist (frame (frame-list))
    (dolist (window (window-list frame))
      (let* ((win-buff  (window-buffer window))
             (buff-file (buffer-file-name win-buff)))
        (when buff-file
          (setq buff-file (f-long buff-file))
          (when (treemacs-is-path buff-file :in old-path)
            (treemacs-without-following
             (with-selected-window window
               (kill-buffer win-buff)
               (let ((new-file (s-replace old-path new-path buff-file)))
                 (find-file-existing new-file)
                 (treemacs--replace-recentf-entry buff-file new-file)))))))))
  ;; then the rest
  (--each (buffer-list)
    (-when-let (buff-file (buffer-file-name it))
      (setq buff-file (f-long buff-file))
      (when (treemacs-is-path buff-file :in old-path)
        (let ((new-file (s-replace old-path new-path buff-file)))
          (kill-buffer it)
          (find-file-noselect new-file)
          (treemacs--replace-recentf-entry buff-file new-file))))))

(defun treemacs-collect-child-nodes (parent-btn)
  "Get all buttons exactly one level deeper than PARENT-BTN.
The child buttons are returned in the same order as they appear in the treemacs
buffer."
  (let (ret)
    (treemacs-first-child-node-where parent-btn
      (push child-btn ret)
      nil)
    (nreverse ret)))
(defalias 'treemacs--get-children-of #'treemacs-collect-child-nodes)
(with-no-warnings
  (make-obsolete #'treemacs--get-children-of #'treemacs-collect-child-nodes "v2.7"))

(defun treemacs--init (&optional root name)
  "Initialize a treemacs buffer from the current workspace.
Add a project for ROOT and NAME if they are non-nil."
  (treemacs--maybe-load-workspaces)
  (let ((origin-buffer (current-buffer))
        (current-workspace (treemacs-current-workspace))
        (run-hook? nil))
    (pcase (treemacs-current-visibility)
      ('visible (treemacs--select-visible-window))
      ('exists (treemacs--select-not-visible-window))
      ('none
       (treemacs--setup-buffer)
       (treemacs-mode)
       (unless current-workspace
         (treemacs--find-workspace (buffer-file-name origin-buffer))
         (setq current-workspace (treemacs-current-workspace))
         (run-hook-with-args treemacs-workspace-first-found-functions
                             current-workspace (selected-frame)))
       ;; Render the projects even if there are none. This ensures that top-level
       ;; extensions are always rendered, and the project markers are initialized.
       (treemacs--render-projects (treemacs-workspace->projects current-workspace))
       (when (treemacs-workspace->is-empty?)
         (-> (treemacs--read-first-project-path)
             (treemacs--canonical-path)
             (treemacs-do-add-project-to-workspace))
         (treemacs-log "Created first project."))
       (goto-char 2)
       (setf run-hook? t)))
    (when root (treemacs-do-add-project-to-workspace (treemacs--canonical-path root) name))
    (with-no-warnings (setq treemacs--ready-to-follow t))
    (when (or treemacs-follow-after-init (with-no-warnings treemacs-follow-mode))
      (with-current-buffer origin-buffer
        (treemacs--follow)))
    ;; The hook should run at the end of the setup, but also only
    ;; if a new buffer was created, as the other cases are already covered
    ;; in their respective setup functions.
    (when run-hook? (run-hooks 'treemacs-select-hook))))

(defun treemacs--on-buffer-kill ()
  "Cleanup to run when a treemacs buffer is killed."
  ;; stop watch must come first since we need a reference to the killed buffer
  ;; to remove it from the filewatch list
  (treemacs--stop-filewatch-for-current-buffer)
  (treemacs--remove-framelocal-buffer)
  (treemacs--tear-down-icon-highlight)
  (unless treemacs--buffer-access
    ;; TODO make local maybe
    (remove-hook 'window-configuration-change-hook #'treemacs--on-window-config-change)))

(defun treemacs--push-button (btn &optional recursive)
  "Execute the appropriate action given the state of the pushed BTN.
Optionally do so in a RECURSIVE fashion."
  (pcase (treemacs-button-get btn :state)
    ('root-node-closed (treemacs--expand-root-node btn))
    ('dir-node-open    (treemacs--collapse-dir-node btn recursive))
    ('dir-node-closed  (treemacs--expand-dir-node btn :recursive recursive))
    ('file-node-open   (treemacs--collapse-file-node btn recursive))
    ('file-node-closed (treemacs--expand-file-node btn recursive))
    ('tag-node-open    (treemacs--collapse-tag-node btn recursive))
    ('tag-node-closed  (treemacs--expand-tag-node btn recursive))
    ('tag-node         (progn (other-window 1) (treemacs--goto-tag btn)))
    (_                 (error "[Treemacs] Cannot push button with unknown state '%s'" (treemacs-button-get btn :state)))))

(defun treemacs--nearest-path (btn)
  "Return the file path of the BTN.
If the `:path' property is not set or not a file, keep looking upward, via the
`:parent' property.  Useful to e.g. find the path of the file of the currently
selected tags or extension entry.  Must be called from treemacs buffer."
  (let ((path (treemacs-button-get btn :path)))
    (if (stringp path)
        path
      (-some-> (treemacs-button-get btn :parent)
               (treemacs--nearest-path)))))

(defun treemacs--create-file/dir (is-file?)
  "Interactively create either a file or directory, depending on IS-FILE.

IS-FILE?: Bool"
  (interactive)
  (let* ((curr-path (--if-let (treemacs-current-button)
                        (treemacs--nearest-path it)
                      (f-expand "~")))
         (path-to-create (read-file-name
                          (if is-file? "Create File: " "Create Directory: ")
                          (treemacs--add-trailing-slash
                           (if (f-dir? curr-path)
                               curr-path
                             (f-dirname curr-path))))))
    (treemacs-block
     (treemacs-error-return-if (file-exists-p path-to-create)
       "%s already exists." (propertize path-to-create 'face 'font-lock-string-face))
     (treemacs--without-filewatch
      (if is-file?
          (-let [dir (f-dirname path-to-create)]
            (unless (f-exists? dir)
              (make-directory dir t))
            (f-touch path-to-create))
        (make-directory path-to-create t)))
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
       (treemacs-goto-file-node (treemacs--canonical-path path-to-create) project)
       (recenter))
     (treemacs-pulse-on-success
         "Created %s." (propertize path-to-create 'face 'font-lock-string-face)))))

(define-inline treemacs--follow-path-elements (btn items)
  "Starting at BTN follow (goto and open) every single element in ITEMS.
Return the button that is found or the symbol `follow-failed' if the search
failed."
  (inline-letevals (btn items)
    (inline-quote
     (cl-block search
       (when (treemacs-is-node-collapsed? ,btn)
         (goto-char ,btn)
         (funcall (cdr (assq (treemacs-button-get ,btn :state) treemacs-TAB-actions-config))))
       (while ,items
         (let ((item (pop ,items)))
           (setq ,btn (treemacs-first-child-node-where ,btn
                       (equal (treemacs-button-get child-btn :key) item)))
           (unless ,btn
             (cl-return-from search
               'follow-failed))
           (goto-char ,btn)
           (when (and ,items (treemacs-is-node-collapsed? ,btn))
             (funcall (cdr (assq (treemacs-button-get ,btn :state) treemacs-TAB-actions-config))))))
       ,btn))))

(define-inline treemacs--follow-each-dir (btn dir-parts project)
  "Starting at BTN follow (goto and open) every single dir in DIR-PARTS.
Return the button that is found or the symbol `follow-failed' if the search
failed.  PROJECT is used for determining whether Git actions are appropriate."
  (inline-letevals (btn dir-parts project)
    (inline-quote
     (let* ((root       (treemacs-button-get ,btn :path))
            (git-future (treemacs--git-status-process root ,project))
            (last-index (- (length ,dir-parts) 1))
            (depth      (treemacs-button-get ,btn :depth)))
       (goto-char ,btn)
       ;; point is currently on the next closest dir to the followed file we could get
       ;; from the dom, so we expand it to keep going
       (pcase (treemacs-button-get ,btn :state)
         ('dir-node-closed (treemacs--expand-dir-node ,btn :git-future git-future))
         ('root-node-closed (treemacs--expand-root-node ,btn)))
       (catch 'follow-failed
         (let ((index 0)
               (dir-part nil))
           ;; for every item in dir-parts append it to the already found path for a new
           ;; 'root' to follow, so for root = /x/ and dir-parts = [src, config, foo.el]
           ;; consecutively try to move to /x/src, /x/src/confg and finally /x/src/config/foo.el
           (while ,dir-parts
             (setq dir-part (pop ,dir-parts)
                   root (f-join root dir-part)
                   ,btn
                   (let (current-btn)
                     (cl-block search
                       ;; first a plain text-based search for the current dir-part string
                       ;; then we grab the node we landed at and see what's going on
                       ;; there's a couple ways this can go
                       (while (progn (goto-char (point-at-eol)) (search-forward dir-part nil :no-error))
                         (setq current-btn (treemacs-current-button))
                         (cond
                          ;; somehow we landed on a line where there isn't even anything to look at
                          ;; technically this should never happen, but better safe than sorry
                          ((null current-btn)
                           (cl-return-from search))
                          ;; the search matched a custom button - skip those, as they cannot match
                          ;; and their :paths are not strings, which would cause the following checks
                          ;; to crash
                          ((treemacs-button-get current-btn :custom))
                          ;; perfect match - return the node we're at
                          ((treemacs-is-path root :same-as (treemacs-button-get current-btn :path))
                           (cl-return-from search current-btn))
                          ;; perfect match - taking collapsed dirs into account
                          ;; return the node, but make sure to advance the loop variables an
                          ;; appropriate nuber of times, since a collapsed directory is basically
                          ;; multiple search iterations bundled as one
                          ((and (treemacs-button-get current-btn :collapsed)
                                (treemacs-is-path (treemacs-button-get current-btn :path) :parent-of root))
                           (dotimes (_ (car (treemacs-button-get current-btn :collapsed)))
                             (setq root (concat root "/" (pop ,dir-parts)))
                             (cl-incf index))
                           (cl-return-from search current-btn))
                          ;; node we're at has a smaller depth than the one we started from
                          ;; that means we overshot our target and there's nothing to be found here
                          ((>= depth (treemacs-button-get current-btn :depth))
                           (cl-return-from search)))))))
             (unless ,btn (throw 'follow-failed 'follow-failed))
             (goto-char ,btn)
             ;; don't open dir at the very end of the list since we only want to put
             ;; point in its line
             (when (and (eq 'dir-node-closed (treemacs-button-get ,btn :state))
                        (< index last-index))
               (treemacs--expand-dir-node ,btn :git-future git-future))
             (setq index (1+ index))))
         ,btn)))))

(defun treemacs--find-custom-top-level-node (path)
  "Find the position of the top level extension node at PATH."
  (let* ((root-key (cadr path))
         ;; go back here if the search fails
         ;; the root key isn't really a project, it's just the :root-key-form
         (start (prog1 (point) (goto-char (treemacs-project->position root-key))))
         ;; making a copy since the variable is a reference to a node actual path
         ;; and will be changed in-place here
         (goto-path (copy-sequence path))
         (counter (1- (length goto-path)))
         ;; manual as in to be expanded manually after we moved to the next closest node we can find
         ;; in the dom
         (manual-parts nil)
         (dom-node nil))
    ;; Try to move as close as possible to the followed node, starting with its immediate parent
    ;; keep moving upwards in the path we move to until reaching the root of the project. Root of
    ;; project is met when counter is one, (not zero like with other nodes), since the root path of
    ;; top-level extensions is of form (:CUSTOM Root-Key), already containing two elements.
    (while (and (> counter 1)
                (null dom-node))
      (setq dom-node (treemacs-find-in-dom goto-path)
            counter (1- counter))
      (cond
       ((null dom-node)
        (push (nth (1+ counter) goto-path) manual-parts)
        (setcdr (nthcdr counter goto-path) nil))
       ((and dom-node (null (treemacs-dom-node->position dom-node)))
        (setq dom-node nil)
        (push (nth (1+ counter) goto-path) manual-parts)
        (setcdr (nthcdr counter goto-path) nil))))
    (let* ((btn (if dom-node
                    (treemacs-dom-node->position dom-node)
                  (treemacs-project->position root-key)))
           ;; do the rest manually
           (search-result (if manual-parts
                              (treemacs--follow-path-elements btn manual-parts)
                            (goto-char btn))))
      (if (eq 'follow-failed search-result)
          (prog1 nil
            (goto-char start))
        search-result))))

(cl-macrolet
    ((define-find-custom-node (name project-form doc)
       `(defun ,name (path)
          ,doc
          (let* (;; go back here if the search fails
                 (project ,project-form)
                 (start (prog1 (point) (goto-char (treemacs-project->position project))))
                 ;; making a copy since the variable is a reference to a node actual path
                 ;; and will be changed in-place here
                 (goto-path (copy-sequence path))
                 ;; manual as in to be expanded manually after we moved to the next closest node we can find
                 ;; in the dom
                 (manual-parts nil)
                 (dom-node nil))
            ;; try to move as close as possible to the followed node, starting with its immediate parent
            ;; keep moving upwards in the path we move to until reaching the root of the project (counter = 0)
            ;; all the while collecting the parts of the path that beed manual expanding
            (-let [continue t]
              (while continue
                (setf dom-node (treemacs-find-in-dom goto-path))
                (if (or (null dom-node)
                        ;; dom node might exist, but a leaf's position is not always known
                        (null (treemacs-dom-node->position dom-node)))
                    (progn
                      (push (-last-item  goto-path) manual-parts)
                      (setf goto-path (-butlast goto-path))
                      (unless (cdr goto-path) (setf goto-path (car goto-path))))
                  (setf continue nil))))
            (let* ((btn (--if-let (treemacs-dom-node->position dom-node)
                            it
                          (treemacs-project->position project)))
                   ;; do the rest manually
                   (search-result (if manual-parts (treemacs--follow-path-elements btn manual-parts)
                                    (goto-char btn))))
              (if (eq 'follow-failed search-result)
                  (prog1 nil
                    (goto-char start))
                (treemacs-dom-node->set-position! (treemacs-find-in-dom path) search-result)
                search-result))))))
  (define-find-custom-node treemacs--find-custom-project-node (pop path)
    "Move to the project extension node at PATH.")
  (define-find-custom-node treemacs--find-custom-dir-node (treemacs--find-project-for-path (car path))
    "Move to the directory extension node at PATH."))

(defun treemacs-find-visible-node (path)
  "Find position of node at PATH.
Unlike `treemacs-find-node' this will not expand other nodes in the view, but
only look among those currently visible. The result however is the same: either
a marker ponting to the found node or nil.

Unlike `treemacs-find-node', this function does not go to the node.

PATH: Node Path"
  (-when-let (dom-node (treemacs-is-path-visible? path))
    (or (treemacs-dom-node->position dom-node)
        (save-excursion
          (treemacs-find-node path)))))

(defun treemacs-find-node (path &optional project)
  "Find position of node identified by PATH under PROJECT in the current buffer.
Inspite the signature this function effectively supports two different calling
conventions.

The first one is for movement towards a node that identifies a file. In this
case the signature is applied as is, and this function diverges simply into
`treemacs-goto-file-node'. PATH is a filepath string while PROJECT is fully
optional, as treemacs is able to determine which project, if any, a given file
belongs to. Providing the project is therefore only a matter of efficiency and
convenience. If PROJECT is not given it will be found with
`treemacs--find-project-for-path'. No attempt is made to verify that PATH falls
under a project in the workspace. It is assumed that this check has already been
made.

The second calling convention deals with custom nodes defined by an extension
for treemacs. In this case the PATH is made up of all the node keys that lead to
the node to be moved to.

For a directory extension, created with `treemacs-define-directory-extension',
that means that the path's first element must be the filepath of its parent. For
a project extension, created with `treemacs-define-project-extension', the
first element of the path must instead be the keyword `:custom', followed by the
node's unique path. The second argument is therefore ignored in this case.

Either way this fuction will return a marker to the moved to position if it was
successful.

PATH: Filepath | Node Path
PROJECT Project Struct"
  (treemacs-with-path path
    :file-action (when (file-exists-p path) (treemacs-find-file-node path project))
    :top-level-extension-action (treemacs--find-custom-top-level-node path)
    :directory-extension-action (treemacs--find-custom-dir-node path)
    :project-extension-action (treemacs--find-custom-project-node path)))

(defun treemacs-goto-node (path &optional project ignore-file-exists)
  "Move point to button identified by PATH under PROJECT in the current buffer.
Falls under the same constraints as `treemacs-find-node', but will actually move
point. Will do nothing if file at PATH does not exist, unless IGNORE-FILE-EXISTS
is non-nil.

PATH: Filepath | Node Path
PROJECT Project Struct
IGNORE-FILE-EXISTS. Boolean"
  (treemacs-with-path path
    :file-action (when (or ignore-file-exists (file-exists-p path)) (treemacs-goto-file-node path project))
    :top-level-extension-action (treemacs--goto-custom-top-level-node path)
    :directory-extension-action (treemacs--goto-custom-dir-node path)
    :project-extension-action (treemacs--goto-custom-project-node path)))

(defun treemacs-find-file-node (path &optional project)
  "Find position of node identified by PATH under PROJECT in the current buffer.
If PROJECT is not given it will be found with `treemacs--find-project-for-path'.
No attempt is made to verify that PATH falls under a project in the workspace.
It is assumed that this check has already been made.

PATH: File Path
PROJECT: Project Struct"
  (unless project (setq project (treemacs--find-project-for-path path)))
  (let* (;; go back here if the search fails
         (start (prog1 (point) (goto-char (treemacs-project->position project))))
         ;; the path we're moving to minus the project root
         (path-minus-root (->> project (treemacs-project->path) (length) (substring path)))
         ;; the parts of the path that we can try to go to until we arrive at the project root
         (dir-parts (nreverse (s-split (f-path-separator) path-minus-root :omit-nulls)))
         ;; the path we try to quickly move to because it's already open and thus in the dom
         (goto-path path)
         ;; manual as in to be expanded manually after we moved to the next closest node we can find
         ;; in the dom
         (manual-parts nil)
         (dom-node nil))
    ;; try to move as close as possible to the followed file, starting with its immediate parent
    ;; keep moving upwards in the path we move to until reaching the root of the project (counter = 0)
    ;; all the while collecting the parts of the path that beed manual expanding
    (-let [continue t]
      (while continue
        (setf dom-node (treemacs-find-in-dom goto-path)
              goto-path (treemacs--parent goto-path))
        (if (or (null dom-node)
                ;; dom node might exist, but a leaf's position is not always known
                (null (treemacs-dom-node->position dom-node)))
            (progn
              (push (pop dir-parts) manual-parts))
          (setf continue nil))))
    (let* ((btn (--if-let (treemacs-dom-node->position dom-node)
                    it
                  (treemacs-project->position project)))
           ;; do the rest manually - at least the actual file to move to is still left in manual-parts
           (search-result (if manual-parts (save-match-data
                                             (treemacs--follow-each-dir btn manual-parts project))
                            (goto-char btn))))
      (if (eq 'follow-failed search-result)
          (prog1 nil
            (goto-char start))
        (treemacs-dom-node->set-position! (treemacs-find-in-dom path) search-result)
        search-result))))

(cl-macrolet
    ((define-goto (name find-function has-project doc)
       `(define-inline ,name (path ,@(when has-project '(&optional project)))
          ,doc
          (inline-letevals (path ,@(when has-project '(project)))
            (inline-quote
             (-when-let (result (,find-function ,(quote ,path) ,@(when has-project '(,project))))
               (treemacs--evade-image)
               (hl-line-highlight)
               (set-window-point (get-buffer-window) (point))
               result))))))

  (define-goto treemacs-goto-file-node treemacs-find-file-node t
    "Move point to button identified by PATH under PROJECT in the current buffer.
Relies on `treemacs-find-file-node', and will also set window-point and ensure
hl-line highlighting.

Called by `treemacs-goto-node' when PATH identifies a file name.

PATH: Filepath
PROJECT: Project Struct")

  (define-goto treemacs--goto-custom-top-level-node treemacs--find-custom-top-level-node nil
    "Move to the top-level extension node at PATH, returning the button's position.")
  (define-goto treemacs--goto-custom-dir-node treemacs--find-custom-dir-node nil
    "Move to the directory extension node at PATH, returning the button's position.")
  (define-goto treemacs--goto-custom-project-node treemacs--find-custom-project-node nil
    "Move to the project extension node at PATH, returning the button's position."))

(defun treemacs--on-window-config-change ()
  "Collects all tasks that need to run on a window config change."
  (-when-let (w (treemacs-get-local-window))
    (treemacs-without-following
     (with-selected-window w
       ;; apparently keeping the hook around can lead to a feeback loop together with helms
       ;; auto-resize mode as seen in https://github.com/Alexander-Miller/treemacs/issues/76
       (let (window-configuration-change-hook)
         (set-window-parameter w 'no-delete-other-windows treemacs-no-delete-other-windows)
         (when treemacs-display-in-side-window
           (set-window-parameter w 'window-side treemacs-position)
           (set-window-parameter w 'window-slot 0))
         (when treemacs-is-never-other-window
           (set-window-parameter w 'no-other-window t)))))))

(defun treemacs--set-width (width)
  "Set the width of the treemacs buffer to WIDTH."
  (unless (one-window-p)
    (let ((window-size-fixed)
          (w (max width window-min-width)))
      (cond
       ((> (window-width) w)
        (shrink-window-horizontally  (- (window-width) w)))
       ((< (window-width) w)
        (enlarge-window-horizontally (- w (window-width))))))))

(defun treemacs--filter-files-to-be-shown (files)
  "Filter FILES for those files which treemacs should show.
These are the files which return nil for every function in
`treemacs-ignored-file-predicates' and do not match `treemacs-dotfiles-regex'.
The second test not apply if `treemacs-show-hidden-files' is t."
  (if treemacs-show-hidden-files
      (-filter #'treemacs--reject-ignored-files files)
    (-filter #'treemacs--reject-ignored-and-dotfiles files)))

(define-inline treemacs--std-ignore-file-predicate (file _)
  "The default predicate to detect ignored files.
Will return t when FILE
1) starts with '.#' (lockfiles)
2) starts with 'flycheck_' (flycheck temp files)
3) ends with '~' (backup files)
4) is surrounded with # (auto save files)
5) is '.git'
6) is '.' or '..' (default dirs)"
  (declare (side-effect-free t) (pure t))
  (inline-letevals (file)
    (inline-quote
     (let ((last (aref ,file (1- (length ,file)))))
       (or (string-prefix-p ".#" ,file)
           (and (eq ?# last) (eq ?# (aref ,file 0)))
           (eq ?~ last)
           (string-equal ,file ".")
           (string-equal ,file "..")
           (string-equal ,file ".git")
           (string-prefix-p "flycheck_" ,file))))))

(define-inline treemacs--mac-ignore-file-predicate (file _)
  "Ignore FILE if it is .DS_Store and .localized.
Will be added to `treemacs-ignored-file-predicates' on Macs."
  (declare (side-effect-free t) (pure t))
  (inline-letevals (file)
    (inline-quote
     (or (string-equal ,file ".DS_Store")
         (string-equal ,file ".localized")))))

(define-inline treemacs-current-visibility ()
  "Return whether the current visibility state of the treemacs buffer.
Valid states are 'visible, 'exists and 'none."
  (declare (side-effect-free t))
  (inline-quote
   (cond
    ((treemacs-get-local-window) 'visible)
    ((treemacs-get-local-buffer) 'exists)
    (t 'none))))

(defun treemacs--on-frame-kill (frame)
  "Remove its framelocal buffer when FRAME is killed."
  (--when-let (cdr (assq frame treemacs--buffer-access))
    (kill-buffer it))
  (setq treemacs--buffer-access
        (assq-delete-all frame treemacs--buffer-access))
  (unless treemacs--buffer-access
    (setq delete-frame-functions
          (delete #'treemacs--on-frame-kill delete-frame-functions))))

(defun treemacs--setup-buffer ()
  "Create and setup a buffer for treemacs in the right position and size."
  (if treemacs-display-in-side-window
      (-> (treemacs--get-framelocal-buffer)
          (display-buffer-in-side-window `((side . ,treemacs-position)))
          (select-window))
    (-> (selected-window)
        (frame-root-window)
        (split-window nil treemacs-position)
        (select-window))
      (-let [buf (treemacs--get-framelocal-buffer)]
        (switch-to-buffer buf)))
  (treemacs--forget-last-highlight)
  (set-window-dedicated-p (selected-window) t)
  (setq-local treemacs--in-this-buffer t)
  (let ((window-size-fixed))
    (treemacs--set-width treemacs-width)))

(define-inline treemacs--parent (path)
  "Parent of PATH, or PATH itself if PATH is the root directory.

PATH: Node Path"
  (declare (pure t) (side-effect-free t))
  (inline-letevals (path)
    (inline-quote
     (treemacs-with-path ,path
       :file-action (treemacs--parent-dir ,path)
       :top-level-extension-action (when (> (length ,path) 2) (butlast ,path))
       :directory-extension-action (if (> (length ,path) 2) (butlast ,path) (car ,path))
       :project-extension-action (if (> (length ,path) 2) (butlast ,path) (treemacs-project->path (car ,path)))))))

(defun treemacs--evade-image ()
  "The cursor visibly blinks when on top of an icon.
It needs to be moved aside in a way that works for all indent depths and
`treemacs-indentation' settings."
  (when (eq major-mode 'treemacs-mode)
    (beginning-of-line)
    (when (eq 'image (car (get-text-property (point) 'display)))
      (forward-char 1))))

(defun treemacs--read-first-project-path ()
  "Read the first project on init with an empty workspace.
This function is extracted here specifically so that treemacs-projectile can
overwrite it so as to present the project root instead of the current dir as the
first choice."
  (when (treemacs-workspace->is-empty?)
    (file-truename (read-directory-name "Project root: "))))

(defun treemacs--sort-value-selection ()
  "Interactive selection for a new `treemacs-sorting' value.
Retursns a cons cell of a descriptive string name and the sorting symbol."
  (declare (side-effect-free t))
  (let* ((sort-names '(("Sort Alphabetically Ascending" . alphabetic-asc)
                       ("Sort Alphabetically Descending" . alphabetic-desc)
                       ("Sort Case Insensitive Alphabetically Ascending" . alphabetic-case-insensitive-asc)
                       ("Sort Case Insensitive Alphabetically Descending" . alphabetic-case-insensitive-desc)
                       ("Sort by Size Ascending" . size-asc)
                       ("Sort by Size Descending" . size-desc)
                       ("Sort by Modification Date Ascending" . mod-time-asc)
                       ("Sort by Modification Date Descending" . mod-time-desc)))
         (selected-value (completing-read (format "Sort Method (current is %s)" treemacs-sorting)
                                          (-map #'car sort-names))))
    (--first (s-equals? (car it) selected-value) sort-names)))

(defun treemacs--kill-buffers-after-deletion (path is-file)
  "Clean up after a deleted file or directory.
Just kill the buffer visiting PATH if IS-FILE. Otherwise, go
through the buffer list and kill buffer if PATH is a prefix."
  (if is-file
      (let ((buf (get-file-buffer path)))
        (and buf
             (y-or-n-p (format "Kill buffer of %s, too? "
                               (treemacs--filename path)))
             (kill-buffer buf)))

    ;; Prompt for each buffer visiting a file in directory
    (--each (buffer-list)
      (and
       (treemacs-is-path (buffer-file-name it) :in path)
       (y-or-n-p (format "Kill buffer %s in %s, too? "
                         (buffer-name it)
                         (treemacs--filename path)))
       (kill-buffer it)))

    ;; Kill all dired buffers in one step
    (when (bound-and-true-p dired-buffers)
      (-when-let (dired-buffers-for-path
                  (->> dired-buffers
                       (--filter (treemacs-is-path (car it) :in path))
                       (-map #'cdr)))
        (and (y-or-n-p (format "Kill Dired buffers of %s, too? "
                               (treemacs--filename path)))
             (-each dired-buffers-for-path #'kill-buffer))))))

(defun treemacs--do-refresh (buffer project)
  "Execute the refresh process for BUFFER and PROJECT in that buffer.
Specifically extracted with the buffer to refresh being supplied so that
filewatch mode can refresh multiple buffers at once.
Will refresh every project when PROJECT is 'all."
  (with-current-buffer buffer
    (treemacs-save-position
     (progn
       (treemacs--cancel-refresh-timer)
       (run-hook-with-args
        'treemacs-pre-refresh-hook
        project curr-win-line curr-btn curr-state curr-file curr-node-path)

       (if (eq 'all project)
           (-each (treemacs-workspace->projects (treemacs-current-workspace)) #'treemacs-project->refresh!)
         (treemacs-project->refresh! project)))

     (run-hook-with-args
      'treemacs-post-refresh-hook
      project curr-win-line curr-btn curr-state curr-file curr-node-path)

     (unless treemacs-silent-refresh
       (treemacs-log "Refresh complete.")))))

(defun treemacs--setup-peek-buffer (btn &optional goto-tag?)
  "Setup the peek buffer and window for BTN.
Additionally also navigate to BTN's tag if GOTO-TAG is t.

BTN: Button
GOTO-TAG: Bool"
  (let ((path (file-truename
               (if goto-tag?
                   (treemacs-with-button-buffer btn
                     (treemacs--nearest-path btn))
                 (treemacs-safe-button-get btn :path))))
        (buffer-to-restore (current-buffer))
        (buffer-to-kill nil))
    (-if-let (buffer (get-file-buffer path))
        (switch-to-buffer buffer)
      (find-file path)
      (setq buffer-to-kill (current-buffer)))
    (when goto-tag?
      (treemacs--goto-tag btn))
    (unless treemacs--pre-peek-state
      (setq treemacs--pre-peek-state `(,(selected-window) ,buffer-to-restore ,buffer-to-kill)))
    (add-hook 'post-command-hook #'treemacs--restore-peeked-window)))

(defun treemacs--restore-peeked-window ()
  "Revert the buffer displayed in the peek window before it was used for peeking."
  (unless (memq this-command
                '(treemacs-peek treemacs-next-line-other-window treemacs-previous-line-other-window
                         treemacs-next-page-other-window treemacs-previous-page-other-window))
    (remove-hook 'post-command-hook #'treemacs--restore-peeked-window)
    (treemacs-without-following
      (when treemacs--pre-peek-state
        (-let [(window buffer-to-restore buffer-to-kill) treemacs--pre-peek-state]
          (setq treemacs--pre-peek-state nil)
          (when (buffer-live-p buffer-to-kill)
            (kill-buffer buffer-to-kill))
          (with-selected-window window
            (switch-to-buffer buffer-to-restore)))))))

(define-inline treemacs-is-node-file-or-dir? (node)
  "Return t when NODE is a file or directory."
  (inline-letevals (node)
    (inline-quote
     (memq (treemacs-button-get node :state)
           '(file-node-open file-node-closed dir-node-open dir-node-closed)))))

(define-inline treemacs-is-path-visible? (path)
  "Return whether a node for PATH is displayed in the current buffer.
Returns the backing dom node is the PATH is visible, nil otherwise.

Morally equivalent to `treemacs-find-in-dom'.

PATH: Node Path"
  (declare (side-effect-free t))
  (inline-letevals (path)
    (inline-quote
     (treemacs-find-in-dom ,path))))

(defun treemacs--copy-or-move (action)
  "Internal implementation for copying and moving files.
ACTION will be either `:copy' or `:move', depenting on whether we are calling
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
             action-function #'f-copy
             finish-msg      "Copied %s to %s"))
      (:move
       (setf no-node-msg     "There is nothing to move here."
             wrong-type-msg  "Only files and directories can be moved."
             prompt          "Move to: "
             action-function #'f-move
             finish-msg      "Moved %s to %s")))
    (treemacs-block
     (treemacs-unless-let (node (treemacs-node-at-point))
         (treemacs-error-return no-node-msg)
       (treemacs-error-return-if (not (treemacs-is-node-file-or-dir? node))
         wrong-type-msg)
       (let* ((source (treemacs-button-get node :path))
              (destination (file-name-as-directory (read-directory-name prompt nil default-directory :must-match)))
              (filename (treemacs--filename source))
              (move-to-on-success (f-join destination filename)))
         (when (file-exists-p (f-join destination filename))
           (let* ((filename-no-ext (f-no-ext filename))
                  (filename-ext (f-ext filename))
                  (copy-template (if filename-ext " (Copy %s)." " (Copy %s)"))
                  (new-name (concat filename-no-ext (format copy-template 1) filename-ext))
                  (new-dest (f-join destination new-name)))
             ;; if even "destfile (Copy 1).ext" already exists try "destfile (Copy 2).ext" etc.
             (-let [n 1]
               (while (file-exists-p new-dest)
                 (cl-incf n)
                 (setf new-dest (f-join destination (concat filename-no-ext (format copy-template n) filename-ext)))))
             (setf destination new-dest
                   move-to-on-success destination)))
         (when (eq action :move)
           ;; do the deletion *before* moving the file, otherwise it will no longer exist and treemacs will
           ;; not recognize it as a file path
           (treemacs-do-delete-single-node source))
         (treemacs--without-filewatch
          (funcall action-function source destination))
         ;; no waiting for filewatch, if we copied to an expanded directory refresh it immediately
         (-let [parent (treemacs--parent move-to-on-success)]
           (when (treemacs-is-path-visible? parent)
             (treemacs-do-update-node parent)))
         (treemacs-goto-file-node move-to-on-success)
         (treemacs-pulse-on-success finish-msg
           (propertize filename 'face 'font-lock-string-face)
           (propertize destination 'face 'font-lock-string-face)))))))

(provide 'treemacs-core-utils)

;;; treemacs-core-utils.el ends here
