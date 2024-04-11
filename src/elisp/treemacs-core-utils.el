;;; treemacs.el --- A tree style file viewer package -*- lexical-binding: t -*-

;; Copyright (C) 2024 Alexander Miller

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

;; General implementation details.

;;; Code:

(require 'hl-line)
(require 'dash)
(require 's)
(require 'ht)
(require 'pfuture)
(require 'treemacs-customization)
(require 'treemacs-logging)

(eval-when-compile
  (require 'inline)
  (require 'cl-lib)
  (require 'treemacs-macros))

(treemacs-import-functions-from "cfrs"
  cfrs-read)

(treemacs-import-functions-from "treemacs-interface"
  treemacs-toggle-node)

(treemacs-import-functions-from "treemacs-tags"
  treemacs--expand-file-node
  treemacs--collapse-file-node
  treemacs--expand-tag-node
  treemacs--collapse-tag-node
  treemacs--extract-position
  treemacs--goto-tag)

(treemacs-import-functions-from "treemacs"
  treemacs-refresh)

(treemacs-import-functions-from "treemacs-scope"
  treemacs-get-local-window
  treemacs-get-local-buffer
  treemacs-get-local-buffer-create
  treemacs-scope-shelf->buffer
  treemacs-scope-shelf->workspace
  treemacs-current-visibility
  treemacs--select-visible-window
  treemacs--remove-buffer-after-kill
  treemacs--scope-store)

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
  treemacs--forget-previously-follow-tag-btn)

(treemacs-import-functions-from "treemacs-async"
  treemacs--git-status-process
  treemacs--non-simple-git-mode-enabled
  treemacs-update-single-file-git-state
  treemacs--flattened-dirs-process)

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

(treemacs-import-functions-from "treemacs-annotations"
  treemacs--delete-annotation)

(declare-function treemacs-mode "treemacs-mode")

(defconst treemacs--empty-table (ht)
  "Constant value of an empty hash table.
Used to avoid creating unnecessary garbage.")

(defvar treemacs--closed-node-states
  '(root-node-closed
    dir-node-closed
    file-node-closed
    tag-node-closed)
  "States marking a node as closed.
Used in `treemacs-is-node-collapsed?'")

(defvar treemacs--open-node-states
  '(project-node-open
    root-node-open
    dir-node-open
    file-node-open
    tag-node-open)
  "States marking a node as open.
Used in `treemacs-is-node-expanded?'")

(define-inline treemacs--unslash (path)
  "Remove the final slash in PATH."
  (declare (pure t) (side-effect-free t))
  (inline-letevals (path)
    (inline-quote
     (if (and (> (length ,path) 1)
              (eq ?/ (aref ,path (1- (length ,path)))))
         (substring ,path 0 -1)
       ,path))))

(define-inline treemacs-string-trim-right (string)
  "Trim STRING of trailing string matching REGEXP.

Same as the builtin `string-trim-right', but re-implemented here for Emacs 27."
  (declare (side-effect-free t))
  (inline-letevals (string)
    (inline-quote
     (let ((i (string-match-p "\\(?:[ \t\n\r]+\\)\\'" ,string)))
       (if i (substring ,string 0 i) ,string)))))

(define-inline treemacs--prefix-arg-to-recurse-depth (arg)
  "Translates prefix ARG into a number.
Used for depth-based expansion of nodes - a numeric prefix will translate to
itself, the default representation translates to 9999."
  (declare (pure t) (side-effect-free t))
  (inline-letevals (arg)
    (inline-quote
     (cond
      ((null ,arg) 0)
      ((integerp ,arg) ,arg)
      (t 999)))))

(defun treemacs--all-buttons-with-depth (depth)
  "Get all buttons with the given DEPTH."
  (declare (side-effect-free t))
  (save-excursion
    (goto-char (point-min))
    (let ((current-btn (treemacs-current-button))
          (result))
      (when (and current-btn
                 (= depth (treemacs-button-get current-btn :depth)))
        (push current-btn result))
      (while (= 0 (forward-line 1))
        (setf current-btn (treemacs-current-button))
        (when (and current-btn
                   (= depth (treemacs-button-get current-btn :depth)))
          (push current-btn result)))
      result)))

(define-inline treemacs--parent-dir (path)
  "Return the parent of PATH is it's a file, or PATH if it is a directory.

PATH: File Path"
  (declare (side-effect-free t) (pure t))
  (inline-letevals (path)
    (inline-quote
     (-> ,path
         (file-name-directory)
         (treemacs--unslash)))))

(defconst treemacs--buffer-name-prefix " *Treemacs-")

(defconst treemacs-dir
  ;; locally we're in src/elisp, installed from melpa we're at the package root
  (-let [dir (-> (if load-file-name
                     (file-name-directory load-file-name)
                   default-directory)
                 (expand-file-name))]
    (if (s-ends-with? "src/elisp/" dir)
        (-> dir (treemacs--unslash) (treemacs--parent-dir) (treemacs--parent-dir))
      dir))
  "The directory treemacs.el is stored in.")

(defvar-local treemacs--width-is-locked t
  "Keeps track of whether the width of the treemacs window is locked.")

(defvar-local treemacs--in-this-buffer nil
  "Non-nil only in buffers meant to show treemacs.
Used to show an error message if someone mistakenly activates `treemacs-mode'.")

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
Unlike the function `kill-whole-line' this won't pollute the kill ring."
  (inline-quote
    (delete-region (line-beginning-position) (min (point-max) (1+ (line-end-position))))))

(define-inline treemacs-current-button ()
  "Get the button in the current line.
Returns nil when point is between projects."
  (declare (side-effect-free error-free))
  (inline-quote
   (-some->
    (text-property-not-all (line-beginning-position) (line-end-position) 'button nil)
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
  "Get the property of button BUTTON named PROP.
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

(define-inline treemacs--get-label-of (btn)
  "Return the text label of BTN."
  (declare (side-effect-free t))
  (inline-quote
   (buffer-substring-no-properties (treemacs-button-start ,btn) (treemacs-button-end ,btn))))

(define-inline treemacs--tokenize-path (path exclude-prefix)
  "Get the PATH's single elements, excluding EXCLUDE-PREFIX.
For example the input /A/B/C/D/E + /A/B will return [C D E].

PATH: File Path
EXCLUDE-PREFIX: File Path"
  (declare (pure t) (side-effect-free t))
  (inline-letevals (path exclude-prefix)
    (inline-quote
     (treemacs-split-path (substring ,path (length ,exclude-prefix))))))

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

(define-inline treemacs--select-not-visible-window ()
  "Switch to treemacs buffer, given that it not visible."
  (inline-quote
   (let ((buffer (current-buffer)))
     (treemacs--setup-buffer)
     (when (or treemacs-follow-after-init
               (with-no-warnings treemacs-follow-mode))
       (with-current-buffer buffer (treemacs--follow)))
     (run-hook-with-args 'treemacs-select-functions 'exists))))

(define-inline treemacs--button-symbol-switch (new-symbol)
  "Replace icon in current line with NEW-SYMBOL."
  (inline-letevals (new-symbol)
    (inline-quote
     (save-excursion
       (let ((len (length ,new-symbol)))
         (goto-char (- (treemacs-button-start (next-button (line-beginning-position) t)) len))
         (insert ,new-symbol)
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

(defun treemacs--file-extension (filename)
  "Same as `file-name-extension', but also works with leading periods.

This is something a of workaround to easily allow assigning icons to a FILENAME
with a name like '.gitignore' without always having to check for both filename
extensions and special names like this."
  (declare (side-effect-free t))
  (if (string-match treemacs-file-extension-regex filename)
      (substring filename (1+ (match-beginning 0)))
    filename))

(define-inline treemacs-is-treemacs-window? (window)
  "Return t when WINDOW is showing a treemacs buffer."
  (declare (side-effect-free t))
  (inline-quote
   (->> ,window (window-buffer) (buffer-name) (s-starts-with? treemacs--buffer-name-prefix))))

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

(define-inline treemacs--on-file-deletion (path &optional no-buffer-delete)
  "Cleanup to run when treemacs file at PATH was deleted.
Do not try to delete buffers for PATH when NO-BUFFER-DELETE is non-nil.  This is
necessary since interacting with magit can cause file delete events for files
being edited to trigger."
  (inline-letevals (path no-buffer-delete)
    (inline-quote
     (progn
       (treemacs--delete-annotation ,path)
       (unless ,no-buffer-delete (treemacs--kill-buffers-after-deletion ,path t))
       (treemacs--stop-watching ,path t)
       ;; filewatch mode needs the node's information to be in the dom
       (unless (with-no-warnings treemacs-filewatch-mode)
         (treemacs-run-in-every-buffer
          (treemacs-on-collapse ,path t)))
       (when (treemacs--non-simple-git-mode-enabled)
         (treemacs-run-in-every-buffer
          (treemacs-update-single-file-git-state (treemacs--parent-dir ,path))))))))

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

(define-inline treemacs-canonical-path (path)
  "The canonical version of PATH for being handled by treemacs.
In practice this means expand PATH and remove its final slash."
  (declare (pure t) (side-effect-free t))
  (inline-letevals (path)
    (inline-quote
     (if (file-remote-p ,path)
         (treemacs--unslash ,path)
       (let (file-name-handler-alist)
         (-> ,path (expand-file-name) (treemacs--unslash)))))))
;; TODO(2020/12/28): alias is for backwards compatibility, remove it eventually
(defalias 'treemacs--canonical-path #'treemacs-canonical-path)

(define-inline treemacs-is-file-git-ignored? (file git-info)
  "Determined if FILE is ignored by git by means of GIT-INFO."
  (declare (side-effect-free t))
  (inline-letevals (file git-info)
    (inline-quote (eq 'treemacs-git-ignored-face (ht-get ,git-info ,file)))))

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
          (setq buff-file (expand-file-name buff-file))
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
      (setq buff-file (expand-file-name buff-file))
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
  "Initialise a treemacs buffer from the current workspace.
Add a project for ROOT and NAME if they are non-nil."
  (treemacs--maybe-load-workspaces)
  (let ((origin-buffer (current-buffer))
        (current-workspace (treemacs-current-workspace))
        (run-hook? nil)
        (visibility (treemacs-current-visibility)))
    (pcase visibility
      ('visible (treemacs--select-visible-window))
      ('exists (treemacs--select-not-visible-window))
      ('none
       (treemacs--setup-buffer)
       (treemacs-mode)
       ;; Render the projects even if there are none. This ensures that top-level
       ;; extensions are always rendered, and the project markers are initialized.
       (treemacs--render-projects (treemacs-workspace->projects current-workspace))
       (when (treemacs-workspace->is-empty?)
         (let* ((path (-> (treemacs--read-first-project-path)
                          (treemacs-canonical-path)))
                (name (treemacs--filename path)))
           (treemacs-do-add-project-to-workspace path name)
           (treemacs-log "Created first project.")))
       (goto-char 2)
       (run-hooks 'treemacs-post-buffer-init-hook)
       (setf run-hook? t)))
    (when root (treemacs-do-add-project-to-workspace (treemacs-canonical-path root) name))
    (with-no-warnings (setq treemacs--ready-to-follow t))
    (let* ((origin-file (buffer-file-name origin-buffer))
           (file-project (treemacs-is-path origin-file :in-workspace)))
      (cond
       ((and (or treemacs-follow-after-init (with-no-warnings treemacs-follow-mode))
             file-project)
        (treemacs-goto-file-node origin-file file-project))
       (treemacs-expand-after-init
        (treemacs-toggle-node))))
    ;; The hook should run at the end of the setup, but also only
    ;; if a new buffer was created, as the other cases are already covered
    ;; in their respective setup functions.
    (when run-hook? (run-hook-with-args 'treemacs-select-functions visibility))))

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
                   root (treemacs-join-path root dir-part)
                   ,btn
                   (let (current-btn)
                     (cl-block search
                       ;; first a plain text-based search for the current dir-part string
                       ;; then we grab the node we landed at and see what's going on
                       ;; there's a couple ways this can go
                       (while (progn (goto-char (line-end-position)) (search-forward dir-part nil :no-error))
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
only look among those currently visible.  The result however is the same: either
a marker pointing to the found node or nil.

Unlike `treemacs-find-node', this function does not go to the node.

PATH: Node Path"
  (-when-let (dom-node (treemacs-is-path-visible? path))
    (or (treemacs-dom-node->position dom-node)
        (save-excursion
          (treemacs-find-node path)))))

(defun treemacs-find-node (path &optional project)
  "Find position of node identified by PATH under PROJECT in the current buffer.

In spite of the signature this function effectively supports two different
calling conventions.

The first one is for movement towards a node that identifies a normal file.  In
this case the signature is applied as is, and this function diverges simply into
`treemacs-goto-file-node'.  PATH is a file path string while PROJECT is a
`treemacs-project' struct instance and fully optional, as treemacs is able to
determine which project, if any, a given file belongs to.  Providing the project
when it happens to be available is therefore only a small optimisation.  If
PROJECT is not given it will be found with `treemacs--find-project-for-path'.
No attempt is made to verify that PATH actually falls under a project in the
workspace.  It is assumed that this check has already been made.

The second calling convention deals with custom nodes defined by an extension
for treemacs.  In this case the PATH is made up of all the node keys that lead
to the node to be moved to and PROJECT is not used.

Either way this function will return a marker to the moved-to position if it was
successful.

PATH: Filepath | Node Path
PROJECT Project Struct"
  (save-excursion
    (treemacs-with-path path
      :file-action (when (and (eq t treemacs--in-this-buffer)
                              (file-exists-p path))
                     (treemacs-find-file-node path project))
      :extension-action (treemacs--find-custom-node path))))

(defun treemacs--find-custom-node (path)
  "Specialisation to find a custom node at the given PATH."
  (let* (;; go back here if the search fails
         (start (point))
         ;; (top-pos (treemacs-dom-node->position (treemacs-find-in-dom (car path))))
         ;; making a copy since the variable is a reference to a node actual path
         ;; and will be changed in-place here
         (goto-path (if (listp path) (copy-sequence path) (list path)))
         ;; manual as in to be expanded manually after we moved to the next closest node we can find
         ;; in the dom
         (manual-parts nil)
         (dom-node nil))
    (-let [continue t]
      (while continue
        (setf dom-node (treemacs-find-in-dom goto-path))
        (if (or (null dom-node)
                ;; dom node might exist, but a leaf's position is not always known
                (null (treemacs-dom-node->position dom-node)))
            (if (cdr goto-path)
                (progn
                  (push (-last-item  goto-path) manual-parts)
                  (setf goto-path (-butlast goto-path)))
              (setf goto-path (car goto-path)))
          (setf continue nil))))
    (let* ((btn (treemacs-dom-node->position dom-node))
           ;; do the rest manually
           (search-result (if manual-parts (treemacs--follow-path-elements btn manual-parts)
                            (goto-char btn))))
      (if (eq 'follow-failed search-result)
          (prog1 nil
            (goto-char start))
        (treemacs-dom-node->set-position! (treemacs-find-in-dom path) search-result)
        search-result))))

(defun treemacs-goto-node (path &optional project ignore-file-exists)
  "Move point to button identified by PATH under PROJECT in the current buffer.
Falls under the same constraints as `treemacs-find-node', but will actually move
point.  Will do nothing if file at PATH does not exist, unless
IGNORE-FILE-EXISTS is non-nil.

PATH: Filepath | Node Path
PROJECT Project Struct
IGNORE-FILE-EXISTS Boolean"
  (treemacs-with-path path
    :file-action (when (or ignore-file-exists (file-exists-p path))
                   (treemacs-goto-file-node path project))
    :extension-action (treemacs-goto-extension-node path)))

(define-inline treemacs-goto-extension-node (path)
  "Move to an extension node at the given PATH.
Small short-cut over `treemacs-goto-node' if you know for certain that PATH
leads to an extension node."
  (inline-letevals (path)
    (inline-quote
     (-when-let (result (treemacs--find-custom-node ,path))
       (treemacs--evade-image)
       (hl-line-highlight)
       ;; Only change window point if the current buffer is actually visible
       (-when-let (window (get-buffer-window))
         (set-window-point window (point)))
       result))))

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
         (dir-parts (nreverse (s-split "/" path-minus-root :omit-nulls)))
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
               ;; Only change window point if the current buffer is actually visible
               (-when-let (window (get-buffer-window))
                 (set-window-point window (point)))
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
          (w (max width window-safe-min-width)))
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
1) starts with \".#\" (lockfiles)
2) starts with \"flycheck_\" (flycheck temp files)
3) ends with \"~\" (backup files)
4) is surrounded with \"#\" (auto save files)
5) is \".git\" (see also `treemacs-hide-dot-git-directory')
6) is \".\" or \"..\" (default dirs)"
  (declare (side-effect-free t) (pure t))
  (inline-letevals (file)
    (inline-quote
     (let ((last (aref ,file (1- (length ,file)))))
       (or (string-prefix-p ".#" ,file)
           (and (eq ?# last) (eq ?# (aref ,file 0)))
           (eq ?~ last)
           (string-equal ,file ".")
           (string-equal ,file "..")
           (and treemacs-hide-dot-git-directory
                (string-equal ,file ".git"))
           (string-prefix-p "flycheck_" ,file))))))

(define-inline treemacs--mac-ignore-file-predicate (file _)
  "Ignore FILE if it is .DS_Store and .localized.
Will be added to `treemacs-ignored-file-predicates' on Macs."
  (declare (side-effect-free t) (pure t))
  (inline-letevals (file)
    (inline-quote
     (or (string-equal ,file ".DS_Store")
         (string-equal ,file ".localized")))))

(defun treemacs--popup-window ()
  "Pop up a side window and buffer for treemacs."
  (let ((buf (treemacs-get-local-buffer-create)))
    (display-buffer buf
                    `(,(if treemacs-display-in-side-window
                           'display-buffer-in-side-window
                         'display-buffer-in-direction)
                      . (;; for buffer in direction
                         (direction . ,treemacs-position)
                         (window . root)
                         ;; for side windows
                         (slot . -1)
                         (side . ,treemacs-position)
                         ;; general-purpose settings
                         (window-width . ,treemacs-width)
                         (dedicated . t))))
    (select-window (get-buffer-window buf))))

(defun treemacs--setup-buffer ()
  "Create and setup a buffer for treemacs in the right position and size."
  (-if-let (lv-buffer (-some->
                       (--find (string= " *LV*" (buffer-name (window-buffer it)))
                               (window-list (selected-frame)))
                       (window-buffer)))
      (progn
        ;; workaround for LV windows like spacemacs' transient states preventing
        ;; side windows from popping up right
        ;; see https://github.com/abo-abo/hydra/issues/362
        (with-current-buffer lv-buffer (setf window-size-fixed nil))
        (treemacs--popup-window)
        (with-current-buffer lv-buffer (setf window-size-fixed t)))
    (treemacs--popup-window))
  (setq-local treemacs--in-this-buffer t))

(define-inline treemacs--parent (path)
  "Parent of PATH, or PATH itself if PATH is the root directory.

PATH: Node Path"
  (declare (pure t) (side-effect-free t))
  (inline-letevals (path)
    (inline-quote
     (treemacs-with-path ,path
       :file-action (treemacs--parent-dir ,path)
       :extension-action (-butlast ,path)
       :no-match-action (user-error "Path %s appears to be neither a file nor an extension" ,path)))))

(define-inline treemacs--evade-image ()
  "The cursor visibly blinks when on top of an icon.
It needs to be moved aside in a way that works for all indent depths and
`treemacs-indentation' settings."
  (inline-quote
   (when (eq major-mode 'treemacs-mode)
     (beginning-of-line)
     (when (eq 'image (car-safe (get-text-property (point) 'display)))
       (forward-char 1)))))

(defun treemacs--read-first-project-path ()
  "Read the first project on start with an empty workspace.
This function is extracted here specifically so that treemacs-projectile can
overwrite it so as to present the project root instead of the current dir as the
first choice."
  (when (treemacs-workspace->is-empty?)
    (file-truename (read-directory-name "Project root: "))))

(defun treemacs--sort-value-selection ()
  "Interactive selection for a new `treemacs-sorting' value.
Returns a cons cell of a descriptive string name and the sorting symbol."
  (declare (side-effect-free t))
  (let* ((sort-names '(("Sort Alphabetically Ascending" . alphabetic-asc)
                       ("Sort Alphabetically Descending" . alphabetic-desc)
                       ("Sort Alphabetically and Numerically Ascending" . alphabetic-numeric-asc)
                       ("Sort Alphabetically and Numerically Descending" . alphabetic-numeric-desc)
                       ("Sort Case Insensitive Alphabetically Ascending" . alphabetic-case-insensitive-asc)
                       ("Sort Case Insensitive Alphabetically Descending" . alphabetic-case-insensitive-desc)
                       ("Sort Case Insensitive Alphabetically and Numerically Ascending" . alphabetic-numeric-case-insensitive-asc)
                       ("Sort Case Insensitive Alphabetically and Numerically Descending" . alphabetic-numeric-case-insensitive-desc)
                       ("Sort by Size Ascending" . size-asc)
                       ("Sort by Size Descending" . size-desc)
                       ("Sort by Modification Date Ascending" . mod-time-asc)
                       ("Sort by Modification Date Descending" . mod-time-desc)))
         (selected-value (completing-read (format "Sort Method (current is %s)" treemacs-sorting)
                                          (-map #'car sort-names))))
    (--first (s-equals? (car it) selected-value) sort-names)))

(defun treemacs--kill-buffers-after-deletion (path is-file)
  "Clean up after a deleted file or directory.
Just kill the buffer visiting PATH if IS-FILE.  Otherwise, go
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
Will refresh every project when PROJECT is \\='all."
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

(defun treemacs--find-repeated-file-name (path)
  "Find a fitting copy name for given file PATH.
Returns a name in the /file/name (Copy 1).ext.  If that also already
exists it returns /file/name (Copy 2).ext etc."
  (let* ((n 0)
         (dir (treemacs--parent-dir path))
         (filename (treemacs--filename path))
         (filename-no-ext (file-name-sans-extension path))
         (ext (--when-let (file-name-extension filename) (concat "." it)))
         (template " (Copy %d)")
         (new-path path))
    (while (file-exists-p new-path)
      (cl-incf n)
      (setf new-path (treemacs-join-path dir (concat filename-no-ext (format template n) ext))))
    new-path))

(defun treemacs--read-string (prompt &optional initial-input)
  "Read a string with an interface based on `treemacs-read-string-input'.
PROMPT and INITIAL-INPUT will be passed on to the read function.

PROMPT: String
INITIAL-INPUT: String"
  (declare (side-effect-free t))
  (pcase treemacs-read-string-input
    ('from-child-frame (cfrs-read prompt initial-input))
    ('from-minibuffer  (read-string prompt initial-input))
    (other (user-error "Unknown read-string-input value: `%s'" other))))

(defun treemacs-join-path (&rest items)
  "Join the given ITEMS to a single file PATH."
  (declare (side-effect-free t))
  (--reduce-from (expand-file-name it acc) "/" items))

(define-inline treemacs-split-path (path)
  "Split the given PATH into single items."
  (declare (pure t) (side-effect-free t))
  (inline-letevals (path)
    (inline-quote (split-string ,path "/" :omit-nulls))))

(defun treemacs--jump-to-next-treemacs-window ()
  "Jump from the current to the next treemacs-based window.
Will do nothing and return nil if no such window exists, or if there is only one
treemacs window."
  (let* ((current-window (selected-window))
         (treemacs-windows
          (--filter
           (buffer-local-value 'treemacs--in-this-buffer (window-buffer it))
           (window-list))))
    (-when-let (idx (--find-index (equal it current-window) treemacs-windows))
      (-let [next-window (nth (% (1+ idx) (length treemacs-windows)) treemacs-windows)]
        (unless (eq next-window current-window)
          (select-window next-window))))))

(provide 'treemacs-core-utils)

;;; treemacs-core-utils.el ends here
