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

;; Code in this file is considered performance critical.  The usual
;; restrictions w.r.t quality, readability and maintainability are
;; lifted here.

;;; Code:

(require 's)
(require 'ht)
(require 'treemacs-core-utils)
(require 'treemacs-icons)
(require 'treemacs-async)
(require 'treemacs-customization)
(require 'treemacs-dom)
(require 'treemacs-workspaces)
(require 'treemacs-visuals)
(require 'treemacs-logging)
(require 'treemacs-annotations)

(eval-when-compile
  (require 'cl-lib)
  (require 'treemacs-macros)
  (require 'inline))

(treemacs-import-functions-from "treemacs"
  treemacs-select-window)

(treemacs-import-functions-from "treemacs-filewatch-mode"
  treemacs--start-watching
  treemacs--stop-watching)

(treemacs-import-functions-from "treemacs-visuals"
  treemacs--get-indentation)

(treemacs-import-functions-from "treemacs-interface"
  treemacs-add-project-to-workspace
  treemacs-TAB-action)

(treemacs-import-functions-from "treemacs-extensions"
  treemacs--apply-root-top-extensions
  treemacs--apply-root-bottom-extensions
  treemacs--apply-project-top-extensions
  treemacs--apply-project-bottom-extensions
  treemacs--apply-directory-top-extensions
  treemacs--apply-directory-bottom-extensions)

(treemacs-import-functions-from "treemacs-tags"
  treemacs--expand-file-node
  treemacs--expand-tag-node)

(defvar-local treemacs--projects-end nil
  "Marker pointing to position at the end of the last project.

If there are no projects, points to the position at the end of any top level
extensions positioned to `TOP'.  This can always be used as the insertion point
for new projects.")

(defvar treemacs--file-name-handler-alist nil
  "Value of `file-name-handler-alist' when treemacs loads a directory's content.")

(defvar treemacs--no-recenter nil
  "Set for non-interactive updates.
When non-nil `treemacs--maybe-recenter' will have no effect.")

(define-inline treemacs--projects-end ()
  "Importable accessor for `treemacs--projects-end'."
  (declare (side-effect-free t))
  (inline-quote treemacs--projects-end))

(define-inline treemacs--button-at (pos)
  "Return the button at position POS in the current buffer, or nil.
If the button at POS is a text property button, the return value
is a marker pointing to POS."
  (declare (side-effect-free t))
  (inline-letevals (pos)
    (inline-quote (copy-marker ,pos t))))

(define-inline treemacs--button-in-line (pos)
  "Return the button in the line at POS in the current buffer, or nil.
If the button at POS is a text property button, the return value
is a marker pointing to POS."
  (inline-letevals (pos)
    (inline-quote
     (save-excursion
       (goto-char ,pos)
       (copy-marker
        (next-single-property-change
         (line-beginning-position) 'button nil (line-end-position))
        t)))))

(define-inline treemacs--current-screen-line ()
  "Get the current screen line in the selected window."
  (declare (side-effect-free t))
  (inline-quote
   (max 1 (count-screen-lines (window-start) (point-at-eol)))))

(define-inline treemacs--lines-in-window ()
  "Determine the number of lines visible in the current (treemacs) window.
A simple call to something like `window-screen-lines' is insufficient because
the height of treemacs' icons must be taken into account."
  (declare (side-effect-free t))
  (inline-quote
   (/ (- (window-pixel-height) (window-mode-line-height))
      (max treemacs--icon-size (frame-char-height)))))

(define-inline treemacs--sort-alphabetic-asc (f1 f2)
  "Sort F1 and F2 alphabetically ascending."
  (declare (pure t) (side-effect-free t))
  (inline-letevals (f1 f2)
    (inline-quote (string-lessp ,f1 ,f2))))

(define-inline treemacs--sort-alphabetic-desc (f1 f2)
  "Sort F1 and F2 alphabetically descending."
  (declare (pure t) (side-effect-free t))
  (inline-letevals (f1 f2)
    (inline-quote (string-lessp ,f2 ,f1))))

(define-inline treemacs--sort-alphabetic-case-insensitive-asc (f1 f2)
  "Sort F1 and F2 case insensitive alphabetically ascending."
  (declare (pure t) (side-effect-free t))
  (inline-letevals (f1 f2)
    (inline-quote (string-lessp (downcase ,f1) (downcase ,f2)))))

(define-inline treemacs--sort-alphabetic-case-insensitive-desc (f1 f2)
  "Sort F1 and F2 case insensitive alphabetically descending."
  (declare (pure t) (side-effect-free t))
  (inline-letevals (f1 f2)
    (inline-quote (string-lessp (downcase ,f2) (downcase ,f1)))))

(define-inline treemacs--sort-size-asc (f1 f2)
  "Sort F1 and F2 by size ascending."
  (declare (side-effect-free t))
  (inline-letevals (f1 f2)
    (inline-quote
     (< (nth 7 (file-attributes ,f1))
        (nth 7 (file-attributes ,f2))))))

(define-inline treemacs--sort-size-desc (f1 f2)
  "Sort F1 and F2 by size descending."
  (declare (side-effect-free t))
  (inline-letevals (f1 f2)
    (inline-quote
     (>= (nth 7 (file-attributes ,f1))
         (nth 7 (file-attributes ,f2))))))

(define-inline treemacs--sort-mod-time-asc (f1 f2)
  "Sort F1 and F2 by modification time ascending."
  (declare (side-effect-free t))
  (inline-letevals (f1 f2)
    (inline-quote (file-newer-than-file-p ,f2 ,f1))))

(define-inline treemacs--sort-mod-time-desc (f1 f2)
  "Sort F1 and F2 by modification time descending."
  (declare (side-effect-free t))
  (inline-letevals (f1 f2)
    (inline-quote (file-newer-than-file-p ,f1 ,f2))))

(define-inline treemacs--insert-root-separator ()
  "Insert a root-level separator at point, moving point after the separator."
  (inline-quote
   (insert (if treemacs-space-between-root-nodes "\n\n" "\n"))))

(define-inline treemacs--get-sort-fuction ()
  (declare (side-effect-free t))
  (inline-quote
   (pcase treemacs-sorting
     ('alphabetic-asc #'treemacs--sort-alphabetic-asc)
     ('alphabetic-desc #'treemacs--sort-alphabetic-desc)
     ('alphabetic-case-insensitive-asc  #'treemacs--sort-alphabetic-case-insensitive-asc)
     ('alphabetic-case-insensitive-desc #'treemacs--sort-alphabetic-case-insensitive-desc)
     ('size-asc #'treemacs--sort-size-asc)
     ('size-desc #'treemacs--sort-size-desc)
     ('mod-time-asc #'treemacs--sort-mod-time-asc)
     ('mod-time-desc #'treemacs--sort-mod-time-desc)
     (other other))))

(define-inline treemacs--get-dir-content (dir)
  "Get the content of DIR, separated into sub-lists of first dirs, then files."
  (inline-letevals (dir)
    (inline-quote
     ;; `directory-files' is much faster in a temp buffer for whatever reason
     (with-temp-buffer
       (let* ((file-name-handler-alist treemacs--file-name-handler-alist)
              (sort-func (treemacs--get-sort-fuction))
              (entries (-> ,dir (directory-files :absolute-names nil :no-sort) (treemacs--filter-files-to-be-shown)))
              (dirs-files (-separate #'file-directory-p entries)))
         (list (sort (car dirs-files) sort-func)
               (sort (cadr dirs-files) sort-func)))))))

(define-inline treemacs--create-dir-button-strings (path prefix parent depth)
  "Return the text to insert for a directory button for PATH.
PREFIX is a string inserted as indentation.
PARENT is the (optional) button under which this one is inserted.
DEPTH indicates how deep in the filetree the current button is."
  ;; for directories the icon is included in the prefix since it's always known
  (inline-letevals (path prefix parent depth)
    (inline-quote
     (list
      ,prefix
      (propertize (->> ,path file-name-nondirectory (funcall treemacs-directory-name-transformer))
                  'button '(t)
                  'category 'default-button
                  'help-echo nil
                  'keymap nil
                  :default-face 'treemacs-directory-face
                  :state 'dir-node-closed
                  :path ,path
                  :key ,path
                  :symlink (file-symlink-p ,path)
                  :parent ,parent
                  :depth ,depth)))))

(define-inline treemacs--create-file-button-strings (path prefix parent depth)
  "Return the text to insert for a file button for PATH.
PREFIX is a string inserted as indentation.
PARENT is the (optional) button under which this one is inserted.
DEPTH indicates how deep in the filetree the current button is."
  (inline-letevals (path prefix parent depth)
    (inline-quote
     (list
      ,prefix
      (treemacs-icon-for-file ,path)
      (propertize (->> ,path file-name-nondirectory (funcall treemacs-file-name-transformer))
                  'button '(t)
                  'category 'default-button
                  'help-echo nil
                  'keymap nil
                  :default-face 'treemacs-git-unmodified-face
                  :state 'file-node-closed
                  :path ,path
                  :key ,path
                  :parent ,parent
                  :depth ,depth)))))

;; TODO document open-action return strings
(cl-defmacro treemacs--button-open (&key button new-state new-icon open-action post-open-action immediate-insert)
  "Building block macro to open a BUTTON.
Gives the button a NEW-STATE, and, optionally, a NEW-ICON.  Performs OPEN-ACTION
and, optionally, POST-OPEN-ACTION.  If IMMEDIATE-INSERT is non-nil it will
concat and apply `insert' on the items returned from OPEN-ACTION.  If it is nil
either OPEN-ACTION or POST-OPEN-ACTION are expected to take over insertion."
  `(prog1
     (save-excursion
       (let ((p (point))
             lines)
         (treemacs-with-writable-buffer
          (treemacs-button-put ,button :state ,new-state)
          ,@(when new-icon
              `((beginning-of-line)
                (treemacs--button-symbol-switch ,new-icon)))
          (goto-char (treemacs-button-end ,button))
          ,@(if immediate-insert
                `((progn
                    (insert (apply #'concat ,open-action))))
              `(,open-action))
          (setf lines (count-lines p (point)))
          ,post-open-action
          lines)))
     (when treemacs-move-forward-on-expand
       (let* ((parent (treemacs-current-button))
              (child (next-button parent)))
         (when (equal parent (treemacs-button-get child :parent))
           (forward-line 1))))))

(cl-defmacro treemacs--create-buttons (&key nodes node-action  depth extra-vars node-name)
  "Building block macro for creating buttons from a list of NODES.
Will not making any insertions, but instead return a list of strings created by
NODE-ACTION, so that the list can be further manipulated and efficiently
inserted in one go.
NODES is the list to create buttons from.
DEPTH is the indentation level buttons will be created on.
EXTRA-VARS are additional var bindings inserted into the initial let block.
NODE-ACTION is the button creating form inserted for every NODE.
NODE-NAME is the variable individual nodes are bound to in NODE-ACTION."
  `(let* ((depth ,depth)
          (prefix (concat "\n" (treemacs--get-indentation depth)))
          (,node-name (car ,nodes))
          (strings)
          ,@extra-vars)
     ;; extensions only implicitly use the prefix by calling into `treemacs-render-node'
     ;; (ignore prefix)
     (when ,node-name
       (dolist (,node-name ,nodes)
         (--each ,node-action
           (push it strings))))
     (nreverse strings)))

(defun treemacs--flatten-dirs (dirs)
  "Display DIRS as flattened.
Go to each dir button, expand its label with the collapsed dirs, set its new
path and give it a special parent-path property so opening it will add the
correct cache entries.

DIRS: List of Collapse Paths.  Each Collapse Path is a list of
 1) the extra text that must be appended in the view,
 2) The original full and un-collapsed path,
 3) a series of intermediate steps which are the result of appending the
    collapsed path elements onto the original, ending in
 4) the full path to the
    directory that the collapsing leads to.  For Example:
    (\"/26.0/elpa\"
     \"/home/a/Documents/git/treemacs/.cask\"
     \"/home/a/Documents/git/treemacs/.cask/26.0\"
     \"/home/a/Documents/git/treemacs/.cask/26.0/elpa\")"
  (when dirs
    (-let [project (-> dirs (car) (cadr) (treemacs--find-project-for-path))]
      (dolist (it dirs)
        (let* ((label-to-add (car it))
               (original-path (cadr it))
               (extra-steps (cddr it))
               (new-path (-last-item extra-steps))
               (coll-count (length extra-steps)))
          ;; use when-let because the operation may fail when we try to move to a node
          ;; that us not visible because treemacs ignores it
          (-when-let (b (treemacs-find-file-node original-path project))
            ;; no warning since filewatch mode is known to be defined
            (when (with-no-warnings treemacs-filewatch-mode)
              (treemacs--start-watching original-path)
              (dolist (step extra-steps)
                (treemacs--start-watching step t)))
            ;; make extra dom entries for the flattened steps
            (-let [dom-node (treemacs-find-in-dom original-path)]
              (dolist (step extra-steps)
                (ht-set! treemacs-dom step dom-node))
              (setf (treemacs-dom-node->collapse-keys dom-node) extra-steps))
            (-let [props (text-properties-at (treemacs-button-start b))]
              (treemacs-button-put b :path new-path)
              ;; if the collapsed path leads to a symlinked directory the button needs to be marked as a symlink
              ;; so `treemacs--expand-dir-node' will know to start a new git future under its true-name
              (treemacs-button-put b :symlink (or (treemacs-button-get b :symlink)
                                                  (--first (file-symlink-p it) extra-steps)))
              ;; number of directories that have been appended to the original path plus all extra steps
              ;; to use as dom keys when the node is expanded
              (treemacs-button-put b :collapsed (cons coll-count (cons original-path extra-steps)))
              (end-of-line)
              (-let [beg (point)]
                (insert label-to-add)
                (add-text-properties beg (point) props)
                (unless (memq treemacs--git-mode '(deferred extended))
                  (add-text-properties
                   beg (point)
                   '(face treemacs-directory-collapsed-face)))))))))))

(defmacro treemacs--inplace-map-when-unrolled (items interval &rest mapper)
  "Unrolled in-place mapping operation.
Maps ITEMS at given index INTERVAL using MAPPER function."
  (declare (indent 2))
  (let ((l (make-symbol "list"))
        (tail-op (cl-case interval
                   (2 'cdr)
                   (3 'cddr)
                   (4 'cdddr)
                   (_ (error "Interval %s is not handled yet" interval)))))
    `(let ((,l ,items))
       (while ,l
          (setq ,l (,tail-op ,l))
          (let ((it (car ,l)))
            (setf (car ,l) ,@mapper)
            (pop ,l)))
       ,items)))

(define-inline treemacs--create-branch (root depth git-future collapse-process &optional parent)
  "Create a new treemacs branch under ROOT.
The branch is indented at DEPTH and uses the eventual outputs of
GIT-FUTURE to decide on file buttons' faces and COLLAPSE-PROCESS to determine
which directories should be displayed as one.  The buttons' parent property is
set to PARENT."
  (inline-letevals (root depth git-future collapse-process parent)
    (inline-quote
     (save-excursion
       (let* ((dirs-and-files (treemacs--get-dir-content ,root))
              (dirs (car dirs-and-files))
              (files (cadr dirs-and-files))
              (parent-node (treemacs-find-in-dom ,root))
              (dir-dom-nodes)
              (file-dom-nodes)
              (git-info)
              (file-strings)
              (dir-strings))
         (setq dir-strings
               (treemacs--create-buttons
                :nodes dirs
                :extra-vars ((dir-prefix (concat prefix treemacs-icon-dir-closed)))
                :depth ,depth
                :node-name node
                :node-action (treemacs--create-dir-button-strings node dir-prefix ,parent ,depth)))
         (setq file-strings
               (treemacs--create-buttons
                :nodes files
                :depth ,depth
                :node-name node
                :node-action (treemacs--create-file-button-strings node prefix ,parent ,depth)))

         (end-of-line)

         ;; the files list contains 3 item tuples: the prefix the icon and the filename
         ;; direcories are different, since dirs do not  have different icons the icon is part if the prefix
         ;; therefore when filtering or propertizing the files and dirs only every 3rd or 2nd item must be looked at

         ;; as reopening is done recursively the parsed git status is passed down to subsequent calls
         ;; so there are two possibilities: either the future given to this function is a pfuture object
         ;; that needs to complete and be parsed or it's an already finished git status hash table
         ;; additionally when git mode is deferred we don't parse the git output right here, it is instead done later
         ;; by means of an idle timer. The git info used is instead fetched from `treemacs--git-cache', which is
         ;; based on previous invocations
         ;; if git-mode is disabled there is nothing to do - in this case the git status parse function will always
         ;; produce an empty hash table
         (pcase treemacs--git-mode
           ((or 'simple 'extended)
            (setf git-info (treemacs--get-or-parse-git-result ,git-future))
            (ht-set! treemacs--git-cache ,root git-info))
           ('deferred
             (setf git-info (or (ht-get treemacs--git-cache ,root) treemacs--empty-table)))
           (_
            (setf git-info treemacs--empty-table)))

         (run-with-timer
          0.5 nil
          #'treemacs--apply-annotations-deferred
          ,parent ,root (current-buffer) ,git-future)

         (if treemacs-pre-file-insert-predicates
             (progn
               (-let [result nil]
                 (while file-strings
                   (let* ((prefix (car file-strings))
                          (icon (cadr file-strings))
                          (filename (caddr file-strings))
                          (filepath (concat ,root "/" filename)))
                     (unless (--any? (funcall it filepath git-info) treemacs-pre-file-insert-predicates)
                       (setq result (cons filename (cons icon (cons prefix result))))
                       (push (treemacs-dom-node->create! :parent parent-node :key filepath)
                             file-dom-nodes)))
                   (setq file-strings (cdddr file-strings)))
                 (setq file-strings (nreverse result)))
               (-let [result nil]
                 (while dir-strings
                   (let* ((prefix (car dir-strings))
                          (dirname (cadr dir-strings))
                          (dirpath (concat ,root "/" dirname)))
                     (unless (--any? (funcall it dirpath git-info) treemacs-pre-file-insert-predicates)
                       (setq result (cons dirname (cons prefix result)))
                       (push (treemacs-dom-node->create! :parent parent-node :key dirpath)
                             dir-dom-nodes)))
                   (setq dir-strings (cddr dir-strings)))
                 (setq dir-strings (nreverse result))))
           (setf
            file-dom-nodes
            (--map (treemacs-dom-node->create! :parent parent-node :key it) files)
            dir-dom-nodes
            (--map (treemacs-dom-node->create! :parent parent-node :key it) dirs)))

         ;; do nodes can only be created *after* any potential fitering has taken place,
         ;; otherwise we end up with dom entries for files that are not rendered
         (setf (treemacs-dom-node->children parent-node)
               (nconc dir-dom-nodes file-dom-nodes (treemacs-dom-node->children parent-node)))
         (dolist (it (treemacs-dom-node->children parent-node))
           (treemacs-dom-node->insert-into-dom! it))

         (treemacs--inplace-map-when-unrolled dir-strings 2
           (-if-let (ann (treemacs-get-annotation (concat ,root "/" it)))
               (progn
                 (put-text-property
                  0
                  (length it)
                  'face
                  (treemacs-annotation->face-value ann)
                  it)
                 (concat it (treemacs-annotation->suffix-value ann)))
             (put-text-property
              0
              (length it)
              'face
              'treemacs-directory-face
              it)
             it))
         (insert (apply #'concat dir-strings))

         (end-of-line)
         (setf file-strings
               (treemacs--inplace-map-when-unrolled file-strings 3
                 (-if-let (ann (treemacs-get-annotation (concat ,root "/" it)))
                     (progn
                       (put-text-property
                        0
                        (length it)
                        'face
                        (treemacs-annotation->face-value ann)
                        it)
                       (concat it (treemacs-annotation->suffix-value ann)))
                   (put-text-property
                    0
                    (length it)
                    'face
                    'treemacs-git-unmodified-face
                    it)
                   it)))
         (insert (apply #'concat file-strings))

         (save-excursion
           (treemacs--flatten-dirs (treemacs--parse-collapsed-dirs ,collapse-process))
           (treemacs--reentry ,root ,git-future))
         (point-at-eol))))))

(cl-defmacro treemacs--button-close (&key button new-icon new-state post-close-action)
  "Close node given by BUTTON, use NEW-ICON and BUTTON's state to NEW-STATE.
Run POST-CLOSE-ACTION after everything else is done."
  `(save-excursion
     (treemacs-with-writable-buffer
      ,@(when new-icon
          `((treemacs--button-symbol-switch ,new-icon)))
      (treemacs-button-put ,button :state ,new-state)
      (-let [next (next-button (button-end ,button))]
        (if (or (null next)
                (/= (1+ (treemacs-button-get ,button :depth))
                    (treemacs-button-get (copy-marker next t) :depth)))
            (delete-trailing-whitespace)
          ;; Delete from end of the current button to end of the last sub-button.
          ;; This will make the EOL of the last button become the EOL of the
          ;; current button, making the treemacs--projects-end marker track
          ;; properly when collapsing the last project or a last directory of the
          ;; last project.
          (let* ((pos-start (point-at-eol))
                 (next (treemacs--next-non-child-button ,button))
                 (pos-end (if next
                              (-> next (treemacs-button-start) (previous-button) (treemacs-button-end))
                            (point-max))))
            (delete-region pos-start pos-end))))
      ,post-close-action)))

(defun treemacs--expand-root-node (btn &optional recursive)
  "Expand the given root BTN.
Open every child-directory as well when RECURSIVE is non-nil.

BTN: Button
RECURSIVE: Bool"
  (let ((project (treemacs-button-get btn :project)))
    (treemacs-with-writable-buffer
     (treemacs-project->refresh-path-status! project))
    (if (treemacs-project->is-unreadable? project)
        (treemacs-pulse-on-failure
            (format "%s is not readable."
                    (propertize (treemacs-project->path project) 'face 'font-lock-string-face)))
      (let* ((path (treemacs-button-get btn :path))
             (git-path (if (treemacs-button-get btn :symlink) (file-truename path) path))
             (git-future (treemacs--git-status-process git-path project))
             (collapse-future (treemacs--collapsed-dirs-process path project)))
        (treemacs--maybe-recenter treemacs-recenter-after-project-expand
          (treemacs--button-open
           :immediate-insert nil
           :button btn
           :new-state 'root-node-open
           :new-icon treemacs-icon-root-open
           :open-action
           (progn
             ;; TODO(2019/10/14): go back to post open
             ;; expand first because it creates a dom node entry
             (treemacs-on-expand path btn)
             (treemacs--apply-project-top-extensions btn project)
             (goto-char (treemacs--create-branch path (1+ (treemacs-button-get btn :depth)) git-future collapse-future btn))
             (treemacs--apply-project-bottom-extensions btn project)
             (treemacs--start-watching path)
             ;; Performing FS ops on a disconnected Tramp project
             ;; might have changed the state to connected.
             (treemacs-with-writable-buffer
              (treemacs-project->refresh-path-status! project))
             (when (and recursive (treemacs-project->is-readable? project))
               (--each (treemacs-collect-child-nodes btn)
                 (when (eq 'dir-node-closed (treemacs-button-get it :state))
                   (goto-char (treemacs-button-start it))
                   (treemacs--expand-dir-node it :git-future git-future :recursive t)))))))))))

(defun treemacs--collapse-root-node (btn &optional recursive)
  "Collapse the given root BTN.
Remove all open entries below BTN when RECURSIVE is non-nil."
  (treemacs--button-close
   :button btn
   :new-state 'root-node-closed
   :new-icon treemacs-icon-root-closed
   :post-close-action
   (-let [path (treemacs-button-get btn :path)]
     (treemacs--stop-watching path)
     (treemacs-on-collapse path recursive))))

(cl-defun treemacs--expand-dir-node (btn &key git-future recursive)
  "Open the node given by BTN.

BTN: Button
GIT-FUTURE: Pfuture|HashMap
RECURSIVE: Bool"
  (-let [path (treemacs-button-get btn :path)]
    (if (not (file-readable-p path))
        (treemacs-pulse-on-failure
            "Directory %s is not readable." (propertize path 'face 'font-lock-string-face))
      (let* ((project (treemacs-project-of-node btn))
             (git-future (if (treemacs-button-get btn :symlink)
                             (treemacs--git-status-process (file-truename path) project)
                           (or git-future (treemacs--git-status-process path project))))
             (collapse-future (treemacs--collapsed-dirs-process path project)))
        (treemacs--button-open
         :immediate-insert nil
         :button btn
         :new-state 'dir-node-open
         :new-icon treemacs-icon-dir-open
         :open-action
         (progn
           ;; do on-expand first so buttons that need collapsing can quickly find their parent
           (treemacs-on-expand path btn)
           (treemacs--apply-directory-top-extensions btn path)
           (goto-char (treemacs--create-branch path (1+ (treemacs-button-get btn :depth)) git-future collapse-future btn))
           (treemacs--apply-directory-bottom-extensions btn path)
           (treemacs--start-watching path)
           (when recursive
             (--each (treemacs-collect-child-nodes btn)
               (when (eq 'dir-node-closed (treemacs-button-get it :state))
                 (goto-char (treemacs-button-start it))
                 (treemacs--expand-dir-node it :git-future git-future :recursive t))))))))))

(defun treemacs--collapse-dir-node (btn &optional recursive)
  "Close node given by BTN.
Remove all open dir and tag entries under BTN when RECURSIVE."
  (treemacs--button-close
   :button btn
   :new-state 'dir-node-closed
   :new-icon treemacs-icon-dir-closed
   :post-close-action
   (-let [path (treemacs-button-get btn :path)]
     (treemacs--stop-watching path)
     (treemacs-on-collapse path recursive))))

(defun treemacs--root-face (project)
  "Get the face to be used for PROJECT."
  (cl-case (treemacs-project->path-status project)
    (local-unreadable 'treemacs-root-unreadable-face)
    (remote-readable 'treemacs-root-remote-face)
    (remote-disconnected 'treemacs-root-remote-disconnected-face)
    (remote-unreadable 'treemacs-root-remote-unreadable-face)
    (otherwise 'treemacs-root-face)))

(defun treemacs--add-root-element (project)
  "Insert a new root node for the given PROJECT node.

PROJECT: Project Struct"
  (insert treemacs-icon-root-closed)
  (let* ((pos (point-marker))
         (path (treemacs-project->path project))
         (dom-node (treemacs-dom-node->create! :key path :position pos)))
    (treemacs-dom-node->insert-into-dom! dom-node)
    (treemacs--set-project-position project pos)
    (insert
     (propertize (treemacs-project->name project)
                 'button '(t)
                 'category 'default-button
                 'face (treemacs--root-face project)
                 :project project
                 :default-face 'treemacs-root-face
                 :key path
                 :symlink (when (treemacs-project->is-readable? project)
                            (file-symlink-p path))
                 :state 'root-node-closed
                 :path path
                 :depth 0))))

(defun treemacs--render-projects (projects)
  "Actually render the given PROJECTS in the current buffer."
  (treemacs-with-writable-buffer
   (unless treemacs--projects-end
     (setq treemacs--projects-end (make-marker)))
   (let* ((projects (-reject #'treemacs-project->is-disabled? projects))
          (current-workspace (treemacs-current-workspace))
          (has-previous (treemacs--apply-root-top-extensions current-workspace)))

     (--each projects
       (when has-previous (treemacs--insert-root-separator))
       (setq has-previous t)
       (treemacs--add-root-element it))

     ;; Set the end marker after inserting the extensions. Otherwise, the
     ;; extensions would move the marker.
     (let ((projects-end-point (point)))
       (treemacs--apply-root-bottom-extensions current-workspace has-previous)
       ;; If the marker lies at the start of the buffer, expanding extensions would
       ;; move the marker. Make sure that the marker does not move when doing so.
       (set-marker-insertion-type treemacs--projects-end has-previous)
       (set-marker treemacs--projects-end projects-end-point)))))

(define-inline treemacs-do-update-node (path &optional force-expand)
  "Update the node identified by its PATH.
Throws an error when the node cannot be found.  Does nothing if the node is not
expanded, unless FORCE-EXPAND is non-nil, in which case the node will be
expanded.
Same as `treemacs-update-node', but does not take care to either save
position or assure hl-line highlighting, so it should be used when making
multiple updates.

PATH: Node Path
FORCE-EXPAND: Boolean"
  (inline-letevals (path force-expand)
    (inline-quote
     (treemacs-without-recenter
      (-if-let (btn (if ,force-expand
                        (treemacs-goto-node ,path)
                      (-some-> (treemacs-find-visible-node ,path)
                        (goto-char))))
          (if (treemacs-is-node-expanded? btn)
              (-let [close-func (alist-get (treemacs-button-get btn :state) treemacs-TAB-actions-config)]
                (funcall close-func)
                ;; close node again if no new lines were rendered
                (when (eq 1 (funcall (alist-get (treemacs-button-get btn :state) treemacs-TAB-actions-config)))
                  (funcall close-func)))
            (when ,force-expand
              (funcall (alist-get (treemacs-button-get btn :state) treemacs-TAB-actions-config))))
        (-when-let (dom-node (treemacs-find-in-dom ,path))
          (setf (treemacs-dom-node->refresh-flag dom-node) t)))))))

(defun treemacs-update-node (path &optional force-expand)
  "Update the node identified by its PATH.
Same as `treemacs-do-update-node', but wraps the call in
`treemacs-save-position'.

PATH: Node Path
FORCE-EXPAND: Boolean"
  (treemacs-save-position
   (treemacs-do-update-node path force-expand)))

(defun treemacs-delete-single-node (path &optional project)
  "Delete single node at given PATH and PROJECT.
Does nothing when the given node is not visible.  Must be run in a treemacs
buffer.

This will also take care of all the necessary house-keeping like making sure
child nodes are deleted as well and everything is removed from the dom.

If multiple nodes are to be deleted it is more efficient to make multiple calls
to `treemacs-do-delete-single-node' wrapped in `treemacs-save-position' instead.

PATH: Node Path
Project: Project Struct"
  (treemacs-save-position
   (treemacs-do-delete-single-node path project)
   (hl-line-highlight)))

(defun treemacs-do-delete-single-node (path &optional project)
  "Actual implementation of single node deletion.
Will delete node at given PATH and PROJECT.  See also
`treemacs-delete-single-node'.

PATH: Node Path
Project: Project Struct"
  (-when-let (dom-node (treemacs-find-in-dom path))
    (-let [btn (or (treemacs-dom-node->position dom-node)
                   (treemacs-goto-node path project :ignore-file-exists))]
      (goto-char btn)
      (when (treemacs-is-node-expanded? btn)
        (treemacs-TAB-action :purge))
      (treemacs-with-writable-buffer
       (if (treemacs-button-get btn :collapsed)
           (treemacs--delete-at-flattened-path btn path dom-node)
         (treemacs--delete-line)
         (treemacs-dom-node->remove-from-dom! dom-node))))))

(defun treemacs--delete-at-flattened-path (btn deleted-path dom-node)
  "Handle a delete for a flattened path BTN for given DELETED-PATH.
Remove DOM-NODE from the dom if the entire line was deleted.

Btn: Button
DELETED-PATH: File Path
DOM-NODE: Dom Node"
  (let* ((key (treemacs-button-get btn :key))
         (coll-status (treemacs-button-get btn :collapsed))
         (curr-collapse-steps (cdr coll-status)))
    (if (string= deleted-path key)
        (progn
          ;; remove full dom entry if entire line was deleted
          (treemacs--delete-line)
          (treemacs-dom-node->remove-from-dom! dom-node))
      ;; otherwise change the current line and update its properties
      (let* ((path (treemacs-button-get btn :path))
             (new-path (treemacs--parent deleted-path))
             (delete-offset (- (length path) (length new-path)))
             (new-label (substring new-path (length key)))
             (old-coll-count (car coll-status))
             (new-coll-count (length (treemacs-split-path new-label))))
        (treemacs-button-put btn :path new-path)
        (end-of-line)
        ;; delete just enough to get rid of the deleted dirs
        (delete-region (- (point) delete-offset) (point))
        ;; then remove the deleted directories from the dom
        (-let [removed-collapse-keys (last curr-collapse-steps (- old-coll-count new-coll-count))]
          (treemacs-dom-node->remove-collapse-keys! dom-node removed-collapse-keys)
          (-each removed-collapse-keys #'treemacs--stop-watching))
        ;; and update inline collpase info
        (if (= 0 new-coll-count)
            (treemacs-button-put btn :collapsed nil)
          (treemacs-button-put
           btn :collapsed
           (cons new-coll-count (-take (1+ new-coll-count) curr-collapse-steps))))))))

(defun treemacs--determine-insert-position (path parent-btn sort-function)
  "Determine the insert location for PATH under PARENT-BTN.
Specifically this will return the node *after* which to make the new insert.

Mostly this means the position before the first node for whose path returns
SORT-FUNCTION returns non-nil, but files and directories must be handled
properly,and edge cases for inserting at the end of the project and buffer must
be taken into account.

PATH: File Path
PARENT-BTN: Button
SORT-FUNCTION: Button -> Boolean."
  (let* ((parent-dom-node (treemacs-find-in-dom (treemacs-button-get parent-btn :path)))
         (children (treemacs-dom-node->children parent-dom-node))
         (dirs-files (--separate (-let [path (treemacs-dom-node->key it)]
                                   (and (stringp path) (file-directory-p path)))
                                 children))
         (dirs (sort (car dirs-files) (lambda (d1 d2)
                                        (funcall sort-function
                                                 (treemacs-dom-node->key d1)
                                                 (treemacs-dom-node->key d2)))))
         (files (sort (cadr dirs-files) (lambda (f1 f2)
                                        (funcall sort-function
                                                 (treemacs-dom-node->key f1)
                                                 (treemacs-dom-node->key f2))))))
    (if (file-directory-p path)
        ;; insert directory ...
        (or
         ;; at first dir that fits sort order
         (--when-let (--first (funcall sort-function path (treemacs-dom-node->key it)) dirs)
           (previous-button (or (treemacs-dom-node->position it)
                                (treemacs-find-file-node (treemacs-dom-node->key it)))))
         ;; after last dir
         (--when-let (-last-item dirs)
           (or (treemacs-dom-node->position it)
               (treemacs-find-file-node (treemacs-dom-node->key it))))
         ;; before first file
         (--when-let (car files)
           (previous-button (or (treemacs-dom-node->position it)
                                (treemacs-find-file-node (treemacs-dom-node->key it)))))
         ;; after parent
         parent-btn)

      ;; insert file ...
      (or
       ;; at first file that fits sort order
       (--when-let (--first (funcall sort-function path (treemacs-dom-node->key it)) files)
         (previous-button (or (treemacs-dom-node->position it)
                              (treemacs-find-file-node (treemacs-dom-node->key it)))) )
       ;; after last file
       (--when-let (-last-item files)
         (or (treemacs-dom-node->position it)
             (treemacs-find-file-node (treemacs-dom-node->key it))) )
       ;; after last dir
       (--when-let (-last-item dirs)
         (or (treemacs-dom-node->position it)
             (treemacs-find-file-node (treemacs-dom-node->key it))))
       ;; after parent
       parent-btn))))

(defun treemacs-do-insert-single-node (path parent-path)
  "Insert single file node at given PATH and below PARENT-PATH.

PATH: File Path
PARENT-PATH: File Path"
  (-when-let (parent-dom-node (treemacs-find-in-dom parent-path))
    (if (treemacs-find-in-dom path)
        ;; "creating" a file that is already present may happen due to an interaction in magit
        ;; in that case we need to checkthe file's git status
        (treemacs-update-single-file-git-state path)
      (let* ((parent-btn (treemacs-dom-node->position parent-dom-node))
             (parent-flatten-info (treemacs-button-get parent-btn :collapsed)))
        (treemacs-with-writable-buffer
         (if parent-flatten-info
             (treemacs--insert-node-in-flattened-directory
              path parent-btn parent-dom-node parent-flatten-info)
           (treemacs--insert-single-node
            path parent-btn parent-dom-node)))))))

(defun treemacs--insert-single-node (created-path parent-btn parent-dom-node)
  "Insert new CREATED-PATH below non-flattened directory at PARENT-BTN.
Will find the correct insert location, insert the necessary strings, and make
the necessary dom entries and adjust PARENT-DOM-NODE."
  (let* ((sort-function (treemacs--get-sort-fuction))
         (insert-after (treemacs--determine-insert-position created-path parent-btn sort-function)))
    (goto-char insert-after)
    (end-of-line)
    (insert "\n" (treemacs--create-string-for-single-insert
                  created-path parent-btn (1+ (button-get parent-btn :depth))))
    (-let [new-dom-node (treemacs-dom-node->create! :key created-path :parent parent-dom-node)]
      (treemacs-dom-node->insert-into-dom! new-dom-node)
      (treemacs-dom-node->add-child! parent-dom-node new-dom-node))
    (when treemacs-git-mode
      (treemacs-do-update-single-file-git-state created-path :exclude-parents :override-status))))

(defun treemacs--insert-node-in-flattened-directory (created-path parent-btn parent-dom-node flatten-info)
  "Insert new CREATED-PATH below flattened directory at PARENT-BTN.

Will take care of every part necessary for adding a new node under a flattened
directory - adjusting the label, the state PARENT-DOM-NODE, the FLATTEN-INFO and
path text properties, the filewatch entries.  It will also differentiate between
creating new files and new directories and re-open the node accordingly.

PATH: File Path
PARENT-BTN: Button
PARENT-DOM-NODE: Dom Node Struct
FLATTEN-INFO [Int File Path...]"

  (treemacs-block
   (let ((is-file? (file-regular-p created-path))
         (insert-at-end? (treemacs-is-path created-path :in (-last-item flatten-info)))
         (is-expanded? (treemacs-is-node-expanded? parent-btn)))

     ;; Simple addition of a file
     (treemacs-return-if (and is-file? insert-at-end? is-expanded?)
       (treemacs--insert-single-node created-path parent-btn parent-dom-node))

     ;; Simple file addition at the end, but the node is collapsed so we do nothing
     (treemacs-return-if (and is-file? insert-at-end? (not is-expanded?))
       t)

     (let* ((properties (text-properties-at parent-btn))
            (current-base-path (treemacs-button-get parent-btn :key))
            ;; In case we either add a new file or a directory somewhere in the middle of the flattened paths
            ;; we move the `created-path' up a step because that means we do not simple add another directory to
            ;; the flattened path. Instead we remove everything *up to* the directory the new item was created in.
            ;; Pretending the `created-path' has moved up like is an easy way to make sure the new button label
            ;; and properties are determined correctly.
            (created-path (if (or is-file? (not insert-at-end?))
                              (treemacs--parent-dir created-path)
                            created-path))
            (new-path-tokens (treemacs--tokenize-path created-path current-base-path))
            (new-button-label (substring created-path (1+ (length (treemacs--parent-dir current-base-path)))))
            ;; TODO(2020/10/02): Check again when exactly this count is actually used
            ;; maybe it can be removed by now
            (new-flatten-info-count 0)
            (new-flatten-info (list current-base-path))
            (new-flatten-info-item current-base-path))

       ;; Do nothing if we add a new directory and we have already reached maximum length
       (unless (and insert-at-end?
                    (>= (car flatten-info) treemacs-collapse-dirs)
                    (not is-file?))

         ;; Create the path items of the new `:collapsed' property
         (dolist (token new-path-tokens)
           (cl-incf new-flatten-info-count)
           (setf new-flatten-info-item (treemacs-join-path new-flatten-info-item token))
           (push new-flatten-info-item new-flatten-info))
         (setf new-flatten-info (nreverse new-flatten-info))

         ;; Take care of filewatch and dom entries for all paths added and removed
         (let* ((old-flatten-paths (-difference (cdr flatten-info) new-flatten-info))
                (new-flatten-paths (-difference new-flatten-info (cdr flatten-info))))
           (dolist  (old-flatten-path old-flatten-paths)
             (treemacs--stop-watching old-flatten-path)
             (ht-set! treemacs-dom old-flatten-path nil))
           (dolist (new-flatten-path new-flatten-paths)
             (treemacs--start-watching new-flatten-path :flatten)
             (ht-set! treemacs-dom new-flatten-path parent-dom-node))
           (setf (treemacs-dom-node->collapse-keys parent-dom-node) (copy-sequence (cdr new-flatten-info))))

         ;; Update text properties with new state
         (setf new-flatten-info (when (> new-flatten-info-count 0)
                                  (cons new-flatten-info-count new-flatten-info)))
         (plist-put properties :collapsed new-flatten-info)
         (plist-put properties :path created-path)

         ;; Insert new label
         (goto-char parent-btn)
         (delete-region (point) (point-at-eol))
         (insert (apply #'propertize new-button-label properties))

         ;; Fixing marker probably necessary since it's also in the dom
         (goto-char (- (point) (length new-button-label)))
         (set-marker parent-btn (point))

         (if (and insert-at-end? is-file?)


             ;; TODO(2020/10/01): this reopening is used multiple tims like this
             ;; it should be abstracted properly
             (funcall (alist-get (treemacs-button-get parent-btn :state) treemacs-TAB-actions-config))
           (funcall (alist-get (treemacs-button-get parent-btn :state) treemacs-TAB-actions-config))
           (setf (treemacs-dom-node->refresh-flag parent-dom-node) nil)))))))

(define-inline treemacs--create-string-for-single-insert (path parent depth)
  "Create the necessary strings to insert a new file node.
Creates the required indent prefix and file icon based on the given file PATH,
PARENT node and node DEPTH.

PATH: File Path
PARENT: Button
DEPTH: Int"
  (declare (side-effect-free t))
  (inline-letevals (path depth parent)
    (inline-quote
     (let ((prefix (treemacs--get-indentation ,depth)))
       (apply
        #'concat
        (let* ((strs)
               (face))
          (if (file-directory-p ,path)
              (setf strs (treemacs--create-dir-button-strings
                          ,path
                          (concat prefix treemacs-icon-dir-closed)
                          ,parent
                          ,depth)
                    face 'treemacs-directory-face)
            (setf strs (treemacs--create-file-button-strings ,path prefix ,parent ,depth)
                  face 'treemacs-file-face))
          (-let [last (-last-item strs)]
            (put-text-property 0 (length last) 'face face last))
          strs))))) )

(defun treemacs--maybe-recenter (when &optional new-lines)
  "Potentially recenter based on value of WHEN.

WHEN can take the following values:

 * always: Recenter indiscriminately,
 * on-distance: Recentering depends on the distance between `point' and the
   window top/bottom being smaller than `treemacs-recenter-distance'.
 * on-visibility: Special case for projects: recentering depends on whether the
   newly rendered number of NEW-LINES fits the view."
  (declare (indent 1))
  (when (and (null treemacs--no-recenter)
             (treemacs-is-treemacs-window? (selected-window)))
    (let* ((current-line (float (treemacs--current-screen-line)))
           (all-lines (float (treemacs--lines-in-window))))
      (pcase when
        ('always (recenter))
        ('on-visibility
         (-let [lines-left (- all-lines current-line)]
           (when (> new-lines lines-left)
             ;; if possible recenter only as much as is needed to bring all new lines
             ;; into view
             (recenter (max 0 (round (- current-line (- new-lines lines-left))))))))
        ('on-distance
         (let* ((distance-from-top (/ current-line all-lines))
                (distance-from-bottom (- 1.0 distance-from-top)))
           (when (or (> treemacs-recenter-distance distance-from-top)
                     (> treemacs-recenter-distance distance-from-bottom))
             (recenter))))))))

;; TODO(201/10/30): update of parents
(defun treemacs--recursive-refresh-descent (node project)
  "Recursively refresh by descending the dom starting from NODE.
If NODE under PROJECT is marked for refresh and in an open state (since it could
have been collapsed in the meantime) it will simply be collapsed and
re-expanded.  If NODE is node marked its children will be recursively
investigated instead.
Additionally all the refreshed nodes are collected and returned so their
parents' git status can be updated."
  (let ((recurse t)
        (refreshed-nodes nil))
    (-when-let (change-list (treemacs-dom-node->refresh-flag node))
      (setf (treemacs-dom-node->refresh-flag node) nil)
      (push node refreshed-nodes)
      (if (> (length change-list) 8)
          (progn
            (setf recurse nil)
            (if (null (treemacs-dom-node->parent node))
                (treemacs-project->refresh! project)
              (treemacs--refresh-dir (treemacs-dom-node->key node) project)))
        (dolist (change change-list)
          (-let [(path . type) change]
            (pcase type
              ('deleted
               (treemacs-do-delete-single-node path project))
              ('changed
               (treemacs-do-update-node path)
               (when (memq treemacs--git-mode '(extended deferred))
                 (treemacs-update-single-file-git-state path)))
              ('created
               (treemacs-do-insert-single-node path (treemacs-dom-node->key node)))
              ('force-refresh
               (setf recurse nil)
               (if (null (treemacs-dom-node->parent node))
                   (treemacs-project->refresh! project)
                 (treemacs--refresh-dir (treemacs-dom-node->key node) project)))
              (_
               ;; Renaming is handled as a combination of delete+create, so
               ;; this case should never be taken
               (treemacs-log-failure "Unknown change event: %s" change)
               (setf recurse nil)
               (if (null (treemacs-dom-node->parent node))
                   (treemacs-project->refresh! project)
                 (treemacs--refresh-dir (treemacs-dom-node->key node) project))))))))
    (when recurse
      (dolist (child (treemacs-dom-node->children node))
        (setq refreshed-nodes
              (nconc refreshed-nodes
                     (treemacs--recursive-refresh-descent child project)))))
    ;; TODO(2019/07/30): add as little as possible
    refreshed-nodes))

(define-inline treemacs--should-reenter? (path)
  "Check if PATH should not be reentered.
Returns nil if PATH is either not a file or it should be hidden on account of
`treemacs-show-hidden-files' being non-nil.

PATH: Node Path"
  (declare (side-effect-free t))
  (inline-letevals (path)
    (inline-quote
     (let ((path (cond
                  ((stringp ,path)
                   ,path)
                  ;; tags should be reopened also
                  ((and (consp ,path)
                        (stringp (car ,path)))
                   (car ,path)))))
       (if path
           (or treemacs-show-hidden-files
               (not (s-matches? treemacs-dotfiles-regex
                                (treemacs--filename path))))
         t)))))

(defun treemacs--reentry (path &optional git-info)
  "Reopen dirs below PATH.
GIT-INFO is passed through from the previous branch build.

PATH: Node Path
GIT-INFO: Pfuture | Map<String, String>"
  (-when-let* ((dom-node (treemacs-find-in-dom path))
               (reopen-list (treemacs-dom-node->reentry-nodes dom-node)))
    ;; get rid of the reentry-remnant so it wont pollute the actual dom
    (setf (treemacs-dom-node->reentry-nodes dom-node) nil)
    (dolist (to-reopen-dom-node reopen-list)
      ;; the dom-node in the reentry-remnant and the one currently in the dom
      ;; are different, we need to make sure the latter is present, otherwise
      ;; the file has since been deleted
      (let* ((reopen-path (treemacs-dom-node->key to-reopen-dom-node))
             (actual-dom-node (treemacs-find-in-dom reopen-path)))
        (when (and actual-dom-node
                   (treemacs--should-reenter? reopen-path))
          ;; move the next level of the reentry-remnant to the new reopened dom
          ;; so the process can continue
          (setf (treemacs-dom-node->reentry-nodes actual-dom-node)
                (treemacs-dom-node->reentry-nodes to-reopen-dom-node))
          (treemacs--reopen-node (treemacs-goto-node reopen-path) git-info))))))

(defun treemacs--reopen-node (btn &optional git-info)
  "Reopen file BTN.
GIT-INFO is passed through from the previous branch build."
  (pcase (treemacs-button-get btn :state)
    ('dir-node-closed  (treemacs--expand-dir-node btn :git-future git-info))
    ('file-node-closed (treemacs--expand-file-node btn))
    ('tag-node-closed  (treemacs--expand-tag-node btn))
    ('root-node-closed (treemacs--expand-root-node btn))
    (other             (funcall (alist-get other treemacs-TAB-actions-config) btn))))

(defun treemacs--show-single-project (path name)
  "Show only a project for the given PATH and NAME in the current workspace."
  (-let [ws (treemacs-current-workspace)]
    (if (treemacs-workspace->is-empty?)
        (progn
          (treemacs-do-add-project-to-workspace path name)
          (treemacs-select-window)
          (treemacs-pulse-on-success))
      (setf (treemacs-workspace->projects ws)
            (--filter (string= path (treemacs-project->path it))
                      (treemacs-workspace->projects ws)))
      (unless (treemacs-workspace->projects ws)
        (let ((treemacs--no-messages t)
              (treemacs-pulse-on-success nil))
          (treemacs-add-project-to-workspace path name)))
      (treemacs-select-window)
      (treemacs--consolidate-projects)
      (goto-char 2)
      (-let [btn (treemacs-current-button)]
        (unless (treemacs-is-node-expanded? btn)
          (treemacs--expand-root-node btn)))
      (treemacs-pulse-on-success))))

(provide 'treemacs-rendering)

;;; treemacs-rendering.el ends here
