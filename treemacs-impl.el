;;; treemacs.el --- A tree style file viewer package

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
;;; General implementation details.

;;; Code:

;;;;;;;;;;;;;;;;;;
;; Requirements ;;
;;;;;;;;;;;;;;;;;;

(require 'cl-lib)
(require 'dash)
(require 's)
(require 'f)
(require 'ace-window)
(require 'vc-hooks)
(require 'treemacs-customization)

(declare-function treemacs-mode "treemacs-mode")
(declare-function treemacs-refresh "treemacs-core")
(declare-function treemacs--follow "treemacs-follow-mode")
(declare-function treemacs-visit-file-vertical-split "treemacs-core")
(declare-function projectile-project-root "projectile")

;;;;;;;;;;;;;;;;;;
;; Private vars ;;
;;;;;;;;;;;;;;;;;;

(defvar treemacs--open-dirs-cache '()
  "Cache to keep track of opened subfolders.")

(defconst treemacs--buffer-name "Treemacs"
  "Name of the treemacs buffer.")

(defconst treemacs-dir
  (expand-file-name (if load-file-name
                        (file-name-directory load-file-name)
                      default-directory))
  "The directory treemacs.el is stored in.")

(defvar treemacs--in-gui (window-system)
  "Indicates whether Emacs is running in a gui or a terminal.")

(defvar treemacs--insert-image #'treemacs--insert-image-png)

(defvar treemacs--ready nil
  "Signals to `treemacs-follow-mode' if a follow action may be run.
Must be set to nil when no follow actions should be triggered, e.g. when the
treemacs buffer is being rebuilt or during treemacs' own window selection
functions.")

;;;;;;;;;;;;
;; Macros ;;
;;;;;;;;;;;;

(defmacro treemacs--setup-icon (var file-name &rest extensions)
  "Define a variable VAR with the value being the image created from FILE-NAME.
Insert VAR into icon-cache for each of the given file EXTENSIONS."
  `(progn
     (defconst ,var
       (create-image (f-join treemacs-dir "icons/" ,file-name)
                     'png nil :ascent 'center))
     (--each (quote ,extensions) (puthash it ,var treemacs-icons-hash))))

(defmacro treemacs--with-writable-buffer (&rest body)
  "Temporarily turn off read-ony mode to execute BODY."
  `(progn
     (read-only-mode -1)
     (unwind-protect
         (progn ,@body)
       (read-only-mode t))))

(defmacro treemacs--without-messages (&rest body)
  "Temporarily turn off messages to execute BODY."
  `(cl-flet ((message (_) (ignore)))
     ,@body))

;;;;;;;;;;;;;;;;;;;
;; Substitutions ;;
;;;;;;;;;;;;;;;;;;;

(defsubst treemacs--refresh-on-ui-change ()
  "Refresh the treemacs buffer when the window system has changed."
  (when (treemacs--check-window-system)
    (treemacs-refresh)))

(defsubst treemacs--reopen (abs-path git-info)
  "Reopen the node identified by its ABS-PATH.
Pass GIT-INFO along till it's needed."
  (treemacs--without-messages
   (treemacs--open-node (treemacs--goto-button-at abs-path) git-info t)))

(defsubst treemacs--add-to-cache (parent opened-child)
  "Add to PARENT's open dirs cache an entry for OPENED-CHILD."
  (let ((cache (assoc parent treemacs--open-dirs-cache)))
    (if cache
        (push opened-child (cdr cache))
      (add-to-list 'treemacs--open-dirs-cache `(,parent ,opened-child)))))

(defsubst treemacs--is-visible? ()
  "Inidicates whether the treemacs buffer is currently visible.
Will return the treemacs window if true."
  (get-buffer-window treemacs--buffer-name))

(defsubst treemacs--buffer-exists? ()
  "Indicates whether the treemacs buffer exists even if it is not visible."
  (-contains?
   (-map #'buffer-name (buffer-list))
   treemacs--buffer-name))

(defsubst treemacs--select-visible ()
  "Switch to treemacs buffer, given that it is currently visible."
  (select-window (get-buffer-window treemacs--buffer-name)))

(defsubst treemacs--select-not-visible ()
  "Switch to treemacs buffer, given that it not visible."
  (treemacs--setup-buffer)
  (switch-to-buffer treemacs--buffer-name))

(defsubst treemacs--is-dir-git-controlled? (path)
  "Check whether PATH is under git control."
  (let ((default-directory path))
    (->> "git rev-parse"
         (shell-command-to-string)
         (s-starts-with? "fatal")
         (not))))

(defsubst treemacs--unqote (str)
  "Unquote STR if it is wrapped in quotes."
  (if (s-starts-with? "\"" str)
      (replace-regexp-in-string "\"" "" str)
    str))

(defsubst treemacs--parse-git-status (path)
  "Use the git command line to parse the git states of the files at PATH.
Parsing only takes place if
1) `treemacs-git-integrtion' is t.
2) PATH is under git control."
  (let ((canonical-path (f-canonical path)))
    (when (and treemacs-git-integration
               (treemacs--is-dir-git-controlled? canonical-path))
      (let* ((default-directory canonical-path)
             (git-output (shell-command-to-string  "git status --ignored --porcelain"))
             ;; need the actual git root since git status outputs paths relative to it
             ;; and the output must be valid also for files in dirs being reopened
             (git-root (vc-call-backend 'Git 'root default-directory)))
        (if (s-blank? git-output) '()
          (let ((status
                 (->> (substring git-output 0 -1)
                      (s-split "\n")
                      (--map (s-split-up-to " " (s-trim it) 1)))))
            (--each status
              (setcdr it (->> (cl-second it) (s-trim-left) (treemacs--unqote) (f-join git-root))))
            status))))))

(defsubst treemacs--insert-node (path prefix depth parent &optional git-info)
 "Insert a single button node.
PATH is the node's absolute path.
PREFIX is an empty string used for indentation.
DEPTH is the nesting depth, used for calculating the prefix length
of all potential child branches.
PARENT is the node the new node is nested under, if any.
GIT-INFO is an alist mapping each file to its git state (no mapping meaning
the file is unchanged)."
  (end-of-line)
  (let ((is-dir? (f-directory? path)))
    (insert prefix)
    (funcall treemacs--insert-image path is-dir?)
    (insert-text-button (f-filename path)
                        'state     (if is-dir? 'dir-closed 'file)
                        'action    #'treemacs--push-button
                        'abs-path  path
                        'parent    parent
                        'depth     depth
                        'face      (treemacs--get-face path is-dir? git-info))))

(defsubst treemacs--prop-at-point (prop)
  "Grab property PROP of the button at point."
  (save-excursion
    (beginning-of-line)
    (button-get (next-button (point) t) prop)))

(defsubst treemacs--set-neighbours (buttons)
  "Set next- and previous-node properties for each button in BUTTONS."
  (when buttons
    (cl-dolist (i (number-sequence 0 (- (length buttons) 2)))
      (let ((b1 (nth i buttons))
            (b2 (nth (1+ i) buttons)))
        (button-put b1 'next-node b2)
        (button-put b2 'prev-node b1)))))

(defsubst treemacs--is-path-in-dir? (path dir)
  "Is PATH in directory DIR?"
  (s-starts-with? (f-slash dir) path))

(defsubst treemacs--current-root ()
  "Return the current root directory.
Requires and assumes to be called inside the treemacs buffer.

If both the root button and the root dir are needed it's more efficient to get
the root button and then grab its 'abs-path property."
  (f-long default-directory))

(defsubst treemacs--maybe-filter-dotfiles (dirs)
  "Remove from DIRS directories that shouldn't be reopened.
That is, directories (and their descendants) that are in the reopen cache, but
are not being shown on account of `treemacs-show-hidden-files' being nil."
  (if treemacs-show-hidden-files
      dirs
    (let ((root (treemacs--current-root)))
      (--filter (not (--any (s-matches? treemacs-dotfiles-regex it)
                            (f-split (substring it (length root)))))
                dirs))))

(defsubst treemacs--sort-alphabetic-asc (files)
  "Sort FILES alphabetically asc."
  (--sort (string> (cl-first it) (cl-first other)) files))

(defsubst treemacs--sort-alphabetic-desc (files)
  "Sort FILES alphabetically desc."
  (--sort (string< (cl-first it) (cl-first other)) files))

(defsubst treemacs--sort-size-asc (files)
  "Sort FILES by size asc."
  (--sort (> (file-attribute-size (cdr it)) (file-attribute-size (cdr other))) files))

(defsubst treemacs--sort-size-desc (files)
  "Sort FILES by size desc."
  (--sort (< (file-attribute-size (cdr it)) (file-attribute-size (cdr other))) files))

(defsubst treemacs--sort-mod-time-asc (files)
  "Sort FILES by modification time asc."
   (--sort (not (time-less-p (file-attribute-modification-time (cdr it)) (file-attribute-modification-time (cdr other)))) files))

(defsubst treemacs--sort-mod-time-desc (files)
  "Sort FILES by modification time desc."
  (--sort (time-less-p (file-attribute-modification-time (cdr it)) (file-attribute-modification-time (cdr other))) files))

(defsubst treemacs--reject-ignored-files (file)
  "Return t if FILE is *not* an ignored file.
FILE here is a list consisting of an absolute path and file attributes."
  (--none? (funcall it (f-filename (cl-first file))) treemacs-ignored-file-predicates))

(defsubst treemacs--reject-ignored-and-dotfiles (file)
  "Return t when FILE is neither ignored, nor a dotfile.
FILE here is a list consisting of an absolute path and file attributes."
  (let ((filename (f-filename (cl-first file))))
    (and (not (s-matches? treemacs-dotfiles-regex filename))
         (--none? (funcall it (f-filename filename)) treemacs-ignored-file-predicates))))


;;;;;;;;;;;;;;;
;; Functions ;;
;;;;;;;;;;;;;;;

(defun treemacs--init (root)
  "Initialize and build treemacs buffer for ROOT."
  (let ((origin-buffer (current-buffer)))
    (treemacs--buffer-teardown)
    (if (treemacs--is-visible?)
        (treemacs--select-visible)
      (progn
        (treemacs--setup-buffer)
        (switch-to-buffer (get-buffer-create treemacs--buffer-name))
        (bury-buffer treemacs--buffer-name)))
    ;; f-long to expand ~ and remove final slash
    ;; needed for root dirs given by projectile
    (treemacs--build-tree (f-long root))
    ;; do mode activation last - if the treemacs buffer is empty when the major
    ;; mode is activated (this may happen when treemacs is restored from other
    ;; than desktop save mode) treemacs will attempt to restore the previous session
    (treemacs-mode)
    (setq treemacs--ready t)
    ;; no warnings since follow mode is known to be defined
    (when (or treemacs-follow-after-init (with-no-warnings treemacs-follow-mode))
      (with-current-buffer origin-buffer
        (treemacs--follow)))))

(defun treemacs--check-window-system ()
  "Check if the window system has changed since the last call.
Make the necessary render function changes changes if so and explicitly
return t."
  (let ((current-ui (window-system)))
    (unless (eq current-ui treemacs--in-gui)
        (setq treemacs--in-gui current-ui)
        (if current-ui
            ;; icon variables are known to exist
            (with-no-warnings
              (setf treemacs--insert-image #'treemacs--insert-image-png)
              (setq treemacs-icon-closed treemacs-icon-closed-png
                    treemacs-icon-open   treemacs-icon-open-png))
          ;; icon variables are known to exist
          (with-no-warnings
            (setf treemacs--insert-image #'treemacs--insert-image-text)
            (setq treemacs-icon-closed treemacs-icon-closed-text
                  treemacs-icon-open   treemacs-icon-open-text)))
        t)))

(defun treemacs--build-tree (root)
  "Build the file tree starting at the given ROOT."
  (treemacs--check-window-system)
  (treemacs--with-writable-buffer
   (treemacs--delete-all)
   (treemacs--insert-header root)
   (treemacs--create-branch root 0 (treemacs--parse-git-status root))
   (goto-char 0)
   (forward-line 1)
   (treemacs--evade-image)))

(defun treemacs--delete-all ()
  "Delete all content of the buffer."
  (delete-region (point-min) (point-max)))

(defun treemacs--create-header (root)
  "Use ROOT's directory name as treemacs' header."
   (format "*%s*" (f-filename root)))

(defun treemacs--create-header-projectile (root)
  "Try to use the projectile project name for ROOT as treemacs' header.
If not projectile name was found call `treemacs--create-header' for ROOT instead."
  (-if-let (project-name
            (when (bound-and-true-p projectile-project-name-function)
              (condition-case nil
                  (projectile-project-root)
                (error nil))))
      ;; name function is known to be defined
      (format "*%s*" (funcall (with-no-warnings projectile-project-name-function) project-name))
    (treemacs--create-header root)))

(defun treemacs--insert-header (root)
  "Insert the header line for the given ROOT."
  (setq default-directory (f-full root))
  (insert-button (propertize (funcall treemacs-header-function root)
                             'face 'treemacs-header-face)
                 'face 'treemacs-header-face
                 'abs-path root
                 'action #'ignore))

(defun treemacs--get-face (path is-dir? git-info)
  "Return the appropriate face for PATH given IS-DIR? and GIT-INFO."
  (if is-dir? 'treemacs-directory-face
    (if treemacs-git-integration
        ;; for the sake of simplicity we only look at the state in the working tree
        ;; see OUTPUT section `git help status'
        (pcase (-some-> (rassoc path git-info) (car) (substring 0 1))
          ("M" 'treemacs-git-modified-face)
          ("?" 'treemacs-git-untracked-face)
          ("!" 'treemacs-git-ignored-face)
          ("A" 'treemacs-git-added-face)
          (_   'treemacs-git-unmodified-face))
      'treemacs-file-face)))

(defun treemacs--insert-image-png (path is-dir?)
  "Insert the appropriate png image for PATH given IS-DIR?."
  (insert-image
   ;; no warnings since both icons are known to be defined
   (if is-dir? (with-no-warnings treemacs-icon-closed)
     (gethash (-some-> path (file-name-extension) (downcase)) treemacs-icons-hash (with-no-warnings treemacs-icon-text))))
  (insert " "))

(defun treemacs--insert-image-text (_ is-dir?)
  "Insert the appropriate text image a path given IS-DIR?."
  ;; no warnings since the icon is known to be defined
  (when is-dir? (insert (with-no-warnings treemacs-icon-closed-text) " ")))

(defun treemacs--create-branch (root indent-depth git-info &optional parent)
  "Create a new treemacs branch under ROOT.
The branch is indented at INDENT-DEPTH and uses GIT-INFO to decide on file
nodes' faces. The nodes' parent property is set to PARENT."
  (save-excursion
    (let* ((prefix       (concat "\n" (make-string (* indent-depth treemacs-indentation) ?\ )))
           (entries      (treemacs--get-dir-content root))
           (directories  (cl-first entries))
           (files        (cl-second entries))
           (dir-buttons  (--map (-> (treemacs--insert-node it prefix indent-depth parent git-info) (button-at)) directories))
           (file-buttons (--map (-> (treemacs--insert-node it prefix indent-depth parent git-info) (button-at)) files))
           (last-dir     (-some-> (last dir-buttons) (car)))
           (first-file   (cl-first file-buttons)))
      (treemacs--set-neighbours dir-buttons)
      (treemacs--set-neighbours file-buttons)
      (when (and last-dir first-file)
        (button-put last-dir 'next-node first-file)
        (button-put first-file 'prev-node last-dir))
      ;; reopen here only since create-branch is called both when opening a node and
      ;; building the entire tree
      (treemacs--reopen-at root git-info))))

(defun treemacs--buffer-teardown ()
  "Cleanup to be run when the treemacs buffer gets killed."
  (setq treemacs--open-dirs-cache nil
        treemacs--ready nil))

(defun treemacs--push-button (btn)
  "Execute the appropriate action given the state of the BTN that has been pushed."
  (cl-case (button-get btn 'state)
    ('file       (treemacs-visit-file-vertical-split))
    ('dir-closed (treemacs--open-node btn))
    ('dir-open   (treemacs--close-node btn))))

(defun treemacs--open-node (btn &optional git-info no-add)
  "Open the node given by BTN.
Pass on GIT-INFO to set faces when `treemacs-git-integration' is t.
Do not reopen its previously open children when NO-ADD is given."
  (if (not (f-readable? (button-get btn 'abs-path)))
      (message "Directory is not readable.")
    (treemacs--with-writable-buffer
     (let ((abs-path (button-get btn 'abs-path)))
       (button-put btn 'state 'dir-open)
       (beginning-of-line)
       ;; icon is known to be defined
       (treemacs--node-symbol-switch (with-no-warnings treemacs-icon-open))
       (treemacs--create-branch abs-path (1+ (button-get btn 'depth)) (or git-info (treemacs--parse-git-status abs-path)) btn)
       (unless no-add (treemacs--add-to-cache (treemacs--parent abs-path) abs-path))))))

(defun treemacs--close-node (btn)
  "Close node given by BTN."
  (treemacs--with-writable-buffer
   ;; no warnings since icon is known to be defined
   (treemacs--node-symbol-switch (with-no-warnings treemacs-icon-closed))
   (treemacs--clear-from-cache (button-get btn 'abs-path))
   (end-of-line)
   (forward-button 1)
   (beginning-of-line)
   (let* ((pos-start (point))
          (next-node (treemacs--next-node btn))
          (pos-end   (if next-node
                         (-> next-node (button-start) (previous-button) (button-end) (1+))
                       (point-max))))
     (button-put btn 'state 'dir-closed)
     (delete-region pos-start pos-end)
     (delete-trailing-whitespace))))

(defun treemacs--open-file (&optional window split-func)
  "Visit file of the current node.  Split WINDOW using SPLIT-FUNC.
Do nothing if current node is a directory.
Do not split window if SPLIT-FUNC is nil.
Use `next-window' if WINDOW is nil."
  (let* ((path     (treemacs--prop-at-point 'abs-path))
         (is-file? (f-file? path)))
    (when is-file?
      (select-window (or window (next-window)))
      (when split-func
        (call-interactively split-func)
        (call-interactively 'other-window))
      (find-file path))))

(defun treemacs--node-symbol-switch (new-sym)
  "Replace icon in current line with NEW-SYM."
  (beginning-of-line)
  (forward-char (* treemacs-indentation (treemacs--prop-at-point 'depth)))
  (delete-char 1)
  (if (window-system)
      (insert-image new-sym)
    (insert new-sym)))

(defun treemacs--reopen-at (abs-path git-info)
  "Reopen dirs below ABS-PATH.
Pass GIT-INFO along till it's needed."
  (-some->
   abs-path
   (assoc treemacs--open-dirs-cache)
   (cdr)
   (treemacs--maybe-filter-dotfiles)
   (--each (treemacs--reopen it git-info))))

(defun treemacs--clear-from-cache (path &optional purge)
  "Remove PATH from the open dirs cache.
Also remove any dirs below if PURGE is given."
  (let* ((parent (treemacs--parent path))
         (cache  (assoc parent treemacs--open-dirs-cache))
         (values (cdr cache)))
    (when values
      (if (= 1 (length values))
          (setq treemacs--open-dirs-cache (delete cache treemacs--open-dirs-cache))
        (setcdr cache (delete path values))))
    (when purge
      ;; recursively grab all nodes open below PATH and remove them too
      (-if-let (children
                (->> values
                     (--map (cdr (assoc it treemacs--open-dirs-cache)))
                     (-flatten)))
          (--each children (treemacs--clear-from-cache it t))))))

(defun treemacs--create-icons ()
  "Create icons and put them in the icons hash."

  (setq treemacs-icons-hash (make-hash-table :test #'equal))

  (treemacs--setup-icon treemacs-icon-closed-png "dir_closed.png")
  (treemacs--setup-icon treemacs-icon-open-png   "dir_open.png")
  (treemacs--setup-icon treemacs-icon-text       "txt.png")

  (treemacs--setup-icon treemacs-icon-shell      "shell.png"       "sh" "zsh" "fish")
  (treemacs--setup-icon treemacs-icon-pdf        "pdf.png"         "pdf")
  (treemacs--setup-icon treemacs-icon-c          "c.png"           "c" "h")
  (treemacs--setup-icon treemacs-icon-cpp        "cpp.png"         "cpp" "hpp")
  (treemacs--setup-icon treemacs-icon-haskell    "haskell.png"     "hs")
  (treemacs--setup-icon treemacs-icon-python     "python.png"      "py" "pyc")
  (treemacs--setup-icon treemacs-icon-markdown   "markdown.png"    "md")
  (treemacs--setup-icon treemacs-icon-rust       "rust.png"        "rs" "toml")
  (treemacs--setup-icon treemacs-icon-image      "image.png"       "jpg" "bmp" "svg" "png")
  (treemacs--setup-icon treemacs-icon-emacs      "emacs.png"       "el" "elc" "org")
  (treemacs--setup-icon treemacs-icon-clojure    "clojure.png"     "clj" "cljs" "cljc")
  (treemacs--setup-icon treemacs-icon-typescript "typescript.png"  "ts")
  (treemacs--setup-icon treemacs-icon-css        "css.png"         "css")
  (treemacs--setup-icon treemacs-icon-conf       "conf.png"        "conf" "ini")

  (defvar treemacs-icon-closed-text (propertize "+" 'face 'treemacs-term-node-face))
  (defvar treemacs-icon-open-text   (propertize "-" 'face 'treemacs-term-node-face))
  (defvar treemacs-icon-closed treemacs-icon-closed-png)
  (defvar treemacs-icon-open treemacs-icon-open-png))

(cl-defun treemacs--goto-button-at (abs-path &optional (start-from (point-min)))
  "Move point to button identified by ABS-PATH, starting search at START.
Also return that button."
  (let ((keep-looking t)
        (filename (f-filename abs-path))
        (start (point))
        (ret))
    (goto-char start-from)
    (while (and keep-looking
                (search-forward filename nil t))
      (beginning-of-line)
      (let ((btn (next-button (point) t)))
        (if (s-equals? abs-path (button-get btn 'abs-path))
            (progn (treemacs--evade-image)
                   (setq keep-looking nil
                         ret btn))
          (beginning-of-line 2))))
    (unless ret (goto-char start))
    ret))

(defun treemacs--set-width (width)
  "Set the width of the treemacs buffer to WIDTH when it is created."
  (unless (one-window-p)
    (let ((window-size-fixed)
          (w (max width window-min-width)))
      (cond
       ((> (window-width) w)
        (shrink-window-horizontally  (- (window-width) w)))
       ((< (window-width) w)
        (enlarge-window-horizontally (- w (window-width))))))))

(defun treemacs--get-dir-content (dir)
  "Get the list of files in DIR.
Returns a list of two lists - first directories, then files, both sorted
according to `treemacs-sorting'."
  (let* ((attr-entries (directory-files-and-attributes dir t nil t))
         (sep (->> attr-entries
                   (treemacs--filter-files-to-be-shown)
                   (--separate (f-directory? (cl-first it)))))
         (sort-func (cl-case treemacs-sorting
                      (alphabetic-asc  #'treemacs--sort-alphabetic-asc)
                      (alphabetic-desc #'treemacs--sort-alphabetic-desc)
                      (size-asc        #'treemacs--sort-size-asc)
                      (size-desc       #'treemacs--sort-size-desc)
                      (mod-time-asc    #'treemacs--sort-mod-time-asc)
                      (mod-time-desc   #'treemacs--sort-mod-time-desc)
                      (t               (user-error "Unknown treemacs-sorting value '%s'" treemacs-sorting)))))
    (list
     (->> (cl-first sep)
          (funcall sort-func)
          (-map #'cl-first))
     (->> (cl-second sep)
          (funcall sort-func)
          (-map #'cl-first)))))

(defun treemacs--filter-files-to-be-shown (files)
  "Filter FILES for those files which treemacs should show.
These are the files which return nil for every function in
`treemacs-ignored-file-predicates' and do not match `treemacs-dotfiles-regex'.
The second test not apply if `treemacs-show-hidden-files' is t."
       (if treemacs-show-hidden-files
           (-filter #'treemacs--reject-ignored-files files)
         (-filter #'treemacs--reject-ignored-and-dotfiles files)))

(defun treemacs--std-ignore-file-predicate (file)
  "The default predicate to detect ignored files.
Will return t when FILE starts with '#.' or 'flycheck_' or is '.' or '..'."
       (s-matches? (rx bol
                       (or (seq (or "#." "flycheck_") (1+ any))
                           (or "." ".."))
                       eol)
                   file))

(defun treemacs--current-visibility ()
  "Return whether the current visibility state of the treemacs buffer.
Valid states are 'visible, 'exists and 'none."
  (cond
   ((treemacs--is-visible?)    'visible)
   ((treemacs--buffer-exists?) 'exists)
   (t 'none)))

(defun treemacs--current-root-btn ()
  "Return the current root button."
  (save-excursion
    (goto-char (point-min))
    (next-button (point) t)))

(defun treemacs--setup-buffer ()
  "Create and setup a buffer for treemacs in the right position and size."
  (select-window
   (display-buffer-in-side-window
    (get-buffer-create treemacs--buffer-name) '((side . left))))
  (treemacs--set-width treemacs-width)
  (let ((window-size-fixed))
    (set-window-dedicated-p (get-buffer-window) t)))

(defun treemacs--next-node (node)
  "Return NODE's lower same-indentation neighbour or nil if there is none."
  (when node
    (-if-let (next (button-get node 'next-node))
        next
      (treemacs--next-node (button-get node 'parent)))))

(defun str-assq-delete-all (key alist)
  "Same as `assq-delete-all', but use `string=' instead of `eq'.
Delete all elements whose car is ‘string=’ to KEY from ALIST."
  (while (and (consp (car alist))
              (string= (car (car alist)) key))
    (setq alist (cdr alist)))
  (let ((tail alist) tail-cdr)
    (while (setq tail-cdr (cdr tail))
      (if (and (consp (car tail-cdr))
               (string= (car (car tail-cdr)) key))
          (setcdr tail (cdr tail-cdr))
        (setq tail tail-cdr))))
  alist)

(defun treemacs--parent (path)
  "Parent of PATH, or PATH itself if PATH is the root directory."
  (if (f-root? path)
      path
    (f-parent path)))

(defalias 'treemacs--evade-image #'end-of-line
  "The cursor blinks visibly when it is on top of an image. Always moving it to
to end of the line prevents this from happening.")

(defun treemacs--kill-buffers-after-deletion (path is-file)
  "Clean up after a deleted file or directory.
Just kill the buffer visiting PATH if IS-FILE. Otherwise, go
through the buffer list and kill buffer if PATH is a prefix."

  (if is-file
      (let ((buf (get-file-buffer path)))
        (and buf
             (y-or-n-p (format "Kill buffer of %s, too? "
                               (f-filename path)))
             (kill-buffer buf)))

    ;; Prompt for each buffer visiting a file in directory
    (--each (buffer-list)
      (and
       (treemacs--is-path-in-dir? (buffer-file-name it) path)
       (y-or-n-p (format "Kill buffer %s in %s, too? "
                         (buffer-name it)
                         (f-filename path)))
       (kill-buffer it)))

    ;; Kill all dired buffers in one step
    (when (bound-and-true-p dired-buffers)
      (-when-let (dired-buffers-for-path
                 (->> dired-buffers
                      (--filter (treemacs--is-path-in-dir? (car it) path))
                      (-map #'cdr)))
        (and (y-or-n-p (format "Kill Dired buffers of %s, too? "
                               (f-filename path)))
             (-each dired-buffers-for-path #'kill-buffer))))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Winum Compatibility ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(with-eval-after-load 'winum

  ;; somestimes the compiler asks for the strangest things
  (declare-function treemacs--window-number-zero "treemacs-impl")

  (defun treemacs--window-number-zero ()
    (when (s-equals? (buffer-name) treemacs--buffer-name) 10))

  (when (boundp 'winum-assign-func)
    (setq winum-assign-func #'treemacs--window-number-zero)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Popwin Compatibility ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-eval-after-load 'popwin

  (defadvice popwin:create-popup-window
      (around treemacs--popwin-popup-buffer activate)
    (let ((v? (treemacs--is-visible?))
          (tb (get-buffer treemacs--buffer-name)))
      (when v?
        (with-current-buffer tb
          (setq window-size-fixed nil)))
      ad-do-it
      (when v?
        (with-current-buffer tb
          (setq window-size-fixed 'width)))))

  (defadvice popwin:close-popup-window
      (around treemacs--popwin-close-buffer activate)
    (let ((v? (treemacs--is-visible?))
          (tb (get-buffer treemacs--buffer-name)))
      (when v?
        (with-current-buffer tb
          (setq window-size-fixed nil)))
      ad-do-it
      (when v?
        (with-current-buffer tb
          (setq window-size-fixed 'width))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Golden Ratio Compatibility ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-eval-after-load 'golden-ratio
  (when (bound-and-true-p golden-ratio-exclude-modes)
    (add-to-list 'golden-ratio-exclude-modes 'treemacs-mode)))

(provide 'treemacs-impl)

;;; treemacs-impl.el ends here
