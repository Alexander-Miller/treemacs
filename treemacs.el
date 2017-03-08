;;; treemacs.el --- A tree style file viewer package

;; Copyright (C) 2016 Alexander Miller

;; Author: Alexander Miller <alexanderm@web.de>
;; Package-Requires: ((cl-lib "0.5") (dash "2.11.0") (s "1.10.0") (f "0.11.0") (ace-window "0.9.0"))
;; Homepage: https://github.com/Alexander-Miller/treemacs
;; Version: 1.0
;; Keywords: tree, file, explorer

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

;;;;;;;;;;;;;;;;;;
;; Requirements ;;
;;;;;;;;;;;;;;;;;;

(require 'cl-lib)
(require 'dash)
(require 's)
(require 'f)
(require 'ido)
(require 'ace-window)

;;;;;;;;;;;;
;; Groups ;;
;;;;;;;;;;;;

(defgroup treemacs nil
  "A major mode for displaying the file system in a tree layout."
  :group 'files
  :prefix "treemacs-"
  :link '(url-link :tag "Repository" "https://github.com/Alexander-Miller/treemacs"))

(defgroup treemacs-faces nil
  "Faces for treemacs' syntax highlighting."
  :group 'treemacs
  :group 'faces)

(defgroup treemacs-configuration nil
  "Treemacs configuration options."
  :group 'treemacs
  :prefix "treemacs-")

;;;;;;;;;;;
;; Faces ;;
;;;;;;;;;;;

(defface treemacs-directory-face
  '((t :inherit font-lock-function-name-face))
  "Face used by treemacs for directories."
  :group 'treemacs-faces)

(defface treemacs-file-face
  '((t :inherit default))
  "Face used by treemacs for files."
  :group 'treemacs-faces)

(defface treemacs-header-face
  '((t :inherit font-lock-constant-face :underline t :size 1.4))
  "Face used by treemacs for its header."
  :group 'treemacs-faces)

(defface treemacs-git-unmodified-face
  '((t :inherit treemacs-file-face))
  "Face used for unmodified files."
  :group 'treemacs-faces)

(defface treemacs-git-modified-face
  '((t :inherit font-lock-variable-name-face))
  "Face used for modified files."
  :group 'treemacs-faces)

(defface treemacs-git-ignored-face
  '((t :inherit font-lock-comment-face))
  "Face for ignored files."
  :group 'treemacs-faces)

(defface treemacs-git-untracked-face
  '((t :inherit font-lock-string-face))
  "Face for untracked files."
  :group 'treemacs-faces)

(defface treemacs-git-added-face
  '((t :inherit font-lock-type-face))
  "Face for newly added files."
  :group 'treemacs-faces)

;;;;;;;;;;;;;;;;;;;
;; Configuration ;;
;;;;;;;;;;;;;;;;;;;

(defcustom treemacs-indentation 2
  "The number of spaces each level is indented in the tree."
  :type 'integer
  :group 'treemacs-configuration)

(defcustom treemacs-width 35
  "Width of the treemacs buffer."
  :type 'integer
  :group 'treemacs-configuration)

(defcustom treemacs-show-hidden-files t
  "Dotfiles will be shown if this is set to t and be hidden otherwise."
  :type 'boolean
  :group 'treemacs-configuration)

(defcustom treemacs-header-format "*%s*"
  "Format string which is used to format the header line.  Valid formats
are all strings accepted by the `format' function for a single formatting
argument, which is the current root directory."
  :type 'string
  :group 'treemacs-configuration)

(defcustom treemacs-icons-hash (make-hash-table :test 'equal)
  "Hash table containing a mapping of icons onto file extensions."
  :type 'plist
  :group 'treemacs-configuration)

(defcustom treemacs-be-evil nil
  "When t use evil keys for navigation (j/k instead of n/p)."
  :type 'boolean
  :group 'treemacs-configuration)

(defcustom treemacs-git-integration nil
  "When t use different faces for files' different git states."
  :type 'boolean
  :group 'treemacs-configuration)

(defcustom treemacs-dotfiles-regex (rx bol "." (1+ any))
  "Files matching this regular expression count as dotfiles."
  :type 'regexp
  :group 'treemacs-configuration)

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
     ,@body
     (read-only-mode t)))

(defmacro treemacs--without-messages (&rest body)
  "Temporarily turn off messages to execute BODY."
  `(let ((inhibit inhibit-message))
     (setq inhibit-message t)
     ,@body
     (setq inhibit-message inhibit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Building and tearing down the file trees ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun treemacs--init (root)
  "Initialize and build treemacs buffer for ROOT."
  (if (treemacs--is-visible?)
      (treemacs--select-visible)
    (progn
      (treemacs--setup-buffer)
      (switch-to-buffer (get-buffer-create treemacs--buffer-name))
      (bury-buffer treemacs--buffer-name)))
  (treemacs-mode)
  (treemacs--build-tree root))

(defun treemacs--build-tree (root)
  "Build the file tree starting at the given ROOT. "
  (treemacs--with-writable-buffer
   (treemacs--delete-all)
   (treemacs--insert-header root)
   (treemacs--create-branch root 0)
   (goto-char 0)
   (forward-line 1)
   (treemacs-goto-column-1)))

(defun treemacs--delete-all ()
  "Delete all content of the buffer."
  (delete-region (point-min) (point-max)))

(defun treemacs--insert-header (root)
  "Insert the header line for the given ROOT."
  (setq default-directory root)
  (insert-button (format treemacs-header-format (f-filename root))
                 'face 'treemacs-header-face
                 'abs-path root
                 'action #'ignore))

(defsubst treemacs--set-neighbours (buttons)
  "Set next- and previous-node properties for each button in BUTTONS."
  (when buttons
    (cl-dolist (i (number-sequence 0 (- (seq-length buttons) 2)))
      (let ((b1 (nth i buttons))
            (b2 (nth (1+ i) buttons)))
        (button-put b1 'next-node b2)
        (button-put b2 'prev-node b1)))))


(defsubst treemacs--insert-node (path prefix depth parent &optional git-controlled? git-info)
 "Insert a single button node.
PATH is the node's absolute path.
PREFIX is an empty string used for indentation.
DEPTH is the nesting depth, used for calculating the prefix length
of all potential child branches.
PARENT is the node the new node is nested under, if any.
GIT-CONTROLLED? indicates wheter the files under parent are under git control.
GIT-INFO is an alist mapping each file to its git state (no mapping meaning
the file is unchanged)."
  (end-of-line)
  (let ((is-dir? (f-directory? path)))
    (insert prefix)
    (insert-image (if is-dir?
                      treemacs-icon-closed
                    (gethash (-some-> path (file-name-extension) (downcase)) treemacs-icons-hash treemacs-icon-text)))
    (insert-text-button (concat " " (f-filename path))
                        'state     (if is-dir? 'dir-closed 'file)
                        'action    #'treemacs--push-button
                        'abs-path  path
                        'parent    parent
                        'depth     depth
                        'face      (if is-dir? 'treemacs-directory-face
                                     (if (and treemacs-git-integration
                                              git-controlled?)
                                         (treemacs--git-face path git-info)
                                       'treemacs-file-face)))))

(defun treemacs--create-branch (root indent-depth &optional parent)
  "Create a new filetree branch below ROOT path with the given INDENT-DEPTH.
PARENT is same as ROOT, but only provided if the branch
to be created is nested below another and not directly at the top level."
  (save-excursion
    (let* ((prefix       (concat "\n" (s-repeat (* indent-depth treemacs-indentation) " ")))
           (entries      (treemacs--get-dir-content root))
           (directories  (first entries))
           (files        (second entries))
           (is-git-dir?  (when treemacs-git-integration (treemacs--is-dir-git-controlled? root)))
           (git-info     (when is-git-dir? (treemacs--parse-git-status root)))
           (dir-buttons  (--map (-> (treemacs--insert-node it prefix indent-depth parent is-git-dir? git-info) (button-at)) directories))
           (file-buttons (--map (-> (treemacs--insert-node it prefix indent-depth parent is-git-dir? git-info) (button-at)) files))
           (last-dir     (-some-> (last dir-buttons) (car)))
           (first-file   (first file-buttons)))
      (treemacs--set-neighbours dir-buttons)
      (treemacs--set-neighbours file-buttons)
      (when (and last-dir first-file)
        (button-put last-dir 'next-node first-file)
        (button-put first-file 'prev-node last-dir)))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Button interactions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defsubst treemacs--prop-at-point (prop)
  "Grab property PROP of the button at point."
  (button-get (next-button (point) t) prop))

(defun treemacs--push-button (btn)
  "Execute the appropriate action given the state of the BTN that has been pushed."
  (cl-case (button-get btn 'state)
    ('file       (treemacs-visit-file-vertical-split))
    ('dir-closed (treemacs--open-node btn))
    ('dir-open   (treemacs--close-node btn))))

(defun treemacs--open-node (btn &optional no-add)
  "Open the node given by BTN.
Do not reopen its previously open children when NO-REOPEN is given."
  (if (not (f-readable? (button-get btn 'abs-path)))
      (message "Directory is not readable.")
    (treemacs--with-writable-buffer
     (let ((abs-path (button-get btn 'abs-path)))
       (button-put btn 'state 'dir-open)
       (beginning-of-line)
       (treemacs--node-symbol-switch treemacs-icon-open)
       (treemacs--create-branch abs-path (1+ (button-get btn 'depth)) btn)
       (unless no-add (treemacs--add-to-cache (treemacs--parent abs-path) abs-path))
       (treemacs--reopen-at btn)))))

(defsubst treemacs--next-node (node)
  "Return NODE's lower same-indentation neighbour or nil if there is none."
  (when node
    (-if-let (next (button-get node 'next-node))
        next
      (treemacs--next-node (button-get node 'parent)))))

(defun treemacs--close-node (btn)
  "Close node given by BTN."
  (treemacs--with-writable-buffer
   (treemacs--node-symbol-switch treemacs-icon-closed)
   (treemacs--clear-from-cache (button-get btn 'abs-path))
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
  (save-excursion
    (beginning-of-line)
    (let* ((path     (button-get (next-button (point)) 'abs-path))
           (is-file? (f-file? path)))
      (when is-file?
        (select-window (or window (next-window)))
        (when split-func
          (call-interactively split-func)
          (call-interactively 'other-window))
        (find-file path)))))

(defun treemacs--node-symbol-switch (new-sym)
  "Replace icon in current line with NEW-SYM."
  (beginning-of-line)
  (if (> (button-get (next-button (point)) 'depth) 0)
      (progn
        (skip-chars-forward "[[:space:]]")
        (backward-char)
        (delete-char -1))
    (delete-char 1))
  (insert-image new-sym))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Restoration of opened dirs ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defsubst treemacs--maybe-filter-dotfiles (dirs)
  (unless treemacs-show-hidden-files
    (cl-dolist (dir dirs)
      (when (s-matches? treemacs-dotfiles-regex (f-filename dir))
        (setq dirs (--filter (not (or (s-equals? dir it) (s-starts-with? dir it))) dirs)))))
  dirs)

(defun treemacs--reopen-at (btn)
  "Reopen dirs below BTN."
  (-some-> (button-get btn 'abs-path)
           (assoc treemacs--open-dirs-cache)
           (cdr)
           (treemacs--maybe-filter-dotfiles)
           (-each #'treemacs--reopen)))

(defsubst treemacs--reopen (abs-path)
  "Reopen the node identified by its ABS-PATH."
  (treemacs--without-messages
   (treemacs--open-node (treemacs--goto-button-at abs-path) t)))

(defsubst treemacs--clear-from-cache (path &optional purge)
  "Remove PATH from the open dirs cache.
Also remove any dirs below if PURGE is given."
  (let* ((parent (treemacs--parent path))
         (cache  (assoc parent treemacs--open-dirs-cache))
         (values (cdr cache)))
    (if (= 1 (seq-length values))
        (setq treemacs--open-dirs-cache (delete cache treemacs--open-dirs-cache))
      (setcdr cache (delete path values)))
    (when purge
      (-if-let (children (-flatten (--map (cdr (assoc it treemacs--open-dirs-cache)) values)))
          (progn
            (--each children (treemacs--clear-from-cache it t)))))))

(defsubst treemacs--add-to-cache (parent opened-child)
  "Add to PARENT's open dirs cache an entry for OPENED-CHILD."
  (let ((cache (assoc parent treemacs--open-dirs-cache)))
    (if cache
        (push opened-child (cdr cache))
      (add-to-list 'treemacs--open-dirs-cache `(,parent ,opened-child)))))

;;;;;;;;;;;;;;;;;;;;;;
;; Buffer selection ;;
;;;;;;;;;;;;;;;;;;;;;;

(defsubst treemacs--is-visible? ()
  "Inidicates whether the treemacs buffer is current visible."
  (-contains? (ido-get-buffers-in-frames t) treemacs--buffer-name))

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

;;;;;;;;;;;
;; Icons ;;
;;;;;;;;;;;

(defun treemacs--create-icons ()
  "Create icons and put them in the icons hash."

  (setq treemacs-icons-hash (make-hash-table :test #'equal))

  (treemacs--setup-icon treemacs-icon-closed   "dir_closed.png")
  (treemacs--setup-icon treemacs-icon-open     "dir_open.png")
  (treemacs--setup-icon treemacs-icon-text     "txt.png")

  (treemacs--setup-icon treemacs-icon-shell    "shell.png"    "sh" "zsh" "fish")
  (treemacs--setup-icon treemacs-icon-pdf      "pdf.png"      "pdf")
  (treemacs--setup-icon treemacs-icon-cpp      "cpp.png"      "cpp" "hpp")
  (treemacs--setup-icon treemacs-icon-haskell  "haskell.png"  "hs")
  (treemacs--setup-icon treemacs-icon-python   "python.png"   "py" "pyc")
  (treemacs--setup-icon treemacs-icon-markdown "markdown.png" "md")
  (treemacs--setup-icon treemacs-icon-image    "image.png"    "jpg" "bmp" "svg" "png")
  (treemacs--setup-icon treemacs-icon-emacs    "emacs.png"    "el" "elc" "org"))

;;;;;;;;;;;;;;;;;;;;
;; Git integrtion ;;
;;;;;;;;;;;;;;;;;;;;

(defsubst treemacs--is-dir-git-controlled? (path)
  "Check whether PATH is under git control."
  (let ((default-directory path))
    (->> "git rev-parse"
        (shell-command-to-string)
        (s-starts-with? "fatal")
        (not))))

(defsubst treemacs--parse-git-status (path)
  "Use the git command line to parse the git states of the files under PATH."
  (let* ((default-directory path)
         (git-output (shell-command-to-string  "git status --ignored --porcelain")))
    (if (s-blank? git-output) '()
      (->> git-output
           (s-trim-right)
           (s-split "\n")
           (--map (s-split-up-to " " (s-trim it) 1))
           (--map `(,(->> (second it)
                          (s-trim-left)
                          (treemacs--unqote) ; file names with spaces are quoted
                          (f-filename)
                          (f-join path))
                    . ,(first it)))))))

(defsubst treemacs--unqote (str)
  "Unquote STR if it is wrapped in quotes."
  (if (s-starts-with? "\"" str)
      (replace-regexp-in-string "\"" "" str)
    str))

(defsubst treemacs--git-face (path git-info)
  "Return the appropriate face for PATH given GIT-INFO."
  (pcase (cdr (assoc path git-info))
    ("M"  'treemacs-git-modified-face)
    ("??" 'treemacs-git-untracked-face)
    ("!!" 'treemacs-git-ignored-face)
    ("A"  'treemacs-git-added-face)
    (_    'treemacs-git-unmodified-face)))

;;;;;;;;;;;;;;;;;
;; Misc. utils ;;
;;;;;;;;;;;;;;;;;

(defun treemacs--goto-button-at (abs-path)
  "Move point to button identified by ABS-PATH.
Also return that button."
  (goto-char (point-min))
  (let ((keep-looking t)
        (filename (f-filename abs-path))
        (ret))
    (while (and keep-looking
                (search-forward filename nil t))
      (message "Look for %s a %s" filename abs-path)
      (beginning-of-line)
      (let ((btn (next-button (point) t)))
        (if (s-equals? abs-path (button-get btn 'abs-path))
            (progn (treemacs-goto-column-1)
                   (setq keep-looking nil
                         ret btn))
          (beginning-of-line 2))))
    ret))

(defun treemacs--set-width (width)
  "Set the width of the treemacs buffer to WIDTH when it is created."
  (let ((w (max width window-min-width)))
    (cond
     ((> (window-width) w) (shrink-window-horizontally (- (window-width) w)))
     ((< (window-width) w) (enlarge-window-horizontally (- w (window-width)))))))

(defun treemacs--get-dir-content (dir)
  "Get the list of files in DIR.  Directories are sorted first."
  (let ((entries (-separate #'f-directory? (f-entries dir))))
    (if treemacs-show-hidden-files
        entries
      (--map (-filter #'treemacs--should-show? it) entries))))

(defsubst treemacs--should-show? (file-name)
  "Indicate whether FILE-NAME should be show in the treemacs buffer or kept hidden."
  (not (s-matches? treemacs-dotfiles-regex (f-filename file-name))))

(defun treemacs--current-root ()
  "Return the current root directory.

If both the root button and the root dir are needed it's more efficient to get
the root button and then grab its 'abs-path property."
  (-> (treemacs--current-root-btn)
      (button-get 'abs-path)))

(defun treemacs--current-root-btn ()
  "Return the current root button."
  (save-excursion
    (goto-char (point-min))
    (next-button (point) t)))

(defun treemacs--setup-buffer ()
  "Setup a buffer for treemacs in the right position and size."
  (-> (selected-window)
      (window-frame)
      (frame-root-window)
      (split-window nil 'left)
      (select-window))
  (treemacs--set-width treemacs-width))

(defun str-assq-delete-all (key alist)
  "Same as `assq-delete-all', but use `string=' instead of `eq'.
Delete all elements whose car is ‘eq’ to KEY from ALIST."
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

(defun treemacs-goto-column-1 ()
  "Move cursor to column #1 in current line."
  (beginning-of-line)
  (forward-char))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Window Numbering Compatibility ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-eval-after-load 'window-numbering

  (defun treemacs--window-number-zero ()
    (when (string= (buffer-name) treemacs--buffer-name) 0))

  (setq window-numbering-assign-func
        (lambda () (treemacs--window-number-zero))))

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

;;;;;;;;;;;;;;;
;; Autoloads ;;
;;;;;;;;;;;;;;;

;;;###autoload
(defun treemacs-toggle ()
  "If a treemacs buffer exists and is visible hide it.
If a treemacs buffer exists, but is not visible bring it to the foreground
and select it.
If no treemacs buffer exists call `treemacs-init.'"
  (interactive)
  (cond
   ((treemacs--is-visible?)
    (progn
      (treemacs--select-visible)
      (if (one-window-p)
          (switch-to-buffer (other-buffer))
        (delete-window))))
   ((treemacs--buffer-exists?)
    (treemacs--select-not-visible))
   (t
    (treemacs-init))))

;;;###autoload
(defun treemacs-init (&optional arg)
  "Open treemacs with current buffer's directory as root.
If the current buffer's default-directory is nil, use $HOME as fallback.
If a prefix argument ARG is given manually select the root directory."
  (interactive "P")
  (treemacs--init (cond
                   (arg (read-directory-name "Treemacs root: "))
                   (default-directory
                     ;; default direcoty (usually) ends with a slash, while the dir names
                     ;; given by f.el do not
                     (if (and (s-ends-with? "/" default-directory)
                              (> (length default-directory) 1))
                         (substring default-directory 0 -1)
                       default-directory))
                   (t (getenv "HOME")))))

;;;###autoload
(defun treemacs-projectile-init (&optional arg)
  "Open treemacs for the current projectile project. If not in a project do
nothing. If a prefix argument ARG is given select the project from among
`projectile-known-projects'."
  (interactive "P")
   (cond
    ((and arg projectile-known-projects)
     (treemacs--init (completing-read "Project: " projectile-known-projects)))
    ((projectile-project-p)
     (treemacs--init (projectile-project-root)))
    (t (message "You're not in a project."))))

;;;###autoload
(defun treemacs-next-line ()
  "Goto next line."
  (interactive)
  (forward-line 1)
  (treemacs-goto-column-1))

;;;###autoload
(defun treemacs-previous-line ()
  "Goto previous line."
  (interactive)
  (forward-line -1)
  (treemacs-goto-column-1))

;;;###autoload
(defun treemacs-push-button ()
  "Open/close directory. Open file with `treemacs-visit-file-vertical-split'."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (forward-button 1)
    (call-interactively #'push-button))
  (treemacs-goto-column-1))

;;;###autoload
(defun treemacs-uproot ()
  "Switch treemacs' root directory to current root's parent, if possible."
  (interactive)
  (let* ((root      (treemacs--current-root))
         (new-root  (treemacs--parent root)))
    (unless (s-equals? root new-root)
      (treemacs--build-tree new-root)
      (goto-char 0)
      (while (not (s-equals?
                   root
                   (-some-> (next-button (point)) (button-get 'abs-path))))
        (forward-button 1))
      (forward-button 1)
      (treemacs-goto-column-1))))

;;;###autoload
(defun treemacs-goto-parent-node ()
  "Select parent of selected node, if possible."
  (interactive)
  (beginning-of-line)
  (and (-some->
        (next-button (point))
        (button-get 'parent)
        (button-start)
        (goto-char))
       (treemacs-goto-column-1)))

;;;###autoload
(defun treemacs-next-neighbour ()
  "Select next node at the same depth as currently selected node, if possible."
  (interactive)
  (beginning-of-line)
  (-some-> (next-button (point))
           (button-get 'next-node)
           (button-start)
           (goto-char)))

;;;###autoload
(defun treemacs-previous-neighbour ()
  "Select previous node at the same depth as currently selected node, if possible."
  (interactive)
  (beginning-of-line)
  (-some-> (next-button (point))
           (button-get 'prev-node)
           (button-start)
           (goto-char)))

;;;###autoload
(defun treemacs-refresh ()
  "Refresh and rebuild treemacs buffer."
  (interactive)
  (-if-let (treemacs-buffer (get-buffer treemacs--buffer-name))
      (with-selected-window (get-buffer-window treemacs-buffer)
        (let* ((curr-line (line-number-at-pos))
               (curr-path (treemacs--prop-at-point 'abs-path))
               (win-start (window-start (get-buffer-window)))
               (root-btn  (treemacs--current-root-btn))
               (root      (button-get root-btn 'abs-path)))
          (treemacs--build-tree root)
          (treemacs--reopen-at root-btn)
          ;; move point to the same file it was with before the refresh if the file
          ;; still exists and is visible, stay in the same line otherwise
          (if (and (f-exists? curr-path)
                   (or treemacs-show-hidden-files
                       (not (s-matches? treemacs-dotfiles-regex (f-filename curr-path)))))
              (treemacs--goto-button-at curr-path)
            ;; not pretty, but there can still be some off by one jitter when
            ;; using forwald-line
            (treemacs--without-messages (with-no-warnings (goto-line curr-line))))
          (treemacs-goto-column-1)
          (set-window-start (get-buffer-window) win-start)
          ;; needs to be turned on again when refresh is called from outside the
          ;; treemacs window, otherwise it looks like the selection disappears
          (hl-line-mode t)
          (message "Treemacs buffer refreshed.")))
    (message "Treemacs buffer does not exist.")))

;;;###autoload
(defun treemacs-change-root ()
  "Use current directory as new root. Do nothing for files."
  (interactive)
  (beginning-of-line)
  (let* ((point     (point))
         (btn       (next-button point))
         (state     (button-get btn 'state))
         (new-root  (button-get btn 'abs-path)))
    (if (not (eq 'file state))
        (treemacs--build-tree new-root)
      (goto-char point))))

;;;###autoload
(defun treemacs-visit-file-vertical-split ()
  "Open current file by vertically splitting `next-window'. Do nothing for directories."
  (interactive)
  (treemacs--open-file nil #'split-window-vertically))

;;;###autoload
(defun treemacs-visit-file-horizontal-split ()
  "Open current file by horizontally splitting `next-window'. Do nothing for directories."
  (interactive)
  (treemacs--open-file nil #'split-window-horizontally))

;;;###autoload
(defun treemacs-visit-file-no-split ()
  "Open current file, performing no split and using `next-window' directly. Do nothing for directories."
  (interactive)
  (treemacs--open-file))

;;;###autoload
(defun treemacs-visit-file-ace ()
  "Open current file, using `ace-window' to decide which buffer to open the file in. Do nothing for directories."
  (interactive)
  (treemacs--open-file
   (aw-select "Select buffer")))

;;;###autoload
(defun treemacs-visit-file-ace-horizontal-split ()
  "Open the current file by horizontally splitting a buffer selected by
`ace-window'. Do nothing for directories."
  (interactive)
  (save-excursion
    (treemacs--open-file
     (aw-select "Select buffer") #'split-window-horizontally)))

;;;###autoload
(defun treemacs-visit-file-ace-vertical-split ()
  "Open current file by vertically splitting a buffer selected by `ace-window'.
Do nothing for directories."
  (interactive)
  (save-excursion
    (treemacs--open-file
     (aw-select "Select buffer") #'split-window-vertically)))

;;;###autoload
(defun treemacs-xdg-open ()
  "Open current file, using the `xdg-open' shell command. Do nothing for directories."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((abs-path (button-get (next-button (point)) 'abs-path)))
      (when (f-file? abs-path)
        (call-process-shell-command (format "xdg-open \"%s\" &" abs-path))))))

;;;###autoload
(defun treemacs-kill-buffer ()
  "Kill the treemacs buffer."
  (interactive)
  (when (string= treemacs--buffer-name
                 (buffer-name))
    (setq treemacs--open-dirs-cache '())
    (kill-this-buffer)
    (when (not (one-window-p))
      (delete-window))))

;;;###autoload
(defun treemacs-delete ()
  "Delete node at point.
A delete action must always be confirmed. Directories are deleted recursively."
  (interactive)
  (beginning-of-line)
  (-if-let (btn (next-button (point)))
      (let* ((path      (button-get btn 'abs-path))
             (file-name (f-filename path)))
        (when
            (cond
             ((f-file? path)
              (when (y-or-n-p (format "Delete %s ? " file-name))
                (f-delete path) t))
             ((f-directory? path)
              (when (y-or-n-p (format "Recursively delete %s ? " file-name))
                (f-delete path t)
                (treemacs--clear-from-cache path t)
                t)))
          (treemacs--without-messages (treemacs-refresh)))))
  (treemacs-goto-column-1))

;;;###autoload
(defun treemacs-create-file (dir filename)
  "In directory DIR create file called FILENAME."
  (interactive "DDirectory: \nMFilename: ")
  (f-touch (f-join dir filename))
  (treemacs--without-messages (treemacs-refresh)))

;;;###autoload
(defun treemacs-create-dir (dir dirname)
  "In directory DIR create directory called DIRNAME."
  (interactive "DDirectory:\nMDirname:")
  (f-mkdir (f-join dir dirname))
  (treemacs--without-messages (treemacs-refresh)))

;;;###autoload
(defun treemacs-toggle-show-dotfiles ()
  "Toggle the hiding and displaying of dotfiles."
  (interactive)
  (setq treemacs-show-hidden-files (not treemacs-show-hidden-files))
  (treemacs-refresh)
  (message (concat "Dotfiles will now be "
                   (if treemacs-show-hidden-files
                       "displayed." "hidden."))))

;;;###autoload
(defun treemacs-toggle-fixed-width ()
  "Toggle whether the treemacs buffer should have a fixed width.
See also `treemacs-width.'"
  (interactive)
  (if window-size-fixed
      (setq window-size-fixed nil)
    (setq window-size-fixed 'width))
  (message "Treemacs buffer width has been %s."
           (if window-size-fixed "locked" "unlocked")))

;;;###autoload
(defun treemacs-reset-width (&optional arg)
  "Reset the width of the treemacs buffer to `treemacs-buffer-width'.
If a prefix argument ARG is provided read a new value for
`treemacs-buffer-width'first."
  (interactive "P")
  (let ((window-size-fixed nil))
    (when arg (setq treemacs-width (read-number "New Width: ")))
    (treemacs--set-width treemacs-width)))

;;;;;;;;;;;;;;;;;;;;;;
;; Mode definitions ;;
;;;;;;;;;;;;;;;;;;;;;;

(defvar treemacs-mode-map
  (let ((map (make-sparse-keymap)))

    (define-key map [tab]        #'treemacs-push-button)
    (define-key map [return]     #'treemacs-visit-file-no-split)
    (define-key map (kbd "l")    #'treemacs-change-root)
    (define-key map (kbd "r")    #'treemacs-refresh)
    (define-key map (kbd "g")    #'treemacs-refresh)
    (define-key map (kbd "d")    #'treemacs-delete)
    (define-key map (kbd "cf")   #'treemacs-create-file)
    (define-key map (kbd "cd")   #'treemacs-create-dir)
    (define-key map (kbd "h")    #'treemacs-uproot)
    (define-key map (kbd "u")    #'treemacs-goto-parent-node)
    (define-key map (kbd "q")    #'treemacs-toggle)
    (define-key map (kbd "Q")    #'treemacs-kill-buffer)
    (define-key map (kbd "ov")   #'treemacs-visit-file-vertical-split)
    (define-key map (kbd "oh")   #'treemacs-visit-file-horizontal-split)
    (define-key map (kbd "oo")   #'treemacs-visit-file-no-split)
    (define-key map (kbd "oaa")  #'treemacs-visit-file-ace)
    (define-key map (kbd "oah")  #'treemacs-visit-file-ace-horizontal-split)
    (define-key map (kbd "oav")  #'treemacs-visit-file-ace-vertical-split)
    (define-key map (kbd "ox")   #'treemacs-xdg-open)

    map)
  "Keymap for `treemacs-mode'.")

(defun treemacs--evil-config ()
  "Create an evil state for treemacs mode.
Use j & k for navigating the treemacs buffer."

  (with-eval-after-load 'evil
    (evil-define-state treemacs
      "Treemacs state"
      :cursor '(hbar . 0)
      :enable (motion))

    (evil-set-initial-state 'treemacs-mode 'treemacs)

    (define-key evil-treemacs-state-map (kbd "j")   #'treemacs-next-line)
    (define-key evil-treemacs-state-map (kbd "k")   #'treemacs-previous-line)
    (define-key evil-treemacs-state-map (kbd "h")   #'treemacs-uproot)
    (define-key evil-treemacs-state-map (kbd "l")   #'treemacs-change-root)
    (define-key evil-treemacs-state-map (kbd "M-j") #'treemacs-next-neighbour)
    (define-key evil-treemacs-state-map (kbd "M-k") #'treemacs-previous-neighbour)
    (define-key evil-treemacs-state-map (kbd "th")  #'treemacs-toggle-show-dotfiles)
    (define-key evil-treemacs-state-map (kbd "tw")  #'treemacs-toggle-fixed-width)
    (define-key evil-treemacs-state-map (kbd "w")   #'treemacs-reset-width))

  t)

(defun treemacs--default-config ()
  "Use n & p for navigating the treemacs buffer."

  (define-key treemacs-mode-map (kbd "n")   #'treemacs-next-line)
  (define-key treemacs-mode-map (kbd "p")   #'treemacs-previous-line)
  (define-key treemacs-mode-map (kbd "M-n") #'treemacs-next-neighbour)
  (define-key treemacs-mode-map (kbd "M-p") #'treemacs-previous-neighbour)
  (define-key treemacs-mode-map (kbd "th")  #'treemacs-toggle-show-dotfiles)
  (define-key treemacs-mode-map (kbd "tw")  #'treemacs-toggle-fixed-width)
  (define-key treemacs-mode-map (kbd "w")   #'treemacs-reset-width)

  t)

(if treemacs-be-evil
    (treemacs--evil-config)
  (treemacs--default-config))

(treemacs--create-icons)

(define-derived-mode treemacs-mode special-mode "Treemacs"
  "A major mode for displaying the file system in a tree layout."

  (setq window-size-fixed   'width
        buffer-read-only    t
        truncate-lines      t
        indent-tabs-mode    nil
        cursor-type         nil
        desktop-save-buffer nil)

  (setq-local show-paren-mode nil)
  (electric-indent-local-mode -1)
  (hl-line-mode t)

  (when (fboundp 'spaceline-compile)
    (spaceline-install "treemacs"
                       '(((workspace-number window-number)
                          :separator "|"
                          :face highlight-face)
                         major-mode)
                       nil)
    (setq
     mode-line-format
     '("%e" (:eval (spaceline-ml-treemacs))))))

(provide 'treemacs)

;;; treemacs.el ends here
