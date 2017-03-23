;;; treemacs.el --- A tree style file viewer package

;; Copyright (C) 2017 Alexander Miller

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

;;; Commentary: Implementation extracted into its own file to reduce clutter.

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
(require 'vc-hooks)

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
  `(let ((inhibit-message))
     ,@body))

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
  (setq treemacs--open-dirs-cache '())
  ;; f-long to expand ~ and remove final slash
  ;; needed for root dirs given by projectile
  (treemacs--build-tree (f-long root)))

(defun treemacs--build-tree (root)
  "Build the file tree starting at the given ROOT."
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
  (-if-let (project-root (condition-case nil
                             (projectile-project-root)
                           (error nil)))
      (format "*%s*"
              (funcall projectile-project-name-function project-root))
    (treemacs--create-header root)))

(defun treemacs--insert-header (root)
  "Insert the header line for the given ROOT."
  (setq default-directory root)
  (insert-button (propertize (funcall treemacs-header-function root)
                             'face 'treemacs-header-face)
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
                                     (if treemacs-git-integration
                                         (treemacs--git-face path git-info)
                                       'treemacs-file-face)))))

(defun treemacs--create-branch (root indent-depth git-info &optional parent)
  "Create a new treemacs branch under ROOT.
The branch is indented at INDENT-DEPTH and uses GIT-INFO to decide on file
nodes' faces. The nodes' parent property is set to PARENT."
  (save-excursion
    (let* ((prefix       (concat "\n" (s-repeat (* indent-depth treemacs-indentation) " ")))
           (entries      (treemacs--get-dir-content root))
           (directories  (first entries))
           (files        (second entries))
           (dir-buttons  (--map (-> (treemacs--insert-node it prefix indent-depth parent git-info) (button-at)) directories))
           (file-buttons (--map (-> (treemacs--insert-node it prefix indent-depth parent git-info) (button-at)) files))
           (last-dir     (-some-> (last dir-buttons) (car)))
           (first-file   (first file-buttons)))
      (treemacs--set-neighbours dir-buttons)
      (treemacs--set-neighbours file-buttons)
      (when (and last-dir first-file)
        (button-put last-dir 'next-node first-file)
        (button-put first-file 'prev-node last-dir))
      ;; reopen here only since create-branch is called both when opening a node and
      ;; building the entire tree
      (treemacs--reopen-at root git-info))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Button interactions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defsubst treemacs--prop-at-point (prop)
  "Grab property PROP of the button at point."
  (save-excursion
    (beginning-of-line)
    (button-get (next-button (point) t) prop)))

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
       (treemacs--node-symbol-switch treemacs-icon-open)
       (treemacs--create-branch abs-path (1+ (button-get btn 'depth)) (or git-info (treemacs--parse-git-status abs-path)) btn)
       (unless no-add (treemacs--add-to-cache (treemacs--parent abs-path) abs-path))))))

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
  "Remove from DIRS directories that shouldn't be reopened.
That is, directories (and their descendants) that are in the reopen cache, but
are not being shown on account of `treemacs-show-hidden-files' being nil."
  (if treemacs-show-hidden-files
      dirs
    (let ((root (treemacs--current-root)))
      (--filter (not (--any (s-matches? treemacs-dotfiles-regex it)
                            (f-split (substring it (length root)))))
                dirs))))

(defun treemacs--reopen-at (abs-path git-info)
  "Reopen dirs below ABS-PATH.
Pass GIT-INFO along till it's needed."
  (-some->
   abs-path
   (assoc treemacs--open-dirs-cache)
   (cdr)
   (treemacs--maybe-filter-dotfiles)
   (--each (treemacs--reopen it git-info))))

(defsubst treemacs--reopen (abs-path git-info)
  "Reopen the node identified by its ABS-PATH.
Pass GIT-INFO along till it's needed."
  (treemacs--without-messages
   (treemacs--open-node (treemacs--goto-button-at abs-path) git-info t)))

(defsubst treemacs--clear-from-cache (path &optional purge)
  "Remove PATH from the open dirs cache.
Also remove any dirs below if PURGE is given."
  (let* ((parent (treemacs--parent path))
         (cache  (assoc parent treemacs--open-dirs-cache))
         (values (cdr cache)))
    (when values
      (if (= 1 (seq-length values))
          (setq treemacs--open-dirs-cache (delete cache treemacs--open-dirs-cache))
        (setcdr cache (delete path values))))
    (when purge
      ;; recursively grab all nodes open below PATH and remove them too
      (-if-let (children
                (->> values
                     (--map (cdr (assoc it treemacs--open-dirs-cache)))
                     (-flatten)))
          (--each children (treemacs--clear-from-cache it t))))))

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
  (treemacs--setup-icon treemacs-icon-rust     "rust.png"     "rs" "toml")
  (treemacs--setup-icon treemacs-icon-image    "image.png"    "jpg" "bmp" "svg" "png")
  (treemacs--setup-icon treemacs-icon-emacs    "emacs.png"    "el" "elc" "org"))

;;;;;;;;;;;;;;;;;;;;;
;; Git integration ;;
;;;;;;;;;;;;;;;;;;;;;

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
  (when (and treemacs-git-integration
             (treemacs--is-dir-git-controlled? path))
    (let* ((default-directory path)
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
            (setcdr it (->> (second it) (s-trim-left) (treemacs--unqote) (f-join git-root))))
          status)))))

(defsubst treemacs--git-face (path git-info)
  "Return the appropriate face for PATH given GIT-INFO."
  ;; for the sake of simplicity we only look at the state in the working tree
  ;; see OUTPUT section `git help status'
  (pcase (-some-> (rassoc path git-info) (car) (substring 0 1))
    ("M" 'treemacs-git-modified-face)
    ("?" 'treemacs-git-untracked-face)
    ("!" 'treemacs-git-ignored-face)
    ("A" 'treemacs-git-added-face)
    (_   'treemacs-git-unmodified-face)))

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
      (beginning-of-line)
      (let ((btn (next-button (point) t)))
        (if (s-equals? abs-path (button-get btn 'abs-path))
            (progn (treemacs--evade-image)
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

(defalias 'treemacs--evade-image #'end-of-line
  "The cursor blinks visibly when it is on top of an image. Always moving it to
to end of the line prevents this from happening.")

(defsubst treemacs--path-in-dir? (path dir)
  "Is PATH in directory DIR?"
  (s-starts-with? (concat dir "/") path))

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
       (treemacs--path-in-dir? (buffer-file-name it) path)
       (y-or-n-p (format "Kill buffer %s in %s, too? "
                         (buffer-name it)
                         (f-filename path)))
       (kill-buffer it)))

    ;; Kill all dired buffers in one step
    (when-let (dired-buffers-for-path
           (->> dired-buffers
                (--filter (treemacs--path-in-dir? (car it) path))
                (-map #'cdr)))
      (and (y-or-n-p (format "Kill Dired buffers of %s, too? "
                             (f-filename path)))
       (-each dired-buffers-for-path #'kill-buffer)))))

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

(provide 'treemacs-impl)

;;; treemacs-impl.el ends here
