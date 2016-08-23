;;; treemacs.el --- A tree style file viewer package

;; Copyright (C) 2016 Alexander Miller

;; Author: Alexander Miller <alexanderm@web.de>
;; Package-Requires: ((cl-lib "0.5") (dash "2.11.0") (s "1.10.0") (f "0.11.0") (ace-window "0.9.0"))
;; Homepage: https://github.com/Alexander-Miller/treemacs
;; Version: 1.0
;; Keywords: tree, file

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

;;;;;;;;;;;
;; Faces ;;
;;;;;;;;;;;

(defface treemacs-directory-face
  '((t :inherit font-lock-function-name-face))
  "Face used by treemacs for directories.")

(defface treemacs-file-face
  '((t :inherit font-lock-variable-name-face))
  "Face used by treemacs for files.")

(defface treemacs-header-face
  '((t :inherit font-lock-constant-face :underline t :size 1.4))
  "Face used by treemacs for its header.")

;;;;;;;;;;;;;;;;;;;
;; Configuration ;;
;;;;;;;;;;;;;;;;;;;

(defvar treemacs-indentation 2
  "Number of spaces each level is indented.")

(defvar treemacs-width 35
  "Width of the treemacs buffer.")

(defvar treemacs-show-hidden-files t
  "Dotfiles will be shown if this is set to t and be hidden otherwise.")

(defvar treemacs-header-format "*%s*"
  "The format string which is used for the header line.  Valid formats are all strings
accepted by the `format' function for a single formatting
argument, which is the current root directory.")

(defvar treemacs-icons-hash (make-hash-table :test 'equal)
  "Hash table containing a mapping of icons onto file extensions.")

;;;;;;;;;;;;;;;;;;
;; Private vars ;;
;;;;;;;;;;;;;;;;;;

(defvar treemacs--open-dirs-cache '()
  "Cache to keep track of opened subfolders.")

(defvar treemacs--buffer-name "Treemacs"
  "Name of the treemacs buffer.")

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
      (if (= 1 (list-length (window-list)))
          (switch-to-buffer (other-buffer))
        (delete-window))))
   ((treemacs--buffer-exists?)
    (treemacs--select-not-visible))
   (t
    (treemacs-init))))

;;;###autoload
(defun treemacs-init ()
  "Open treemacs with current buffer's directory as root.  If the current buffer is not visiting any files use $HOME as fallback."
  (interactive)
  (let ((current-file (buffer-file-name (current-buffer))))
    (treemacs--init (cond
                     ((and current-file (f-directory? current-file)) current-file)
                     (current-file (treemacs--parent current-file))
                     (t (getenv "HOME"))))))

;;;###autoload
(defun treemacs-projectile-init ()
  "Open treemacs for the current projectile project.  If not in a project do nothing."
  (interactive)
  (when (projectile-project-p)
    (treemacs--init (projectile-project-root))))

;;;###autoload
(defun treemacs-next-line ()
  "Goto next line."
  (interactive)
  (next-line)
  (treemacs-goto-column-1))

;;;###autoload
(defun treemacs-previous-line ()
  "Goto previous line."
  (interactive)
  (previous-line)
  (treemacs-goto-column-1))

;;;###autoload
(defun treemacs-push-button ()
  "Push the button in the current line."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (forward-button 1)
    (call-interactively #'push-button))
  (treemacs-goto-column-1))

;;;###autoload
(defun treemacs-uproot ()
  "Change current root to the next higher directory."
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
  "Move cursor to the next higher directory node.  Do nothing if current
node is already at the root level."
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
  "Move to the next node at the same level.  Do nothing if no such node exists."
  (interactive)
  (beginning-of-line)
  (-some-> (next-button (point))
           (button-get 'next-node)
           (button-start)
           (goto-char)))

;;;###autoload
(defun treemacs-previous-neighbour ()
  "Move to the previous node at the same level.  Do nothing if no such node exists."
  (interactive)
  (beginning-of-line)
  (-some-> (next-button (point))
           (button-get 'prev-node)
           (button-start)
           (goto-char)))

;;;###autoload
(defun treemacs-refresh (&optional no-message)
  "Rebuild treemacs buffer."
  (interactive)
  (let* ((point     (point))
         (root      (treemacs--current-root))
         (root-btn  (treemacs--current-root-btn))
         (open-dirs (treemacs--collect-open-dirs root-btn)))
    (treemacs--build-tree root open-dirs)
    (goto-char point)
    (unless no-message
      (message "Treemacs buffer refreshed."))))

;;;###autoload
(defun treemacs-change-root ()
  "Change the root to the path of the node at point."
  (interactive)
  (beginning-of-line)
  (let* ((point     (point))
         (btn       (next-button point))
         (state     (button-get btn 'state))
         (new-root  (button-get btn 'abs-path))
         (open-dirs (treemacs--collect-open-dirs btn)))
    (if (not (eq 'file state))
        (treemacs--build-tree new-root open-dirs)
      (goto-char point))))

;;;###autoload
(defun treemacs-visit-file-vertical-split ()
  "Visit file of the current node in a new vertical split.
Do nothing if current node is a directoy."
  (interactive)
  (treemacs--open-file #'split-window-vertically))

;;;###autoload
(defun treemacs-visit-file-horizontal-split ()
  "Visit file of the current node in a new horizontal split.
Do nothing if current node is a directoy."
  (interactive)
  (treemacs--open-file #'split-window-horizontally))

;;;###autoload
(defun treemacs-visit-file-no-split ()
  "Visit file of the current node in other window without performing a split.
Do nothing if current node is a directoy."
  (interactive)
  (treemacs--open-file))

;;;###autoload
(defun treemacs-visit-file-ace ()
  "Use `ace-window' to choose which buffer to visit the file at point."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((path (button-get (next-button (point)) 'abs-path)))
      (when (f-file? path)
        (select-window (aw-select "Select buffer"))
        (find-file path)))))

;;;###autoload
(defun treemacs-xdg-open ()
  "Xdg open current file of the current node.
Do nothing if current node is a directory."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((abs-path (button-get (next-button (point)) 'abs-path)))
      (when (f-file? abs-path)
        (call-process-shell-command (format "xdg-open \"%s\" &" abs-path))))))

;;;###autoload
(defun treemacs-kill-buffer ()
  "Quit treemacs and kill its buffer.  If the treemacs buffer is not currently
selected do nothing."
  (interactive)
  (when (string= treemacs--buffer-name
                 (buffer-name (current-buffer)))
    (setq treemacs--open-dirs-cache '())
    (kill-this-buffer)
    (when (not (one-window-p))
      (delete-window))))

;;;###autoload
(defun treemacs-delete ()
  "Delete the file at point."
  (interactive)
  (beginning-of-line)
  (-if-let (btn (next-button (point)))
      (let* ((path      (button-get btn 'abs-path))
             (file-name (f-filename path))
             (neighbour (or (button-get btn 'next-node) (button-get btn 'prev-node)))
             (pos       (if neighbour (button-start neighbour) (point))))
        (when
            (cond
             ((f-file? path)
              (when (y-or-n-p (format "Delete %s ? " file-name))
                (f-delete path)
                t))
             ((f-directory? path)
              (when (y-or-n-p (format "Recursively delete %s ? " file-name))
                (f-delete path t)
                (treemacs--clear-from-cache path)
                t)))
          (treemacs-refresh t)
          (goto-char pos))
        (message "")))
  (treemacs-goto-column-1))

;;;###autoload
(defun treemacs-create-file (file-name)
  "Create file called FILE-NAME."
  (interactive "FFile name: ")
  (f-touch file-name)
  (treemacs-refresh t))

;;;###autoload
(defun treemacs-create-dir (dir-name)
  "Create directory called DIR-NAME."
  (interactive "DDirectory name: ")
  (f-mkdir dir-name)
  (treemacs-refresh t))

;;;###autoload
(defun treemacs-toggle-show-dotfiles ()
  "Toggle the hiding and displaying of dotfiles."
  (interactive)
  (setq treemacs-show-hidden-files (not treemacs-show-hidden-files))
  (treemacs-refresh t)
  (message (concat "Dotfiles will now be "
                   (if treemacs-show-hidden-files
                       "displayed." "hidden."))))

;;;###autoload
(defun treemacs-evil-config ()
  "Create an evil state for treemacs mode.  Use j & k for navigating
the treemacs buffer."
  (require 'evil)
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

  t)

;;;###autoload
(defun treemacs-default-config ()
  "Use n & p for navigating the treemacs buffer."
  (define-key treemacs-mode-map (kbd "n")    #'treemacs-next-line)
  (define-key treemacs-mode-map (kbd "p")    #'treemacs-previous-line)
  (define-key treemacs-mode-map (kbd "M-n")  #'treemacs-next-neighbour)
  (define-key treemacs-mode-map (kbd "M-p")  #'treemacs-previous-neighbour)
  (define-key treemacs-mode-map (kbd "th")   #'treemacs-toggle-show-dotfiles)

  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Building and tearing down the file trees ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun treemacs--init (root)
  "Initialize and build treemacs buffer for ROOT."
  (if (treemacs--is-visible?)
      (treemacs--select-visible)
    (progn
      (treemacs--setup-buffer)
      (switch-to-buffer (get-buffer-create treemacs--buffer-name))))
  (treemacs-mode)
  (treemacs--build-tree root))

(defun treemacs--build-tree (root &optional open-dirs)
  "Build the file tree starting at the given ROOT.
If a list of OPEN-DIRS is provided they will be toggled open after the tree is constructed."
  (treemacs--with-writable-buffer
   (treemacs--delete-all)
   (treemacs--insert-header root)
   (treemacs--create-branch root 0)
   (goto-char 0)
   (forward-line 1)
   (treemacs-goto-column-1)
   (when open-dirs (treemacs--reopen-dirs open-dirs))))

(defun treemacs--delete-all ()
  "Delete all content of the buffer."
  (when (treemacs--current-root-btn)
    (let* ((root      (treemacs--current-root-btn))
           (open-dirs (treemacs--collect-open-dirs root))
           (abs-path  (button-get root 'abs-path)))
      (treemacs--clear-from-cache abs-path)
      (when open-dirs
        (treemacs--add-to-cache abs-path open-dirs))))
  (delete-region (point-min) (point-max)))

(defun treemacs--insert-header (root)
  "Insert the header line for the given ROOT."
  (setq default-directory root)
  (insert-button (format treemacs-header-format (f-filename root))
                 'face 'treemacs-header-face
                 'abs-path root
                 'acion #'ignore))

(defun treemacs--create-branch (root indent-depth &optional parent)
  "Create a new filetree branch below the given ROOT path with the given
INDENT-DEPTH.  PARENT is same as ROOT, but only provided if the branch
to be created is nested below another and not directly at the top level."
  (save-excursion
    (let* ((prefix       (concat "\n" (s-repeat (* indent-depth treemacs-indentation) " ")))
           (entries      (treemacs--get-dir-content root))
           (directories  (first entries))
           (files        (second entries))
           (dir-buttons  (--map (-> (treemacs--insert-node it prefix indent-depth parent) (button-at)) directories))
           (file-buttons (--map (-> (treemacs--insert-node it prefix indent-depth parent) (button-at)) files))
           (last-dir     (-some-> (last dir-buttons) (car)))
           (first-file   (first file-buttons)))
      (treemacs-set-neighbours dir-buttons)
      (treemacs-set-neighbours file-buttons)
      (when (and last-dir first-file)
        (button-put last-dir 'next-node first-file)
        (button-put first-file 'prev-node last-dir)))))

(defun treemacs--insert-node (path prefix depth parent)
 "Insert a single button node.
PATH is the node's absolute path.  PREFIX is an empty string used for
indentation.  DEPTH is the nesting depth, used for calculating the prefix length
of all potential child branches.  PARENT is the node the new node is nested
under, if any."
  (end-of-line)
  (let* ((is-dir? (f-directory? path)))
    (insert prefix)
    (insert-image (if is-dir?
                      treemacs-icon-closed
                    (gethash (file-name-extension path) treemacs-icons-hash treemacs-icon-text)))
    (insert-text-button (concat " " (f-filename path))
                        'face      (if is-dir? 'treemacs-directory-face 'treemacs-file-face)
                        'state     (if is-dir? 'dir-closed 'file)
                        'action    #'treemacs--push-button
                        'abs-path  path
                        'parent    parent
                        'depth     depth)))

(defun treemacs-set-neighbours (buttons)
  "Set next- and previous-node properties for each button in buttons."
  (when buttons
    (cl-dolist (i (number-sequence 0 (- (list-length buttons) 2)))
      (let ((b1 (nth i buttons))
            (b2 (nth (1+ i) buttons)))
        (button-put b1 'next-node b2)
        (button-put b2 'prev-node b1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Button interactions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun treemacs--push-button (btn)
  "Execute the appropriate action given the state of the BTN that has been pushed."
  (cl-case (button-get btn 'state)
    ('file       (treemacs-visit-file-vertical-split))
    ('dir-closed (treemacs--open-node btn))
    ('dir-open   (treemacs--close-node btn))))

(defun treemacs--open-node (btn)
  "Open the node given by BTN."
  (if (not (f-readable? (button-get btn 'abs-path)))
      (message "Directory is not readable.")
    (treemacs--with-writable-buffer
     (button-put btn 'state 'dir-open)
     (beginning-of-line)
     (treemacs--node-symbol-switch treemacs-icon-open)
     (treemacs--create-branch
      (button-get btn 'abs-path)
      (1+ (button-get btn 'depth))
      btn)
     (let ((dirs-to-open (-> (button-get btn 'abs-path)
                             (assoc treemacs--open-dirs-cache)
                             (cdr))))
       (when dirs-to-open (treemacs--reopen-dirs dirs-to-open))))))

(defun treemacs--close-node (btn)
  "Close node given by BTN."
  (let* ((abs-path  (button-get btn 'abs-path))
         (open-dirs (treemacs--collect-open-dirs btn)))
    (treemacs--with-writable-buffer
     (treemacs--node-symbol-switch treemacs-icon-closed)
     (forward-button 1)
     (beginning-of-line)
     (let* ((pos-start (point))
            (next-node (treemacs--next-node btn))
            (pos-end   (if next-node
                           (-> next-node (button-start) (previous-button) (button-end) (1+))
                         (point-max))))
       (treemacs--clear-from-cache abs-path)
       (when open-dirs
         (treemacs--add-to-cache abs-path open-dirs))
       (button-put btn 'state 'dir-closed)
       (delete-region pos-start pos-end)
       (delete-trailing-whitespace)))))

(defun treemacs--open-file (&optional split-func)
  "Visit file of the current node.  Use SPLIT-FUNC to split the window.
Do nothing if current node is a directory.  Do not split window if SPLIT-FUNC
is nil."
  (save-excursion
    (beginning-of-line)
    (let* ((path     (button-get (next-button (point)) 'abs-path))
           (is-file? (f-file? path)))
      (when is-file?
        (other-window 1)
        (when split-func (call-interactively split-func))
        (find-file path)))))

(defun treemacs--node-symbol-switch (new-sym)
  "Replace first instance of FROM with TO in current line."
  (beginning-of-line)
  (if (> (button-get (next-button (point)) 'depth) 0)
      (progn
        (skip-chars-forward "[[:space:]]")
        (backward-char)
        (delete-char -1))
    (delete-char 1))
  (insert-image new-sym))

(defun treemacs--next-node (node)
  "Return NODE's lower same-indentation neighbour or nil if there is none."
  (when node
    (-if-let (next (button-get node 'next-node))
        next
      (treemacs--next-node (button-get node 'parent)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Restoration of opened dirs ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun treemacs--collect-open-dirs (root)
  "Collect list of absolute paths of all opened nodes below ROOT."
  (save-excursion
    (goto-char (button-start root))
    (let* ((res       '())
           (btn       (next-button (point)))
           (next-path (-some->
                       (button-get root 'next-node)
                       (button-get 'abs-path))))
      (while (and btn
                  (-> (button-get btn 'abs-path)
                      (string= next-path)
                      (not)))
        (when (eq 'dir-open (button-get btn 'state))
          (add-to-list 'res (button-get btn 'abs-path)))
        (forward-button 1)
        (setq btn (next-button (point))))
      (reverse res))))

(defun treemacs--reopen-dirs (open-dirs)
  "Toggle open every node whose full path is in OPEN-DIRS."
  (save-excursion
    (goto-char 0)
    (cl-dolist (dir open-dirs)
      (while (-> (next-button (point) t)
                 (button-get 'abs-path)
                 (string= dir)
                 (not))
        (forward-button 1))
      (treemacs-push-button))))

(defun treemacs--clear-from-cache (path)
  "Remove from the cache of opened nodes all entries of PATH."
  (setq treemacs--open-dirs-cache (str-assq-delete-all path treemacs--open-dirs-cache)))

(defun treemacs--add-to-cache (path open-dirs)
  "Add to cache the OPEN-DIRS under PATH."
  (add-to-list 'treemacs--open-dirs-cache `(,path . ,open-dirs)))

;;;;;;;;;;;;;;;;;;;;;;
;; Buffer selection ;;
;;;;;;;;;;;;;;;;;;;;;;

(defun treemacs--is-visible? ()
  "Inidicates whether the treemacs buffer is current visible."
  (-contains? (ido-get-buffers-in-frames t) treemacs--buffer-name))

(defun treemacs--buffer-exists? ()
  "Indicates whether the treemacs buffer exists even if it is not visible."
  (-contains?
   (-map #'buffer-name (buffer-list))
   treemacs--buffer-name))

(defun treemacs--select-visible ()
  "Switch to treemacs buffer, given that it is currently visible."
  (select-window (get-buffer-window treemacs--buffer-name)))

(defun treemacs--select-not-visible ()
  "Switch to treemacs buffer, given that it not visible."
  (treemacs--setup-buffer)
  (switch-to-buffer treemacs--buffer-name))

;;;;;;;;;;;
;; Icons ;;
;;;;;;;;;;;

(defmacro treemacs--setup-icon (var file-name &rest extensions)
  "Define a variable VAR with the value being the image created from FILE-NAME.
Insert VAR into icon-cache for each of the given file EXTENSIONS."
  `(progn
     (defvar ,var
        (create-image (concat
                       (expand-file-name (if load-file-name (file-name-directory load-file-name) default-directory))
                       "icons/" ,file-name)
                      'png nil :ascent 'center))
     (--each (quote ,extensions) (puthash it ,var treemacs-icons-hash))))

(defun treemacs--create-icons ()
  "Create icons and put them in the icons hash."

  (setq treemacs-icons-hash (make-hash-table :test #'equal))

  (treemacs--setup-icon treemacs-icon-closed  "dir_closed.png")
  (treemacs--setup-icon treemacs-icon-open    "dir_open.png")
  (treemacs--setup-icon treemacs-icon-text    "txt.png")

  (treemacs--setup-icon treemacs-icon-shell   "shell.png"    "sh" "zsh" "fish")
  (treemacs--setup-icon treemacs-icon-pdf     "pdf.png"      "pdf")
  (treemacs--setup-icon treemacs-icon-cpp     "cpp.png"      "cpp" "hpp")
  (treemacs--setup-icon treemacs-icon-haskell "haskell.png"  "hs")
  (treemacs--setup-icon treemacs-icon-image   "image.png"    "jpg" "bmp" "svg" "png"))

;;;;;;;;;;;;;;;;;
;; Misc. utils ;;
;;;;;;;;;;;;;;;;;

(defmacro treemacs--with-writable-buffer (&rest body)
  "Temporarily turn off read-ony mode to execute BODY."
  `(progn
     (read-only-mode -1)
     ,@body
     (read-only-mode t)))

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

(defun treemacs--should-show? (file-name)
  "Indicate whether FILE-NAME should be show in the treemacs buffer or kept hidden."
  (not (s-matches? "^\\..*" (f-filename file-name))))

(defun treemacs--current-root ()
  "Return the current root directory."
  (save-excursion
    (goto-char (point-min))
    (-> (treemacs--current-root-btn)
        (button-get 'abs-path))))

(defun treemacs--current-root-btn ()
  "Return the current root button."
  (goto-char (point-min))
  (next-button (point) t))

(defun treemacs--setup-buffer ()
  "Setup a buffer for treemacs in the right position and size."
  (-> (selected-window)
      (window-frame)
      (frame-root-window)
      (split-window nil 'left)
      (select-window))
  (treemacs--set-width treemacs-width))

(defun str-assq-delete-all (key alist)
  "Same as `assq-delete-all', but use `string=' instead of `eq'."
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

;;;;;;;;;;;;;;;;;;;;;;
;; Mode definitions ;;
;;;;;;;;;;;;;;;;;;;;;;

(defvar treemacs-mode-map
  (let ((map (make-sparse-keymap)))

    (define-key map [tab]       #'treemacs-push-button)
    (define-key map [return]    #'treemacs-change-root)
    (define-key map (kbd "l")   #'treemacs-change-root)
    (define-key map (kbd "r")   #'treemacs-refresh)
    (define-key map (kbd "d")   #'treemacs-delete)
    (define-key map (kbd "cf")  #'treemacs-create-file)
    (define-key map (kbd "cd")  #'treemacs-create-dir)
    (define-key map (kbd "h")   #'treemacs-uproot)
    (define-key map (kbd "u")   #'treemacs-goto-parent-node)
    (define-key map (kbd "q")   #'treemacs-toggle)
    (define-key map (kbd "Q")   #'treemacs-kill-buffer)
    (define-key map (kbd "ov")  #'treemacs-visit-file-vertical-split)
    (define-key map (kbd "oh")  #'treemacs-visit-file-horizontal-split)
    (define-key map (kbd "oo")  #'treemacs-visit-file-no-split)
    (define-key map (kbd "oa")  #'treemacs-visit-file-ace)
    (define-key map (kbd "ox")  #'treemacs-xdg-open)

    map)
  "Keymap for `treemacs-mode'.")

(define-derived-mode treemacs-mode special-mode "Treemacs"
  "A major mode for displaying the directory tree in text mode."
  (setq buffer-read-only t
        indent-tabs-mode nil
        truncate-lines   t)

  (setq-local cursor-type nil)
  (setq-local show-paren-mode nil)
  (setq window-size-fixed 'width)
  (electric-indent-local-mode -1)
  (hl-line-mode t)

  (when (fboundp 'spaceline-compile)
    (spaceline-install "treemacs" '(workspace-number major-mode) nil)
    (setq
     mode-line-format
     '("%e" (:eval (spaceline-ml-treemacs))))))

(treemacs--create-icons)

(provide 'treemacs)

;;; treemacs.el ends here
