;;; treemacs --- An emacs file viewer

;;; Commentary:

;;; Code:

;;;;;;;;;;;;;;;;;;
;; Requirements ;;
;;;;;;;;;;;;;;;;;;

(require 'cl-lib)
(require 's)
(require 'f)
(require 'ido)

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

(defface treemacs-icon-face
  '((t :inherit font-lock-string-face))
  "Face used by treemacs for its icons.")

;;;;;;;;;;;;;;;;;;;
;; Configuration ;;
;;;;;;;;;;;;;;;;;;;

(defvar treemacs-indentation 2
  "Number of spaces each level is indented.")

(defvar treemacs-width 35
  "Width of the treemacs buffer.")

(defvar treemacs-icon-closed-dir (propertize "⏵ " 'font-lock-face 'treemacs-icon-face)
  "Icon indicating a closed directory.")

(defvar treemacs-icon-opened-dir (propertize "⏷ " 'font-lock-face 'treemacs-icon-face)
  "Icon indicating an opened directory.")

(defvar treemacs-icon-file (propertize "🖺 " 'font-lock-face 'treemacs-icon-face)
  "File icon placeholder.")

(defvar treemacs-show-hidden-files t
  "Dotfiles will be shown if this is set to t and be hidden otherwise.")

(defvar treemacs-header-format "*%s*"
  "The format string which is used for the header line.  Valid formats are all strings
accepted by the `format' function for a single formatting
argument, which is the current root directory.")

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
(defun treemacs-push-button ()
  "Push the button in the current line."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (forward-button 1)
    (call-interactively #'push-button)))

;;;###autoload
(defun treemacs-uproot ()
  "Change current root to the next higher directory."
  (interactive)
  (let ((new-root  (treemacs--current-root)))
    (treemacs--build-tree (treemacs--parent new-root))
    (goto-line 2)))

;;;###autoload
(defun treemacs-next-higher-node ()
  "Move cursor to the next higher directory node.  Do nothing if current
node is already at the root level."
  (interactive)
  (beginning-of-line)
  (-some-> (next-button (point))
           (button-get 'parent)
           (button-start)
           (goto-char)))

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
(defun treemacs-refresh ()
  "Rebuild treemacs buffer."
  (interactive)
  (let* ((point     (point))
         (root      (treemacs--current-root))
         (root-btn  (treemacs--current-root-btn))
         (open-dirs (treemacs--collect-open-dirs root-btn)))
    (treemacs--build-tree root open-dirs)
    (goto-char point)
    (message "Treemacs buffer refreshed.")))

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
  (let* ((path      (-some-> (next-button (point))
                             (button-get 'abs-path)))
         (file-name (when path (f-filename path))))
    (when path
      (cond
       ((f-file? path)
        (when (y-or-n-p (format "Delete %s ? " file-name))
          (f-delete path)))
       ((f-directory? path)
        (when (y-or-n-p (format "Recursively delete %s ? " file-name))
          (f-delete path t)
          (treemacs--clear-from-cache path))))
      (treemacs-refresh))))

;;;###autoload
(defun treemacs-create-file (file-name)
  "Create file called FILE-NAME."
  (interactive "FFile name: ")
  (f-touch file-name)
  (treemacs-refresh))

;;;###autoload
(defun treemacs-create-dir (dir-name)
  "Create directory called DIR-NAME."
  (interactive "FDirectory name: ")
  (f-mkdir dir-name)
  (treemacs-refresh))

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

  (define-key evil-treemacs-state-map (kbd "h")   #'treemacs-uproot)
  (define-key evil-treemacs-state-map (kbd "l")   #'treemacs-change-root)
  (define-key evil-treemacs-state-map (kbd "M-j") #'treemacs-next-neighbour)
  (define-key evil-treemacs-state-map (kbd "M-k") #'treemacs-previous-neighbour))

;;;###autoload
(defun treemacs-default-config ()
  "Use n & p for navigating the treemacs buffer."
  (define-key treemacs-mode-map (kbd "n")  #'next-line)
  (define-key treemacs-mode-map (kbd "p")  #'previous-line)
  (define-key treemacs-mode-map (kbd "M-n")  #'treemacs-next-neighbour)
  (define-key treemacs-mode-map (kbd "M-p")  #'treemacs-previous-neighbour))

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
  (treemacs--delete-all)
  (treemacs--insert-header root)
  (treemacs--create-branch root 0)
  (goto-line 2)
  (when open-dirs (treemacs--reopen-dirs open-dirs)))

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
  (insert-button (format treemacs-header-format (f-filename root))
                 'face 'treemacs-header-face
                 'abs-path root
                 'acion #'ignore))

(defun treemacs--create-branch (root indent-depth &optional parent)
  "Create a new filetree branch below the given ROOT path with the given
INDENT-DEPTH.  PARENT is same as ROOT, but only provided if the branch
to be created is nested below another and not directly at the top level."
  (save-excursion
    (let* ((prefix    (s-repeat (* indent-depth treemacs-indentation) " "))
           (files     (treemacs--get-dir-content root))
           (buttons   (--map (treemacs--insert-node it prefix indent-depth parent) files))
           (btn-pairs (-zip-fill nil buttons (cdr buttons))))
      (--each btn-pairs
        (button-put (car it) 'next-node (cdr it))
        (when (not (null (cdr it)))
          (button-put (cdr it) 'prev-node (car it)))))))

(defun treemacs--insert-node (path prefix depth parent)
 "Insert a single button node.
PATH is the node's absolute path.  PREFIX is an empty string used for
indentation.  DEPTH is the nesting depth, used for calculating the prefix length
of all potential child branches.  PARENT is the node the new node is nested
under, if any."
  (end-of-line)
  (newline)
  (let* ((is-dir? (f-directory? path)))
    (insert prefix (if is-dir? treemacs-icon-closed-dir treemacs-icon-file))
    (insert-button (f-filename path)
                   'face      (if is-dir? 'treemacs-directory-face 'treemacs-file-face)
                   'state     (if is-dir? 'dir-closed 'file)
                   'action    #'treemacs--push-button
                   'abs-path path
                   'parent    parent
                   'depth     depth)))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Button interactions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun treemacs--push-button (btn)
  "Execute the appropriate action given the state of the BTN that has been pushed."
  (cl-case (button-get btn 'state)
    ('file       (treemacs--open-file #'split-window-vertically))
    ('dir-closed (treemacs--open-node btn))
    ('dir-open   (treemacs--close-node btn))))

(defun treemacs--open-node (btn)
  "Open the node given by BTN."
  (button-put btn 'state 'dir-open)
  (treemacs--node-symbol-switch treemacs-icon-closed-dir treemacs-icon-opened-dir)
  (treemacs--create-branch
   (button-get btn 'abs-path)
   (1+ (button-get btn 'depth))
   btn)
  (let ((dirs-to-open (-> (button-get btn 'abs-path)
                          (assoc treemacs--open-dirs-cache)
                          (cdr))))
    (when dirs-to-open (treemacs--reopen-dirs dirs-to-open))))

(defun treemacs--close-node (btn)
  "Close node given by BTN."
  (let* ((abs-path  (button-get btn 'abs-path))
         (open-dirs (treemacs--collect-open-dirs btn)))
    (treemacs--node-symbol-switch treemacs-icon-opened-dir treemacs-icon-closed-dir)
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
      (delete-trailing-whitespace))))

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

(defun treemacs--node-symbol-switch (from to)
  "Replace first instance of FROM with TO in current line."
  (save-excursion
    (beginning-of-line)
    (search-forward from)
    (replace-match (propertize to 'font-lock-face 'treemacs-icon-face))))

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

;;;;;;;;;;;;;;;;;
;; Misc. utils ;;
;;;;;;;;;;;;;;;;;

(defun treemacs--set-width (width)
  "Set the width of the treemacs buffer to WIDTH when it is created."
  (let ((w (max width window-min-width)))
    (cond
     ((> (window-width) w) (shrink-window-horizontally (- (window-width) w)))
     ((< (window-width) w) (enlarge-window-horizontally (- w (window-width)))))))

(defun treemacs--get-dir-content (dir)
  "Get the list of files in DIR.  Directories are sorted first."
  (-concat
   (f-directories dir #'treemacs--should-show?)
   (f-files dir #'treemacs--should-show?)))

(defun treemacs--should-show? (file-name)
  "Indicate whether FILE-NAME should be show in the treemacs buffer or kept hidden."
  (or treemacs-show-hidden-files
      (not (s-matches? "^\\..*" (f-filename file-name)))))

(defun treemacs--current-root ()
  "Return the current root directory."
  (goto-char (point-min))
  (-> (treemacs--current-root-btn)
      (button-get 'abs-path)))

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
    (define-key map (kbd "u")   #'treemacs-next-higher-node)
    (define-key map (kbd "q")   #'treemacs-toggle)
    (define-key map (kbd "Q")   #'treemacs-kill-buffer)
    (define-key map (kbd "ov")  #'treemacs-visit-file-vertical-split)
    (define-key map (kbd "oh")  #'treemacs-visit-file-horizontal-split)
    (define-key map (kbd "oo")  #'treemacs-visit-file-no-split)
    (define-key map (kbd "ox")  #'treemacs-xdg-open)

    map)
  "Keymap for `treemacs-mode'.")

(define-derived-mode treemacs-mode special-mode "Treemacs"
  "A major mode for displaying the directory tree in text mode."
  (setq buffer-read-only nil
        indent-tabs-mode nil
        truncate-lines   t)

  (setq-local cursor-type nil)
  (setq-local show-paren-mode nil)
  (setq window-size-fixed t)
  (electric-indent-local-mode -1)
  (hl-line-mode t)

  (when (fboundp 'spaceline-compile)
    (spaceline-compile "treemacs" '(workspace-number major-mode) nil)
    (setq-local
     mode-line-format
     '("%e" (:eval (spaceline-ml-treemacs))))))

(provide 'treemacs)

;;; treemacs.el ends here
