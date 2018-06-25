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
;;; Code in this file is considered performance critical.
;;; The usual restrictions w.r.t quality, readability and maintainability are
;;; lifted here.

;;; Code:

(require 's)
(require 'ht)
(require 'cl-lib)
(require 'treemacs-impl)
(require 'treemacs-icons)
(require 'treemacs-async)
(require 'treemacs-customization)
(require 'treemacs-structure)
(require 'treemacs-workspaces)
(eval-and-compile (require 'treemacs-macros))

(treemacs-import-functions-from "treemacs-filewatch-mode"
  treemacs--start-watching
  treemacs--stop-watching)

(defsubst treemacs--button-at (pos)
  "Return the button at position POS in the current buffer, or nil.
If the button at POS is a text property button, the return value
is a marker pointing to POS."
  (copy-marker pos t))

(defsubst treemacs--sort-alphabetic-asc (f1 f2)
  "Sort F1 and F2 alphabetically asc."
  (string-lessp f2 f1))

(defsubst treemacs--sort-alphabetic-desc (f1 f2)
  "Sort F1 and F2 alphabetically desc."
  (string-lessp f1 f2))

(defsubst treemacs--sort-alphabetic-case-insensitive-asc (f1 f2)
  "Sort F1 and F2 case insensitive alphabetically asc."
  (string-lessp (downcase f2) (downcase f1)))

(defsubst treemacs--sort-alphabetic-case-insensitive-desc (f1 f2)
  "Sort F1 and F2 case insensitive alphabetically desc."
  (string-lessp (downcase f1) (downcase f2)))

(defsubst treemacs--sort-size-asc (f1 f2)
  "Sort F1 and F2 by size asc."
  (>= (nth 7 (file-attributes f1))
      (nth 7 (file-attributes f2))))

(defsubst treemacs--sort-size-desc (f1 f2)
  "Sort F1 and F2 by size desc."
  (< (nth 7 (file-attributes f1))
     (nth 7 (file-attributes f2))))

(defsubst treemacs--sort-mod-time-asc (f1 f2)
  "Sort F1 and F2 by modification time asc."
  (file-newer-than-file-p f1 f2))

(defsubst treemacs--sort-mod-time-desc (f1 f2)
  "Sort F1 and F2 by modification time desc."
  (file-newer-than-file-p f2 f1))

(defsubst treemacs--get-button-face (path git-info default)
  "Return the appropriate face for PATH based on GIT-INFO.
If there is no git entry for PATH return DEFAULT.

PATH: Filepath
GIT-INFO: Hashtable
DEFAULT: Face"
  (declare (pure t) (side-effect-free t))
  (-pcase (ht-get git-info path)
    [?M 'treemacs-git-modified-face]
    [?U 'treemacs-git-conflict-face]
    [?? 'treemacs-git-untracked-face]
    [?! 'treemacs-git-ignored-face]
    [?A 'treemacs-git-added-face]
    [?R 'treemacs-git-renamed-face]
    [_  default]))

(defsubst treemacs--get-dir-content (dir)
  "Get the content of DIR, separated into sublists of first dirs, then files."
  (let* ((sort-func
          (-pcase treemacs-sorting
            ['alphabetic-asc #'treemacs--sort-alphabetic-asc]
            ['alphabetic-desc #'treemacs--sort-alphabetic-desc]
            ['alphabetic-case-insensitive-asc  #'treemacs--sort-alphabetic-case-insensitive-asc]
            ['alphabetic-case-insensitive-desc #'treemacs--sort-alphabetic-case-insensitive-desc]
            ['size-asc #'treemacs--sort-size-asc]
            ['size-desc #'treemacs--sort-size-desc]
            ['mod-time-asc #'treemacs--sort-mod-time-asc]
            ['mod-time-desc #'treemacs--sort-mod-time-desc]
            [_ (error "[Treemacs] Unknown treemacs-sorting value '%s'" treemacs-sorting)]))
         ;; `directory-files' is much faster in a temp buffer for whatever reason
         (entries (with-temp-buffer (-> dir (directory-files t nil t) (treemacs--filter-files-to-be-shown))))
         (dirs-files (-separate #'file-directory-p entries)))
    (list (sort (cl-first dirs-files) sort-func)
          (sort (cl-second dirs-files) sort-func))))

(defsubst treemacs--create-dir-button-strings (path prefix parent depth)
  "Return the text to insert for a directory button for PATH.
PREFIX is a string inserted as indentation.
PARENT is the (optional) button under which this one is inserted.
DEPTH indicates how deep in the filetree the current button is."
  ;; for directories the icon is included in the prefix since it's always known
  (list
   prefix
   (propertize (file-name-nondirectory path)
               'button '(t)
               'category 'default-button
               'help-echo nil
               :state 'dir-node-closed
               :path path
               :symlink (file-symlink-p path)
               :parent parent
               :depth depth)))

(defsubst treemacs--create-file-button-strings (path prefix parent depth)
  "Return the text to insert for a file button for PATH.
PREFIX is a string inserted as indentation.
PARENT is the (optional) button under which this one is inserted.
DEPTH indicates how deep in the filetree the current button is."
  (list
   prefix
   (ht-get treemacs-icons-hash
           (-> path (treemacs--file-extension) (downcase))
           treemacs-icon-fallback)
   (propertize (file-name-nondirectory path)
               'button '(t)
               'category 'default-button
               'help-echo nil
               :state 'file-node-closed
               :path path
               :parent parent
               :depth depth)))

(cl-defmacro treemacs--button-open (&key button new-state new-icon open-action post-open-action immediate-insert)
  "Building block macro to open a BUTTON.
Gives the button a NEW-STATE, and, optionally, a NEW-ICON. Performs OPEN-ACTION
and, optionally, POST-OPEN-ACTION. If IMMEDIATE-INSERT is non-nil it will concat
and apply `insert' on the items returned from OPEN-ACTION. If it is nil either
OPEN-ACTION or POST-OPEN-ACTION are expected to take over insertion."
  `(save-excursion
     (treemacs-with-writable-buffer
      (button-put ,button :state ,new-state)
      (beginning-of-line)
      ,@(when new-icon
          `((treemacs--button-symbol-switch ,new-icon)))
      (end-of-line)
      ,@(if immediate-insert
            `((progn
                (insert (apply #'concat ,open-action))))
          `(,open-action))
      ,post-open-action)))

(cl-defmacro treemacs--create-buttons (&key nodes depth extra-vars node-action node-name)
  "Building block macro for creating buttons from a list of items.
Will not making any insertions, but instead return a list of strings returned by
NODE-ACTION, so that the list can be further manipulated and efficiently
inserted in one go.
NODES is the list to create buttons from.
DEPTH is the indentation level buttons will be created on.
EXTRA-VARS are additional var bindings inserted into the initial let block.
NODE-ACTION is the button creating form inserted for every NODE.
NODE-NAME is the variable individual nodes are bound to in NODE-ACTION."
  `(let* ((depth ,depth)
          (prefix (concat "\n" (s-repeat (* depth treemacs-indentation) treemacs-indentation-string)))
          (,node-name (cl-first ,nodes))
          (strings)
          ,@extra-vars)
     (when ,node-name
       (dolist (,node-name ,nodes)
         (--each ,node-action
           (push it strings))))
     (nreverse strings)))

(defsubst treemacs--collapse-dirs (dirs)
  "Display DIRS as collpased.
Go to each dir button, expand its label with the collapsed dirs, set its new
path and give it a special parent-patX property so opening it will add the
correct cache entries.

DIRS: List of Collapse Paths. Each Collapse Path is a list of
 1) The original,full and uncollapsed path,
 2) the extra text that must be appended in the view,
 3) a series of intermediate steps which are the result of appending the
    collapsed path elements onto the original, ending in
 4) the full path to the
    directory that the collapsing leads to. For Example:
\(\"/home/a/Documents/git/treemacs/.cask\"
 \"/26.0/elpa\"
 \"/home/a/Documents/git/treemacs/.cask/26.0\"
 \"/home/a/Documents/git/treemacs/.cask/26.0/elpa\"\)"
  (when dirs
    (dolist (it dirs)
      ;; no warning since filewatch mode is known to be defined
      (when (with-no-warnings treemacs-filewatch-mode)
        (treemacs--start-watching (car it))
        (dolist (step (nthcdr 2 it))
          (treemacs--start-watching step t)))
      (let* ((b (treemacs-goto-button (car it)))
             (props (text-properties-at (button-start b)))
             (new-path (nth (- (length it) 1) it)))
        (button-put b :path new-path)
        ;; if the collapsed path leads to a symlinked directory the button needs to be marked as a symlink
        ;; so `treemacs--expand-dir-node' will know to start a new git future under its true-name
        (button-put b :symlink (or (button-get b :symlink)
                                   (--first (file-symlink-p it)
                                            (cdr it))))
        ;; number of direcoties that have been appended to the original path
        ;; value is used in `treemacs--follow-each-dir'
        (button-put b :collapsed (- (length it) 2))
        (end-of-line)
        (let* ((beg (point))
               (dir (cadr it))
               (parent (file-name-directory dir)))
          (insert dir)
          (add-text-properties beg (point) props)
          (add-text-properties
           (button-start b) (+ beg (length parent))
           '(face treemacs-directory-collapsed-face)))))))

(defun treemacs--create-branch (root depth git-future collapse-process &optional parent)
  "Create a new treemacs branch under ROOT.
The branch is indented at DEPTH and uses the eventual outputs of
GIT-FUTURE to decide on file buttons' faces and COLLAPSE-PROCESS to determine
which directories should be displayed as one. The buttons' parent property is
set to PARENT."
  (save-excursion
    (let* ((dirs-and-files (treemacs--get-dir-content root))
           (dirs (cl-first dirs-and-files))
           (files (cl-second dirs-and-files))
           (git-info)
           (file-strings)
           (dir-strings))
      (setq dir-strings
            (treemacs--create-buttons
             :nodes dirs
             :extra-vars ((dir-prefix (concat prefix treemacs-icon-closed)))
             :depth depth
             :node-name node
             :node-action (treemacs--create-dir-button-strings node dir-prefix parent depth)))
      (setq file-strings
            (treemacs--create-buttons
             :nodes files
             :depth depth
             :node-name node
             :node-action (treemacs--create-file-button-strings node prefix parent depth)))

      (--when-let (file-truename (button-get parent :path))
        (setq root it))

      ;; as reopening is done recursively the parsed git status is passed down to subsequent calls
      ;; so there are two possibilities: either the future given to this function is a pfuture object
      ;; that needs to complete and be parsed or it's an already finished git status hash table
      (if (ht? git-future)
          (setq git-info git-future)
        (setq git-info (treemacs--git-status-parse-function git-future)))
      ;; list contains prefixes and dirs, so every second item is a directory that needs a git face
      (end-of-line)

      ;; the files list contains 3 item tuples: the prefix the icon and the filename
      ;; direcories are different, since dirs do not  have different icons the icon is part if the prefix
      ;; therefore when filtering or propertizing the files and dirs only every 3rd or 2nd item must be looked at

      (when treemacs-pre-file-insert-predicates
        (-let [result nil]
          (while file-strings
            (-let*- [(prefix (car file-strings))
                     (icon (cadr file-strings))
                     (filename (cl-caddr file-strings))
                     (filepath (concat root "/" filename))]
              (unless (--any? (funcall it filepath git-info) treemacs-pre-file-insert-predicates)
                (setq result (cons filename (cons icon (cons prefix result))))))
            (setq file-strings (cl-cdddr file-strings)))
          (setq file-strings (nreverse result)))
        (-let- [(result nil)]
          (while dir-strings
            (-let*- [(prefix (car dir-strings))
                     (dirname (cadr dir-strings))
                     (dirpath (concat root "/" dirname))]
              (unless (--any? (funcall it dirpath git-info) treemacs-pre-file-insert-predicates)
                (setq result (cons dirname (cons prefix result)))))
            (setq dir-strings (cddr dir-strings)))
          (setq dir-strings (nreverse result))))

      (insert (apply #'concat
                     (--map-when (= 0 (% (+ 1 it-index) 2))
                                 (propertize it 'face
                                             (treemacs--get-button-face (concat root "/" it) git-info 'treemacs-directory-face))
                                 dir-strings)))

      (end-of-line)

      (insert (apply #'concat
                     (--map-when (= 0 (% (+ 1 it-index) 3))
                                 (propertize it 'face
                                             (treemacs--get-button-face (concat root "/" it) git-info 'treemacs-git-unmodified-face))
                                 file-strings)))
      (treemacs--collapse-dirs (treemacs--parse-collapsed-dirs collapse-process))
      (treemacs--reopen-at root git-info))))

(cl-defmacro treemacs--button-close (&key button new-state new-icon post-close-action)
  "Close node given by BTN, use NEW-ICON and set state of BTN to NEW-STATE."
  `(save-excursion
     (treemacs-with-writable-buffer
      ,@(when new-icon
          `((treemacs--button-symbol-switch ,new-icon)))
      (end-of-line)
      (forward-button 1)
      (beginning-of-line)
      (let* ((pos-start (point))
             (next (treemacs--next-non-child-button ,button))
             (pos-end (if next (-> next (button-start) (previous-button) (button-end) (1+)) (point-max))))
        (button-put ,button :state ,new-state)
        (delete-region pos-start pos-end)
        (delete-trailing-whitespace))
      ,post-close-action)))

(defun treemacs--expand-root-node (btn)
  "Expand the given root BTN."
  (-let*- [(path (button-get btn :path))
           (git-path (if (button-get btn :symlink) (file-truename path) path))
           (git-future (treemacs--git-status-process-function git-path))
           (collapse-future (treemacs--collapsed-dirs-process path))]
    (treemacs--button-open
     :immediate-insert nil
     :button btn
     :new-state 'root-node-open
     :open-action
     (treemacs--create-branch path (1+ (button-get btn :depth)) git-future collapse-future btn)
     :post-open-action
     (progn
       (treemacs-on-expand path btn nil)
       (treemacs--start-watching path)))))

(defun treemacs--collapse-root-node (btn &optional recursive)
  "Collapse the given root BTN.
Remove all open entries below BTN when RECURSIVE is non-nil."
  (treemacs--button-close
   :button btn
   :new-state 'root-node-closed
   :post-close-action
   (-let [path (button-get btn :path)]
     (treemacs--stop-watching path)
     (treemacs-on-collapse path recursive))))

(cl-defun treemacs--expand-dir-node (btn &key git-future recursive)
  "Open the node given by BTN.

BTN: Button
GIT-FUTURE: Pfuture|Hashtable
RECURSIVE: Bool"
  (if (not (f-readable? (button-get btn :path)))
      (treemacs-pulse-on-failure
       "Directory %s is not readable." (propertize (button-get btn :path) 'face 'font-lock-string-face))
    (let* ((path (button-get btn :path))
           (git-future (if (button-get btn :symlink)
                           (treemacs--git-status-process-function (file-truename path))
                         (or git-future (treemacs--git-status-process-function (file-truename path)))))
           (collapse-future (treemacs--collapsed-dirs-process path)))
      (treemacs--button-open
       :immediate-insert nil
       :button btn
       :new-state 'dir-node-open
       :new-icon treemacs-icon-open
       :open-action
       (progn
         ;; do on-expand first so buttons that need collapsing can quickly find their parent
         (treemacs-on-expand path btn (treemacs-parent-of btn))
         (treemacs--create-branch path (1+ (button-get btn :depth)) git-future collapse-future btn))
       :post-open-action
       (progn
         (treemacs--start-watching path)
         (when recursive
           (--each (treemacs--get-children-of btn)
             (when (eq 'dir-node-closed (button-get it :state))
               (goto-char (button-start it))
               (treemacs--expand-dir-node it :git-future git-future :recursive t)))))))))

(defun treemacs--collapse-dir-node (btn &optional recursive)
  "Close node given by BTN.
Remove all open dir and tag entries under BTN when RECURSIVE."
  (treemacs--button-close
   :button btn
   :new-state 'dir-node-closed
   :new-icon treemacs-icon-closed
   :post-close-action
   (-let [path (button-get btn :path)]
     (treemacs--stop-watching path)
     (treemacs-on-collapse path recursive))))

(defun treemacs--add-root-element (project)
  "Insert a new root node for the given PROJECT node.

PROJECT: `cl-struct-treemacs-project'"
  (insert treemacs-icon-root)
  (treemacs--set-project-position project (point-marker))
  (insert
   (propertize (treemacs-project->name project)
               'button '(t)
               'category 'default-button
               'face 'treemacs-root-face
               :project project
               :symlink (file-symlink-p (treemacs-project->path project))
               :state 'root-node-closed
               :path (treemacs-project->path project)
               :depth 0)))

(provide 'treemacs-branch-creation)

;;; treemacs-branch-creation.el ends here
