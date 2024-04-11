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

;; Code for dealing with treemacs' asynchronous features.

;;; Code:

(require 'dash)
(require 'ht)
(require 's)
(require 'vc-hooks)
(require 'pfuture)
(require 'treemacs-core-utils)
(require 'treemacs-customization)
(require 'treemacs-workspaces)
(require 'treemacs-dom)
(require 'treemacs-logging)
(require 'treemacs-visuals)

(eval-when-compile
  (require 'inline)
  (require 'treemacs-macros))

(treemacs-import-functions-from treemacs-rendering
  treemacs-do-delete-single-node)

(treemacs-import-functions-from treemacs-annotations
  treemacs--do-apply-annotation)

(defconst treemacs--dirs-to-collapse.py
  (if (member "treemacs-dirs-to-collapse.py" (directory-files treemacs-dir))
      (treemacs-join-path treemacs-dir "treemacs-dirs-to-collapse.py")
    (treemacs-join-path treemacs-dir "src/scripts/treemacs-dirs-to-collapse.py")))

(defconst treemacs--git-status.py
  (if (member "treemacs-git-status.py" (directory-files treemacs-dir))
      (treemacs-join-path treemacs-dir "treemacs-git-status.py")
    (treemacs-join-path treemacs-dir "src/scripts/treemacs-git-status.py")))

(defconst treemacs--single-file-git-status.py
  (if (member "treemacs-single-file-git-status.py" (directory-files treemacs-dir))
      (treemacs-join-path treemacs-dir "treemacs-single-file-git-status.py")
    (treemacs-join-path treemacs-dir "src/scripts/treemacs-single-file-git-status.py")))

(defconst treemacs--find-ignored-files.py
  (if (member "treemacs-find-ignored-files.py" (directory-files treemacs-dir))
      (treemacs-join-path treemacs-dir "treemacs-find-ignored-files.py")
    (treemacs-join-path treemacs-dir "src/scripts/treemacs-find-ignored-files.py")))

(defconst treemacs--single-git-update-debouce-store (make-hash-table :size 10)
  "Table to keep track of files that will already be updated.")

(defvar treemacs--git-cache-max-size 60
  "Maximum size for `treemacs--git-cache'.
If it does reach that size it will be cut back to 30 entries.")

(defvar treemacs--git-cache (make-hash-table :size treemacs--git-cache-max-size :test #'equal)
  "Stores the results of previous git status calls for directories.
Its effective type is HashMap<FilePath, HashMap<FilePath, Char>>.

These cached results are used as a stand-in during immediate rendering when
`treemacs-git-mode' is set to be deferred, so as to minimise the effect of large
face changes, especially when a full project is refreshed.

Since this table is a global value that can effectively grow indefinitely its
value is limited by `treemacs--git-cache-max-size'.")

(defvar treemacs-git-mode)

(define-inline treemacs--git-status-face (status default)
  "Get the git face for the given STATUS.
Use DEFAULT as default match.

STATUS: String
DEFAULT: Face"
  (declare (pure t) (side-effect-free t))
  (inline-letevals (status default)
    (inline-quote
     (pcase ,status
       ("M" 'treemacs-git-modified-face)
       ("U" 'treemacs-git-conflict-face)
       ("?" 'treemacs-git-untracked-face)
       ("!" 'treemacs-git-ignored-face)
       ("A" 'treemacs-git-added-face)
       ("R" 'treemacs-git-renamed-face)
       (_   ,default)))))

(defvar treemacs--git-mode nil
  "Saves the specific version of git-mode that is active.
Values are either `simple', `extended', `deferred' or nil.")

(defun treemacs--non-simple-git-mode-enabled ()
  "Indicate whether a version of git-mode is enabled that affects directories."
  (declare (side-effect-free t))
  (memq treemacs--git-mode '(deferred extended)))

(defun treemacs--resize-git-cache ()
  "Cuts `treemacs--git-cache' back down to size.
Specifically its size will be reduced to half of `treemacs--git-cache-max-size'."
  (treemacs-block
   (let* ((size (ht-size treemacs--git-cache))
          (count (- size (/ treemacs--git-cache-max-size 2))))
     (treemacs--maphash treemacs--git-cache (key _)
       (ht-remove! treemacs--git-cache key)
       (when (>= 0 (cl-decf count))
         (treemacs-return :done))))))

(defun treemacs--git-status-process-function (path)
  "Dummy with PATH.
Real implementation will be `fset' based on `treemacs-git-mode' value."
  (ignore path))

(defun treemacs--git-status-process (path project)
  "Run `treemacs--git-status-process-function' on PATH, if allowed for PROJECT.
Remote projects are ignored."
  (when (treemacs-project->is-local-and-readable? project)
    (treemacs--git-status-process-function path)))

(defun treemacs--git-status-parse-function (_future)
  "Dummy with FUTURE.
Real implementation will be `fset' based on `treemacs-git-mode' value."
  treemacs--empty-table)

(defun treemacs--git-status-process-extended (path)
  "Start an extended python-parsed git status process for files under PATH."
  (-when-let (git-root (vc-call-backend 'Git 'root path))
    (let* ((file-name-handler-alist nil)
           (git-root (expand-file-name git-root))
           (default-directory path)
           (open-dirs (cons
                       path
                       (-some->>
                        path
                        (treemacs-find-in-dom)
                        (treemacs-dom-node->reentry-nodes)
                        (-map #'treemacs-dom-node->key)
                        ;; Remove extension nodes
                        (-filter #'stringp))))
           (command `(,treemacs-python-executable
                      "-O"
                      ,treemacs--git-status.py
                      ,git-root
                      ,(number-to-string treemacs-max-git-entries)
                      ,treemacs-git-command-pipe
                      ,@open-dirs))
           (future (apply #'pfuture-new command)))
      future)))

(defun treemacs--parse-git-status-extended (git-future)
  "Parse the git status derived from the output of GIT-FUTURE.
The real parsing and formatting is done by the python process.  All that's
really left to do is pick up the cons list and put it in a hash table.

GIT-FUTURE: Pfuture"
  (or (when git-future
        (let* ((git-output (pfuture-await-to-finish git-future))
               (git-stderr (pfuture-stderr git-future)))
          ;; Check stderr separately from parsing, often git status displays
          ;; warnings which do not affect the final result.
          (unless (s-blank? git-stderr)
            (let ((visible-error (--> (s-trim git-stderr)
                                      (s-replace "\n" ", " it)
                                      (s-truncate 80 it)
                                      (propertize it 'face 'error))))
              (if (< (length git-stderr) 80)
                  (treemacs-log-err "treemacs-git-status.py wrote to stderr: %s" visible-error)
                (treemacs-log-err "treemacs-git-status.py wrote to stderr (see full output in *Messages*): %s" visible-error)
                (let ((inhibit-message t))
                  (treemacs-log "treemacs-git-status.py wrote to stderr: %s" git-stderr)))))
          (when (= 0 (process-exit-status git-future))
            (-let [parsed-output (read git-output)]
              (if (hash-table-p parsed-output)
                  parsed-output
                (let ((inhibit-message t))
                  (treemacs-log-err "treemacs-git-status.py output: %s" git-output))
                (treemacs-log-err "treemacs-git-status.py did not output a valid hash table. See full output in *Messages*.")
                nil)))))
      treemacs--empty-table))

(defun treemacs--git-status-process-simple (path)
  "Start a simple git status process for files under PATH."
  (let* ((default-directory (file-truename path))
         (process-environment (cons "GIT_OPTIONAL_LOCKS=0" process-environment))
         (future (pfuture-new "git" "status" "--porcelain" "--ignored=matching" "-z" ".")))
    (process-put future 'default-directory default-directory)
    future))

(defun treemacs--parse-git-status-simple (git-future)
  "Parse the output of GIT-FUTURE into a hash table."
  (-let [git-info-hash (make-hash-table :test #'equal :size 300)]
    (when git-future
      (pfuture-await-to-finish git-future)
      (when (= 0 (process-exit-status git-future))
        (-let [git-output (pfuture-result git-future)]
          (unless (s-blank? git-output)
            ;; need the actual git root since git status outputs paths relative to it
            ;; and the output must be valid also for files in dirs being reopened
            (let* ((git-root (vc-call-backend 'Git 'root (process-get git-future 'default-directory)))
                   (status-list (->> (substring git-output 0 -1)
                                     (s-split "\0")
                                     (--map (s-split-up-to " " (s-trim it) 1)))))
              (let ((len (length status-list))
                    (i 0))
                (while (< i len)
                  (let* ((status-cons (nth i status-list))
                         (status (car status-cons))
                         (path (cadr status-cons)))
                    ;; don't include directories since only a part of the untracked dirs
                    ;; would be shown anway
                    (unless (eq ?/ (aref path (1- (length path))))
                      ;; there's a NUL after every filename, so a rename looks like
                      ;; 'R oldnameNULnewnameNUL' which would break parsing that expects that a NUL separates
                      ;; status entries and not just filenames
                      (if (eq ?R (aref status 0))
                          (setq i (1+ i))
                        (ht-set! git-info-hash
                                 (treemacs-join-path git-root (s-trim-left path))
                                 (treemacs--git-status-face
                                  (substring (s-trim-left status) 0 1)
                                  'treemacs-git-unmodified-face)))))
                  (setq i (1+ i)))))))))
    git-info-hash))

(defun treemacs-update-single-file-git-state (file)
  "Update the FILE node's git state, wrapped in `treemacs-save-position'.
Internally calls `treemacs-do-update-single-file-git-state'.

FILE: FilePath"
  (treemacs-save-position
   (treemacs-do-update-single-file-git-state file)))

(defun treemacs-do-update-single-file-git-state (file &optional exclude-parents override-status)
  "Asynchronously update the given FILE node's git fontification.
Since an update to a single node can potentially also mean a change to the
states of all its parents they will likewise be updated by this function.  If
the file's current and new git status are the same this function will do
nothing.

When EXCLUDE-PARENTS is non-nil only the given FILE only the file node is
updated.  This is only used in case a file-watch update requires the insertion
of a new file that, due to being recently created, does not have a git status
cache entry.

When OVERRIDE-STATUS is non-nil the FILE's cached git status will not be used.

FILE: FilePath
EXCLUDE-PARENTS: Boolean
OVERRIDE-STATUS: Boolean"
  (let* ((local-buffer (current-buffer))
         (parent (treemacs--parent file))
         (parent-node (treemacs-find-in-dom parent)))
    (when (and
           treemacs-git-mode
           parent-node
           (null (ht-get treemacs--single-git-update-debouce-store file)))
      (ht-set! treemacs--single-git-update-debouce-store file t)
      (let* ((parents (unless (or exclude-parents
                                  (eq 'simple treemacs--git-mode)
                                  (null (treemacs-dom-node->parent parent-node)))
                        ;; include the first parent...
                        (cons (treemacs-dom-node->key parent-node)
                              ;; ...but exclude the project root
                              (cdr (-map #'treemacs-dom-node->key
                                         (treemacs-dom-node->all-parents parent-node))))))
             (git-cache (ht-get treemacs--git-cache parent))
             (current-face (if override-status
                               "OVERRIDE"
                             (or (-some-> git-cache (ht-get file) (symbol-name))
                                 "NONE")))
             (cmd `(,treemacs-python-executable
                    "-O"
                    ,treemacs--single-file-git-status.py ,file ,current-face ,@parents)))
        (pfuture-callback cmd
          :directory parent
          :name "Treemacs Update Single File Process"
          :on-success
          (progn
            (ht-remove! treemacs--single-git-update-debouce-store file)
            (when (buffer-live-p local-buffer)
              (with-current-buffer local-buffer
                (treemacs-with-writable-buffer
                 (save-excursion
                   ;; first the file node with its own default face
                   (-let [output (read (pfuture-callback-output))]
                     (-let [(path . face) (pop output)]
                       (treemacs--git-face-quick-change path face git-cache))
                     ;; then the directories
                     (pcase-dolist (`(,path . ,face) output)
                       (treemacs--git-face-quick-change path face))))))))
          :on-error
          (progn
            (ht-remove! treemacs--single-git-update-debouce-store file)
            (pcase (process-exit-status process)
              (2 (ignore "No Change, Do Nothing"))
              (_
               (-let [err-str (treemacs--remove-trailing-newline (pfuture-output-from-buffer pfuture-buffer))]
                 (treemacs-log-err "Update of node \"%s\" failed with status \"%s\" and result"
                   file (treemacs--remove-trailing-newline status))
                 (treemacs-log-err "\"%s\"" (treemacs--remove-trailing-newline err-str)))))))))))

(define-inline treemacs--git-face-quick-change (path git-face &optional git-cache)
  "Quick-change of PATH's GIT-FACE.
Updates the visible face and git-cache + annotation store entries.  GIT-CACHE
might be already known or not.  If not it will be pulled from BTN's parent.
Used when asynchronous processes report back git changes."
  (inline-letevals (path git-face git-cache)
    (inline-quote
     (let ((git-cache (or ,git-cache
                          (ht-get treemacs--git-cache
                                  (treemacs--parent-dir ,path))))
           (btn (treemacs-find-visible-node ,path)))
       (when git-cache
         (ht-set! git-cache ,path ,git-face))
       (when btn
         (treemacs--do-apply-annotation btn ,git-face))))))

(defun treemacs--flattened-dirs-process (path project)
  "Start a new process to determine directories to collapse under PATH.
Only starts the process if PROJECT is locally accessible (i.e. exists, and
is not remote.)
Output format is an elisp list of string lists that's read directly.
Every string list consists of the following elements:
 1) the extra text that must be appended in the view
 2) The original full and non-collapsed path
 3) a series of intermediate steps which are the result of appending the
    collapsed path elements onto the original, ending in
 4) the full path to the
    directory that the collapsing leads to.  For Example:
    (\"/26.0/elpa\"
     \"/home/a/Documents/git/treemacs/.cask\"
     \"/home/a/Documents/git/treemacs/.cask/26.0\"
     \"/home/a/Documents/git/treemacs/.cask/26.0/elpa\")"
  (when (and (> treemacs-collapse-dirs 0)
             treemacs-python-executable
             (treemacs-project->is-local-and-readable? project))
    (let (;; needs to be set or we'll run into trouble when deleting
          ;; haven't taken the time to figure out why, so let's just leave it at that
          (default-directory path)
          (search-paths nil))
      (treemacs-walk-reentry-dom (treemacs-find-in-dom path)
        (lambda (node) (push (treemacs-dom-node->key node) search-paths)))
      (-let [command
             `(,treemacs-python-executable
               "-O"
               ,treemacs--dirs-to-collapse.py
               ,(number-to-string treemacs-collapse-dirs)
               ,(if treemacs-show-hidden-files "t" "x")
               ,@search-paths)]
        (apply #'pfuture-new command)))))

(defun treemacs--parse-flattened-dirs (path future)
  "Parse the output of flattened dirs in PATH with FUTURE."
  (when future
    (-if-let (output (process-get future 'output))
        (ht-get output path)
      (let* ((stdout (pfuture-await-to-finish future))
             (output (if (= 0 (process-exit-status future))
                         (read stdout)
                       (ht))))
        (process-put future 'output output)
        (ht-get output path)))))

(defun treemacs--prefetch-gitignore-cache (path)
  "Pre-load all the git-ignored files in the given PATH.

PATH is either the symbol `all', in which case the state of all projects in the
current workspace is gathered instead, or a single project's path, when that
project has just been added to the workspace.

Required for `treemacs-hide-gitignored-files-mode' to properly work with
deferred git-mode, as otherwise ignored files will not be hidden on the first
run because the git cache has yet to be filled."
  (if (eq path 'all)
      (setf path (-map #'treemacs-project->path
                       (treemacs-workspace->projects (treemacs-current-workspace))))
    (setf path (list path)))
  (pfuture-callback `(,treemacs-python-executable
                      "-O"
                      ,treemacs--find-ignored-files.py
                      ,@path)
    :on-error (ignore)
    :on-success
    (let ((ignore-pairs (read (pfuture-callback-output)))
          (ignored-files nil))
      (while ignore-pairs
        (let* ((root  (pop ignore-pairs))
               (file  (pop ignore-pairs))
               (cache (ht-get treemacs--git-cache root)))
          (unless cache
            (setf cache (make-hash-table :size 20 :test 'equal))
            (ht-set! treemacs--git-cache root cache))
          (ht-set! cache file 'treemacs-git-ignored-face)
          (push file ignored-files)))
      (treemacs-run-in-every-buffer
       (treemacs-save-position
        (dolist (file ignored-files)
          (-when-let (treemacs-is-path-visible? file)
            (treemacs-do-delete-single-node file))))))))

(define-minor-mode treemacs-git-mode
  "Toggle `treemacs-git-mode'.

When enabled treemacs will check files' git status and highlight them
accordingly.  This git integration is available in 3 variants: simple, extended
and deferred.

The simple variant will start a git status process whose output is parsed in
elisp.  This version is simpler and slightly faster, but incomplete - it will
highlight only files, not directories.

The extended variant requires a non-trivial amount of parsing to be done, which
is achieved with python (specifically python3).  It is slightly slower, but
complete - both files and directories will be highlighted according to their git
status.

The deferred variant is the same is extended, except the tasks of rendering
nodes and highlighting them are separated.  The former happens immediately, the
latter after `treemacs-deferred-git-apply-delay' seconds of idle time.  This may
be faster (if not in truth then at least in appearance) as the git process is
given a much greater amount of time to finish.  The downside is that the effect
of nodes changing their colours may be somewhat jarring, though this issue is
largely mitigated due to the use of a caching layer.

All versions run asynchronously and are optimised for not doing more work than
is necessary, so their performance cost should, for the most part, be the
constant time needed to fork a subprocess."
  :init-value nil
  :global     t
  :lighter    nil
  :group      'treemacs-git
  ;; case when the mode is re-activated by `custom-set-minor-mode'
  (when (and (equal arg 1) treemacs--git-mode)
    (setf arg treemacs--git-mode))
  (if treemacs-git-mode
      (if (memq arg '(simple extended deferred))
          (treemacs--setup-git-mode arg)
        (call-interactively 'treemacs--setup-git-mode))
    (treemacs--tear-down-git-mode)))

(defun treemacs--setup-git-mode (&optional arg)
  "Set up `treemacs-git-mode'.
Use either ARG as git integration value of read it interactively."
  (interactive (list (-> (completing-read "Git Integration: " '("Simple" "Extended" "Deferred"))
                         (downcase)
                         (intern))))
  (setf treemacs--git-mode arg)
  (pcase treemacs--git-mode
    ((or 'extended 'deferred)
     (fset 'treemacs--git-status-process-function #'treemacs--git-status-process-extended)
     (fset 'treemacs--git-status-parse-function   #'treemacs--parse-git-status-extended))
    ('simple
     (fset 'treemacs--git-status-process-function #'treemacs--git-status-process-simple)
     (fset 'treemacs--git-status-parse-function   #'treemacs--parse-git-status-simple))
    (_
     (fset 'treemacs--git-status-process-function #'ignore)
     (fset 'treemacs--git-status-parse-function   (lambda (_) treemacs--empty-table)))))

(defun treemacs--tear-down-git-mode ()
  "Tear down `treemacs-git-mode'."
  (setf treemacs--git-mode nil)
  (fset 'treemacs--git-status-process-function #'ignore)
  (fset 'treemacs--git-status-parse-function   (lambda (_) treemacs--empty-table)))

(define-inline treemacs--get-or-parse-git-result (future)
  "Get the parsed git result of FUTURE.
Parse and set it if it hasn't been done yet.  If FUTURE is nil an empty hash
table is returned.

FUTURE: Pfuture process"
  (inline-letevals (future)
    (inline-quote
     (if ,future
         (--if-let (process-get ,future 'git-table)
             it
           (let ((result (treemacs--git-status-parse-function ,future)))
             (process-put ,future 'git-table result)
             result))
       treemacs--empty-table))))

(define-minor-mode treemacs-hide-gitignored-files-mode
  "Toggle `treemacs-hide-gitignored-files-mode'.

When enabled treemacs will hide files that are ignored by git.

Some form of `treemacs-git-mode' *must* be enabled, otherwise this feature will
have no effect.

Both `extended' and `deferred' git-mode settings will work in full (in case of
`deferred' git-mode treemacs will pre-load the list of ignored files so they
will be hidden even on the first run).  The limitations of `simple' git-mode
apply here as well: since it only knows about files and not directories only
files will be hidden."
  :init-value nil
  :global     t
  :lighter    nil
  :group      'treemacs-git
  (if treemacs-hide-gitignored-files-mode
      (progn
        (add-to-list 'treemacs-pre-file-insert-predicates
                     #'treemacs-is-file-git-ignored?)
        (when (and (eq 'deferred treemacs--git-mode)
                   (not (get 'treemacs-hide-gitignored-files-mode
                             :prefetch-done)))
          (treemacs--prefetch-gitignore-cache 'all)
          (put 'treemacs-hide-gitignored-files-mode :prefetch-done t)))
    (setf treemacs-pre-file-insert-predicates
          (delete #'treemacs-is-file-git-ignored?
                  treemacs-pre-file-insert-predicates)))
  (treemacs-run-in-every-buffer
   (treemacs--do-refresh (current-buffer) 'all))
  (when (called-interactively-p 'interactive)
    (treemacs-pulse-on-success "Git-ignored files will now be %s"
      (propertize
       (if treemacs-hide-gitignored-files-mode "hidden." "displayed.")
       'face 'font-lock-constant-face))) )

(treemacs-only-during-init
 (let ((has-git    (not (null (executable-find "git"))))
       (has-python (not (null treemacs-python-executable))))
   (pcase (cons has-git has-python)
     (`(t . t)
      (treemacs-git-mode 'deferred))
     (`(t . _)
      (treemacs-git-mode 'simple)))

   (when has-python
     (setf treemacs-collapse-dirs 3))

   (unless (or has-python (boundp 'treemacs-no-load-time-warnings))
     (treemacs-log-failure "Python3 not found, advanced git-mode and directory flattening features will be disabled."))))

(provide 'treemacs-async)

;;; treemacs-async.el ends here
