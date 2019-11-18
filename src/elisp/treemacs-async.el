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
;;; Code for dealing with asynchronous processes.

;;; Code:

(require 'dash)
(require 'ht)
(require 's)
(require 'pfuture)
(require 'treemacs-core-utils)
(require 'treemacs-customization)
(require 'treemacs-workspaces)
(require 'treemacs-dom)
(eval-and-compile
  (require 'inline)
  (require 'treemacs-macros))

(defvar treemacs--dirs-to-collpase.py
  (eval-when-compile
    (if (member "treemacs-dirs-to-collapse.py" (directory-files treemacs-dir))
        (f-join treemacs-dir "treemacs-dirs-to-collapse.py")
      (f-join treemacs-dir "src/scripts/treemacs-dirs-to-collapse.py"))))

(defvar treemacs--git-status.py
  (eval-when-compile
    (if (member "treemacs-git-status.py" (directory-files treemacs-dir))
        (f-join treemacs-dir "treemacs-git-status.py")
      (f-join treemacs-dir "src/scripts/treemacs-git-status.py"))))

(defvar treemacs--single-file-git-status.py
  (eval-when-compile
    (if (member "treemacs-single-file-git-status.py" (directory-files treemacs-dir))
        (f-join treemacs-dir "treemacs-single-file-git-status.py")
      (f-join treemacs-dir "src/scripts/treemacs-single-file-git-status.py"))))

(defvar treemacs--git-cache-max-size 60
  "Maximum size for `treemacs--git-cache'.
If it does reach that size it will be cut back to 30 entries.")

(defvar treemacs--git-cache (make-hash-table :size treemacs--git-cache-max-size :test #'equal)
  "Stores the results of previous git status calls for directories.
Its effective type is HashMap<FilePath, HashMap<FilePath, Char>>.

These cached results are used as a standin during immediate rendering when
`treemacs-git-mode' is set to be deferred, so as to minimize the effect of large
face changes, epsecially when a full project is refreshed.

Since this table is a global value that can effectively grow indefinitely its
value is limited by `treemacs--git-cache-max-size'.")

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

(define-inline treemacs--get-node-face (path git-info default)
  "Return the appropriate face for PATH based on GIT-INFO.
If there is no git entry for PATH return DEFAULT.

PATH: Filepath
GIT-INFO: Hashtable
DEFAULT: Face"
  (declare (pure t) (side-effect-free t))
  (inline-letevals (path git-info default)
    (inline-quote
     (treemacs--git-status-face (ht-get ,git-info ,path) ,default))))

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
  (ht))

(defun treemacs--git-status-process-extended (path)
  "Start an extended python-parsed git status process for files under PATH."
  (-when-let (git-root (vc-call-backend 'Git 'root path))
    (let* ((file-name-handler-alist nil)
           (git-root (expand-file-name git-root))
           (default-directory path)
           (open-dirs (-some->>
                       path
                       (treemacs-find-in-dom)
                       (treemacs-dom-node->reentry-nodes)
                       (-map #'treemacs-dom-node->key)
                       ;; Remove extension nodes
                       (-filter #'stringp)))
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
The real parsing and formatting is done by the python process. All that's really
left to do is pick up the cons list and put it in a hash table.

GIT-FUTURE: Pfuture"
  (or (when git-future
        (let* ((git-output (pfuture-await-to-finish git-future))
               ;; Check fboundp in case an old version of pfuture is used.
               ;; TODO: Remove the check when pfuture 1.7 has been widely adopted.
               (git-stderr (when (fboundp 'pfuture-stderr)
                             (pfuture-stderr git-future))))
          ;; Check stderr separately from parsing, often git status displays
          ;; warnings which do not affect the final result.
          (unless (s-blank? git-stderr)
            (let ((visible-error (--> (s-trim git-stderr)
                                      (s-replace "\n" ", " it)
                                      (s-truncate 80 it)
                                      (propertize it 'face 'error))))
              (if (< (length git-stderr) 80)
                  (treemacs-log "treemacs-git-status.py wrote to stderr: %s" visible-error)
                (treemacs-log "treemacs-git-status.py wrote to stderr (see full output in *Messages*): %s" visible-error)
                (let ((inhibit-message t))
                  (treemacs-log "treemacs-git-status.py wrote to stderr: %s" git-stderr)))))
          (when (= 0 (process-exit-status git-future))
            (-let [parsed-output (read git-output)]
              (if (hash-table-p parsed-output)
                  parsed-output
                (let ((inhibit-message t))
                  (treemacs-log "treemacs-git-status.py output: %s" git-output))
                (treemacs-log "treemacs-git-status.py did not output a valid hash table. See full output in *Messages*.")
                nil)))))
      (ht)))

(defun treemacs--git-status-process-simple (path)
  "Start a simple git status process for files under PATH."
  (let* ((default-directory (f-canonical path))
         (process-environment (cons "GIT_OPTIONAL_LOCKS=0" process-environment))
         (future (pfuture-new "git" "status" "--porcelain" "--ignored" "-z" ".")))
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
                                 (f-join git-root (s-trim-left path))
                                 (substring (s-trim-left status) 0 1)))))
                  (setq i (1+ i)))))))))
    git-info-hash))

(defun treemacs--apply-deferred-git-state (parent-btn git-future buffer)
  "Apply the git fontification for direct children of PARENT-BTN.
GIT-FUTURE is parsed the same way as in `treemacs--create-branch'. Additionally
since this function is run on an idle timer the BUFFER to work on must be passed
as well since the user may since select a different buffer, window or frame.

PARENT-BTN: Button
GIT-FUTURE: Pfuture|HashMap
BUFFER: Buffer"
  (when (and (buffer-live-p buffer) git-future)
    (with-current-buffer buffer
      ;; cut the cache down to size if it grows too large
      (when (> (ht-size treemacs--git-cache) treemacs--git-cache-max-size)
        (run-with-idle-timer 2 nil #'treemacs--resize-git-cache))
      (-let [parent-path (treemacs-button-get parent-btn :path)]
        ;; the node may have been closed or deleted by now
        (when (and (treemacs-find-in-dom parent-path)
                   (memq (treemacs-button-get parent-btn :state) '(dir-node-open root-node-open)))
          (let ((depth (1+ (treemacs-button-get parent-btn :depth)))
                (git-info (treemacs--get-or-parse-git-result git-future))
                (btn parent-btn))
            (ht-set! treemacs--git-cache parent-path git-info)
            (treemacs-with-writable-buffer
             ;; the depth check ensures that we only iterate over the nodes that are below parent-btn
             ;; and stop when we've moved on to nodes that are above or belong to the next project
             (while (and (setq btn (next-button btn))
                         (>= (treemacs-button-get btn :depth) depth))
               (-let [path (treemacs-button-get btn :key)]
                 (when (and (= depth (treemacs-button-get btn :depth))
                            (not (treemacs-button-get btn :no-git)))
                   (treemacs-button-put
                    btn 'face
                    (treemacs--get-node-face path git-info (treemacs-button-get btn :default-face)))))))))))))

(defun treemacs-update-single-file-git-state (file)
  "Update the FILE node's git state, wrapped in `treemacs-save-position'.
Internally calls `treemacs-do-update-single-file-git-state'.

FILE: Filepath"
  (treemacs-save-position
   (treemacs-do-update-single-file-git-state file)))

(defun treemacs-do-update-single-file-git-state (file &optional exclude-parents override-status)
  "Asynchronously update the given FILE node's git fontification.
Since an update to a single node can potentially also mean a change to the
states of all its parents they will likewise be updated by this function. If the
file's current and new git status are the same this function will do nothing.

When OVERRIDE-STATUS is non-nil the FILE's cached git status will not be used.

When EXCLUDE-PARENTS is non-nil only the given FILE only the file node is
updated. This is only used in case a file-watch update requires the insertion of
a new file that, due to being recently created, does not have a git status cache
entry.

FILE: Filepath
EXCLUDE-PARENTS: Boolean
OVERRIDE-STATUS: Boolean"
  (let* ((local-buffer (current-buffer))
         (parent (treemacs--parent file))
         (parent-node (treemacs-find-in-dom parent)))
    (when parent-node
      (let* ((parents (unless (or exclude-parents
                                  (null (treemacs-dom-node->parent parent-node)))
                        ;; include the first parent...
                        (cons (treemacs-dom-node->key parent-node)
                              ;; ...but exclude the project root
                              (cdr (-map #'treemacs-dom-node->key
                                         (treemacs-dom-node->all-parents parent-node))))))
             (git-cache (ht-get treemacs--git-cache parent))
             (current-state (if override-status
                                "OVERRIDE"
                              (or (-some-> git-cache (ht-get file)) "0")))
             (cmd `(,treemacs-python-executable
                    "-O"
                    ,treemacs--single-file-git-status.py ,file ,current-state ,@parents)))
        (pfuture-callback cmd
          :directory parent
          :name "Treemacs Update Single File Process"
          :on-success
          (when (buffer-live-p local-buffer)
            (with-current-buffer local-buffer
              (treemacs-with-writable-buffer
               ;; first the file node with its own default face
               (-let [output (read (pfuture-callback-output))]
                 (-let [(file . state) (pop output)]
                   (when git-cache
                     (ht-set! git-cache file state))
                   (-when-let (pos (treemacs-find-visible-node file))
                     (-let [face (treemacs--git-status-face state 'treemacs-git-unmodified-face)]
                       (put-text-property
                        (treemacs-button-start pos) (treemacs-button-end pos)
                        'face face))))
                 ;; then the directories
                 (pcase-dolist (`(,file . ,state) output)
                   (-when-let (pos (treemacs-find-visible-node file))
                     (-let [face (treemacs--git-status-face state 'treemacs-directory-face)]
                       (put-text-property
                        (treemacs-button-start pos) (treemacs-button-end pos)
                        'face face))))))))
          :on-error
          (pcase (process-exit-status process)
            (2 (ignore "No Change, Do Nothing"))
            (_
             (-let [err-str (treemacs--remove-trailing-newline (pfuture-output-from-buffer pfuture-buffer))]
               (treemacs-log "Update of node \"%s\" failed with status \"%s\" and result"
                 file (treemacs--remove-trailing-newline status))
               (treemacs-log "\"%s\"" (treemacs--remove-trailing-newline err-str))))))))))

(defun treemacs--collapsed-dirs-process (path project)
  "Start a new process to determine dirs to collpase under PATH.
Only starts the process if PROJECT is locally accessible (i.e. exists, and
is not remote.)
Output format is an elisp list of string lists that's read directly.
Every string list consists of the following elements:
 1) the extra text that must be appended in the view
 2) The original full and uncollapsed path
 3) a series of intermediate steps which are the result of appending the
    collapsed path elements onto the original, ending in
 4) the full path to the
    directory that the collapsing leads to. For Example:
    (\"/26.0/elpa\"
     \"/home/a/Documents/git/treemacs/.cask\"
     \"/home/a/Documents/git/treemacs/.cask/26.0\"
     \"/home/a/Documents/git/treemacs/.cask/26.0/elpa\")"
  (when (and (> treemacs-collapse-dirs 0)
             treemacs-python-executable
             (treemacs-project->is-local-and-readable? project))
    ;; needs to be set or we'll run into trouble when deleting
    ;; haven't taken the time to figure out why, so let's just leave it at that
    (-let [default-directory path]
      (pfuture-new treemacs-python-executable
                   "-O"
                   treemacs--dirs-to-collpase.py
                   path
                   (number-to-string treemacs-collapse-dirs)
                   (if treemacs-show-hidden-files "t" "x")))))

(defun treemacs--parse-collapsed-dirs (future)
  "Parse the output of collpsed dirs FUTURE.
Splits the output on newlines, splits every line on // and swallows the first
newline."
  (when future
    (-let [output (pfuture-await-to-finish future)]
      (when (= 0 (process-exit-status future))
        (read output)))))

(define-minor-mode treemacs-git-mode
  "Toggle `treemacs-git-mode'.
When enabled treemacs will check files' git status and highlight them
accordingly. This git integration is available in 3 variants: simple, extended
and deferred.

The simple variant will start a git status process whose output is parsed in
elisp. This version is simpler and slightly faster, but incomplete - it will
highlight only files, not directories.

The extended variant requires a non-trivial amount of parsing to be done, which
is achieved with python (specifically python3). It is slightly slower, but
complete - both files and directories will be highlighted according to their
git status.

The deferred variant is the same is extended, except the tasks of rendering
nodes and highlighting them are separated. The former happens immediately, the
latter after `treemacs-deferred-git-apply-delay' seconds of idle time. This may
be faster (if not in truth then at least in appereance) as the git process is
given a much greater amount of time to finish. The downside is that the effect
of nodes changing their colors may be somewhat jarring, though this effect is
largely mitigated due to the use of a caching layer.

All versions run asynchronously and are optimized for not doing more work than
is necessary, so their performance cost should, for the most part, be the
constant time needed to fork a subprocess."
  :init-value nil
  :global t
  :lighter nil
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
  (setq treemacs-git-mode arg)
  (pcase treemacs-git-mode
    ((or 'extended 'deferred)
     (fset 'treemacs--git-status-process-function #'treemacs--git-status-process-extended)
     (fset 'treemacs--git-status-parse-function   #'treemacs--parse-git-status-extended))
    ('simple
     (fset 'treemacs--git-status-process-function #'treemacs--git-status-process-simple)
     (fset 'treemacs--git-status-parse-function   #'treemacs--parse-git-status-simple))
    (_
     (fset 'treemacs--git-status-process-function #'ignore)
     (fset 'treemacs--git-status-parse-function   (lambda (_) (ht))))))

(defun treemacs--tear-down-git-mode ()
  "Tear down `treemacs-git-mode'."
  (fset 'treemacs--git-status-process-function #'ignore)
  (fset 'treemacs--git-status-parse-function   (lambda (_) (ht))))

(define-inline treemacs--get-or-parse-git-result (future)
  "Get the parsed git result of FUTURE.
Parse and set it if it hasn't been done yet. If FUTURE is nil an empty hash
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
       (ht)))))

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
     (treemacs-log "Python3 not found, advanced git-mode and directory flattening features will be disabled."))))

(provide 'treemacs-async)

;;; treemacs-async.el ends here
