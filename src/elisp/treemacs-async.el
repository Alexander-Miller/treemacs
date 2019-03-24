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
;;; Code for dealing with asynchronous processes.

;;; Code:

(require 'dash)
(require 'ht)
(require 's)
(require 'pfuture)
(require 'treemacs-impl)
(require 'treemacs-customization)
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

(defun treemacs--git-status-process-function (path)
  "Dummy with PATH.
Real implementation will be `fset' based on `treemacs-git-mode' value."
  (ignore path))

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
                       (treemacs-dom-node->children)
                       (-map #'treemacs-dom-node->key)
                       ;; Remove extension nodes
                       (-filter #'stringp)))
           (command `(,treemacs-python-executable
                      "-O" "-S"
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
  (-let [ret (ht)]
    (when git-future
      (pfuture-await-to-finish git-future)
      (when (= 0 (process-exit-status git-future))
        (-let [git-output (pfuture-result git-future)]
          (setf ret (read git-output)))))
    ret))

(defun treemacs--git-status-process-simple (path)
  "Start a simple git status process for files under PATH."
  (let* ((default-directory (f-canonical path))
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

(defun treemacs--collapsed-dirs-process (path)
  "Start a new process to determine dirs to collpase under PATH.
Output format is an elisp list of string lists that's read directly.
Every string list consists of the following elements:
 * The path that is being collapsed
 * The string to be appened to the collapsed path in the treemacs view
 * The single directories being collapsed, to be put under filewatch
   if `treemacs-filewatch-mode' is on."
  (when (> treemacs-collapse-dirs 0)
    ;; needs to be set or we'll run into trouble when deleting
    ;; haven't taken the time to figure out why, so let's just leave it at that
    (-let [default-directory path]
      (pfuture-new treemacs-python-executable
                   "-O"
                   "-S"
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
 (pcase (cons (not (null (executable-find "git")))
              (not (null (executable-find "python3"))))
   (`(t . t)
    (treemacs-git-mode 'deferred))
   (`(t . _)
    (treemacs-git-mode 'simple))))

(provide 'treemacs-async)

;;; treemacs-async.el ends here
