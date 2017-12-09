;;; treemacs.el --- A tree style file viewer package -*- lexical-binding: t -*-

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
;;; Code for dealing with asynchronous processes.

;;; Code:

(require 'dash)
(require 's)
(require 'pfuture)
(require 'treemacs-impl)
(require 'treemacs-customization)

(defvar treemacs--dirs-to-collpase.py (f-join treemacs-dir "src/scripts/treemacs-dirs-to-collapse.py"))
(defvar treemacs--git-status.py (f-join treemacs-dir "src/scripts/treemacs-git-status.py"))

(defsubst treemacs--git-status-process (path)
  "Create a new process future to get the git status under PATH.

PATH: Filepath"
  (when treemacs-git-integration
    (-when-let- [git-root (vc-call-backend 'Git 'root path)]
      (-let*- [(default-directory (f-canonical path))
               (future (pfuture-new
                        "python"
                        "-O"
                        "/home/a/Documents/git/treemacs/src/scripts/treemacs-git-status.py"
                        (f-long git-root)
                        path))]
        future))))

(defsubst treemacs--parse-git-status (git-future)
  "Parse the git status derived from the output of GIT-FUTURE.
The real parsing and formatting is done by the python process. All that's really
left to do is pick up the cons list and put it in a hash table.

GIT-FUTURE: Pfuture"
  (-let [git-info-hash (make-hash-table :test #'equal :size 300)]
    (when git-future
      (pfuture-await-to-finish git-future)
      (when (= 0 (process-exit-status git-future))
        (-let [git-output (pfuture-result git-future)]
          (unless (s-blank? git-output)
            (--each (read git-output)
              ;; key = path, value = git state char
              (puthash (cdr it)
                       (aref (car it) 0)
                       git-info-hash))))))
    git-info-hash))

(defsubst treemacs--collapsed-dirs-process (path)
  "Start a new process to determine dirs to collpase under PATH.
Output format is an elisp list of string lists that's read directly.
Every string list consists of the following elements:
 * The path that is being collapsed
 * The string to be appened to the collapsed path in the treemacs view
 * The single directories being collapsed, to be put under filewatch
   if `treemacs-filewatch-mode' is on."
  (when (> treemacs-collapse-dirs 0)
    (pfuture-new "python"
                 treemacs--dirs-to-collpase.py
                 path
                 (number-to-string treemacs-collapse-dirs)
                 (if treemacs-show-hidden-files "t" "x"))))

(defun treemacs--parse-collapsed-dirs (future)
  "Parse the output of collpsed dirs FUTURE.
Splits the output on newlines, splits every line on // and swallows the first
newline."
  (-some->
   future
   (pfuture-await-to-finish)
   (read)))

(provide 'treemacs-async)

;;; treemacs-async.el ends here
