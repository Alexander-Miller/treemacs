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

(defvar treemacs--dirs-to-collpase.py (f-join treemacs-dir "treemacs-dirs-to-collapse.py"))

(defsubst treemacs--collapsed-dirs-process (path)
  "Start a new process to determine dirs to collpase under PATH.
Output format is a list of newline delimited lines. Each line is a list of
absolute paths delimited by '//'. The first path is the one being collapsed, the
second path is the string that needs to be appended to the collapsed path in the
treemacs view. The remaining paths are all the directories being collapsed, to
be put under filewatch, if `treemacs-filewatch-mode' is on."
  (when (> treemacs-collapse-dirs 0)
    (pfuture-new "python" treemacs--dirs-to-collpase.py path (number-to-string treemacs-collapse-dirs))))

(defun treemacs--parse-collapsed-dirs (future)
  "Parse the output of collpsed dirs FUTURE.
Splits the output on newlines, splits every line on // and swallows the first
newline."
  (when future
    (-when-let (out (pfuture-await-to-finish future))
      (->> out
           (s-split "\n")
           (cdr)
           (--map (s-split "//" it))))))

(provide 'treemacs-async)

;;; treemacs-async.el ends here
