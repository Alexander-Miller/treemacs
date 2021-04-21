;;; treemacs.el --- A tree style file viewer package -*- lexical-binding: t -*-

;; Copyright (C) 2021 Alexander Miller

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

;; Implementation for logging messages.

;;; Code:

(require 'treemacs-customization)

(defvar treemacs--saved-eldoc-display nil
  "Stores the value of `treemacs-eldoc-display'.
The value is set to nil and stashed here with every log statement to prevent the
logged message being almost immediately overridden by the eldoc output.

The value is also stashed as a single-item-list which serves as a check make
sure it isn't stashed twice (thus stashing the already disabled nil value).")

(defvar treemacs--no-messages nil
  "When set to t `treemacs-log' will produce no output.
Not used directly, but as part of `treemacs-without-messages'.")

(defun treemacs--restore-eldoc-after-log ()
  "Restore the stashed value of `treemacs-eldoc-display'."
  (remove-hook 'pre-command-hook #'treemacs--restore-eldoc-after-log)
  (setf treemacs-eldoc-display (car treemacs--saved-eldoc-display)
        treemacs--saved-eldoc-display nil))

(defmacro treemacs-without-messages (&rest body)
  "Temporarily turn off messages to execute BODY."
  (declare (debug t))
  `(let ((treemacs--no-messages t))
     ,@body))

(defmacro treemacs--do-log (prefix msg &rest args)
  "Print a log statement with the given PREFIX and MSG and format ARGS."
  `(progn
     (unless (listp treemacs--saved-eldoc-display)
       (setf treemacs--saved-eldoc-display (list treemacs-eldoc-display)))
     (setf treemacs-eldoc-display nil)
     (unless treemacs--no-messages
       (message "%s %s" ,prefix (format ,msg ,@args)))
     (add-hook 'post-command-hook #'treemacs--restore-eldoc-after-log)))

(defmacro treemacs-log (msg &rest args)
  "Write an info/success log statement given format string MSG and ARGS."
  (declare (indent 1))
  `(treemacs--do-log
    (propertize "[Treemacs]" 'face 'font-lock-keyword-face)
    ,msg ,@args))

(defmacro treemacs-log-failure (msg &rest args)
  "Write a warning/failure log statement given format string MSG and ARGS."
  (declare (indent 1))
  `(treemacs--do-log
    (propertize "[Treemacs]" 'face '((:inherit warning :weight bold)))
    ,msg ,@args))

(defmacro treemacs-log-err (msg &rest args)
  "Write an error log statement given format string MSG and ARGS."
  (declare (indent 1))
  `(treemacs--do-log
    (propertize "[Treemacs]" 'face '((:inherit error :weight bold)))
    ,msg ,@args))

(provide 'treemacs-logging)

;;; treemacs-logging.el ends here
