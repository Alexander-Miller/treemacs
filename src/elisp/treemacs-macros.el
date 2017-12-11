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
;;; General purpose macros, and those used in, but defined outside of
;;; treemacs-impl.el are put here, to prevent using them before their
;;; definition, hopefully preventing issues like #97.

;;; Code:

(require 'dash)

(defmacro treemacs--import-functions-from (file &rest functions)
  "Import FILE's FUNCTIONS."
  (declare (indent 1))
  (let ((imports (--map (list 'declare-function it file) functions)))
    `(progn ,@imports)))

(defmacro treemacs--defvar-with-default (var val)
  "Define a VAR with value VAL.
Remember the value in `treemacs--defaults-icons'."
  `(progn
     (defvar ,var ,val)
     (push (cons ',var ,val) treemacs--defaults-icons)))

(defmacro treemacs--log (msg &rest args)
  "Write a log statement given format string MSG and ARGS."
  `(unless treemacs--no-messages
     (message
      "%s %s"
      (propertize "[Treemacs]" 'face 'font-lock-keyword-face)
      (format ,msg ,@args))))

(defmacro treemacs--with-writable-buffer (&rest body)
  "Temporarily turn off read-ony mode to execute BODY."
  `(progn
     (read-only-mode -1)
     (unwind-protect
         (progn ,@body)
       (read-only-mode t))))

(defmacro treemacs--without-messages (&rest body)
  "Temporarily turn off messages to execute BODY."
  `(let ((treemacs--no-messages t))
     (unwind-protect
         ,@body
       (setq treemacs--no-messages nil))))

(defmacro treemacs--safe-button-get (button &rest properties)
  "Safely extract BUTTON's PROPERTIES.

Using `button-get' on a button located in a buffer that is not the current
buffer does not work, so this function will run the property extaction from
inside BUTTON's buffer."
  `(with-current-buffer (marker-buffer ,button)
     ,(if (= 1 (length properties))
           `(button-get ,button ,(car properties))
         `(--map (button-get ,button it) ,properties))))

(defmacro treemacs--with-button-buffer (btn &rest body)
  "Use BTN's buffer to execute BODY.
Required for button interactions (like `button-get') that do not work when
called from another buffer than the one the button resides in and
`treemacs--safe-button-get' is not enough."
  (declare (indent 1))
  `(with-current-buffer (marker-buffer ,btn)
    ,@body))

(defmacro -if-let- (var-val then &rest else)
  "Same as `-if-let', but expects VAR-VAL to be a vector.
Delegates VAR-VAL, THEN and ELSE to `-if-let'."
  (declare (debug ((sexp form) form body))
           (indent 2))
  (-let [var-val-lst (list (aref var-val 0) (aref var-val 1))]
    `(-if-let ,var-val-lst ,then ,@else)))

(defmacro treemacs--with-current-button (error-msg &rest body)
  "Execute an action with the current button bound to 'current-btn'.
Log ERROR-MSG if no button is selected, otherwise run BODY."
  (declare (debug (form body)))
  `(-if-let- [current-btn (treemacs-current-button)]
       (progn
         ,@body)
     (treemacs--log ,error-msg)))

(defmacro -when-let- (var-val &rest body)
  "Same as `-when-let', but expects VAR-VAL to be a vector.
Delegates VAR-VAL and BODY to `-when-let'."
  (declare (debug ((sexp form) body))
           (indent 1))
  (-let [var-val-lst (list (aref var-val 0) (aref var-val 1))]
    `(-when-let ,var-val-lst ,@body)))

(defmacro -let- (vars &rest body)
  "Same as `let', but VARS is an array.
Otherwise just delegates VARS and BODY to `let'."
  (declare (indent 1))
  (-let [varlist (cl-map 'list #'identity vars)]
    `(let ,varlist ,@body)))

(defmacro -let*- (vars &rest body)
  "Same as `let*', but VARS is an array.
Otherwise just delegates VARS and BODY to `let*'."
  (declare (indent 1))
  (-let [varlist (cl-map 'list #'identity vars)]
    `(let* ,varlist ,@body)))

(defmacro -pcase (exp &rest cases)
  "Same as `pcase', except that the match arms are vectors.
Otherwise just delegates EXP and CASES to `pcase'."
  (declare (indent 1))
  (let (cases-list)
    (--each cases
      (let (c)
        (dotimes (x (length it))
          (push (aref it x) c))
        (push  (nreverse c) cases-list)))
    `(pcase ,exp ,@(nreverse cases-list))))

(defmacro treemacs--without-following (&rest body)
  "Execute BODY with `treemacs--ready-to-follow' set to nil."
  ;; no warnings since `treemacs--ready-to-follow' is defined in treemacs-follow-mode.el
  ;; and should stay there since this file is for macros only
  `(let ((o (with-no-warnings treemacs--ready-to-follow)))
     (with-no-warnings (setq treemacs--ready-to-follow nil))
     (unwind-protect
         (progn ,@body)
       (with-no-warnings (setq treemacs--ready-to-follow o)))))

(cl-defmacro treemacs--execute-button-action
    (&key save-window
          ensure-window-split
          split-function
          window
          dir-action
          file-action
          tag-action
          no-match-explanation)
  "Infrastructure macro for setting up actions on different button states.
Fetches the currently selected button and verifies it's in the correct state
based on the given state actions.
If it isn't it will log NO-MATCH-EXPLANATION, if it is it selects WINDOW (or
`next-window' if none is given) and splits it with SPLIT-FUNCTION if given.
DIR-ACTION, FILE-ACTION, and TAG-ACTION are inserted into a `pcase' statement
matching the buttons state.
If ENSURE-WINDOW-SPLIT is t treemacs will vertically split the window if
treemacs is the only window to make sure a buffer is opened next to it, not
under or below it."
  (let ((valid-states (list)))
    (when dir-action
      (push 'dir-node-open valid-states)
      (push 'dir-node-closed valid-states))
    (when file-action
      (push 'file-node-open valid-states)
      (push 'file-node-closed valid-states))
    (when tag-action
      (push 'tag-node valid-states))
    `(-when-let (btn (treemacs-current-button))
       (treemacs--without-following
        (let* ((state (button-get btn 'state))
               (current-window (selected-window)))
          (if (not (memq state ',valid-states))
              (treemacs--log "%s" ,no-match-explanation)
            (progn
              ,@(if ensure-window-split
                    `((when (one-window-p)
                        (save-selected-window
                          (split-window nil nil (if (eq 'left treemacs-position) 'right 'left))))))
              (select-window (or ,window (next-window (selected-window) nil nil)))
              ,@(if split-function
                    `((funcall ,split-function)
                      (other-window 1)))
	      ;; Return the result of the action
              (prog1 (pcase state
                       ,@(when dir-action
			   `(((or `dir-node-open `dir-node-closed)
			      ,dir-action)))
                       ,@(when file-action
			   `(((or `file-node-open `file-node-closed)
			      ,file-action)))
                       ,@(when tag-action
			   `((`tag-node
			      ,tag-action)))
                       (_ (error "No match achieved even though button's state %s was part of the set of valid states %s"
				 state ',valid-states)))
		(when ,save-window
                  (select-window current-window))))))))))

(provide 'treemacs-macros)

;;; treemacs-macros.el ends here
