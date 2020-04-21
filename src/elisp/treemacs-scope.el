;;; treemacs.el --- A tree style file viewer package -*- lexical-binding: t -*-

;; Copyright (C) 2020 Alexander Miller

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
;;; Module that handles uniquely associating treemacs buffers with a certain scope,
;;; like the selected frame, or (to be implemented later) the active eyebrowse or
;;; persp desktop.
;;; This is implemented using a (somewhat) OOP style with eieio and static functions,
;;; where each scope type is expected to know how to query the current scope (e.g. the
;;; selected frame) and how to set up and tear down itself (e.g. deleting a frames
;;; associated buffer when the frame is deleted)

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'eieio)
(require 'treemacs-core-utils)
(require 'treemacs-macros)
(require 's)
(require 'inline)

(treemacs-import-functions-from "treemacs-filewatch-mode"
  treemacs--stop-filewatch-for-current-buffer)

(treemacs-import-functions-from "treemacs-visuals"
  treemacs--tear-down-icon-highlight)

(treemacs-import-functions-from "treemacs-interface"
  treemacs-quit
  treemacs-select-window)

(treemacs-import-functions-from "treemacs-workspaces"
  treemacs--find-workspace)

(treemacs--defstruct treemacs-scope-shelf buffer workspace)

(defvar treemacs-scope-types (list (cons 'Frames 'treemacs-frame-scope))
  "List of all known scope types.
The car is the name seen in interactive selection.  The cdr is the eieio class
name.")

(defvar treemacs--current-scope-type 'treemacs-frame-scope
  "The general type of objects/items treemacs is curretly scoped to.")

(defvar treemacs--scope-storage nil
  "Alist of all active scopes mapped to their buffers & workspaces.
The car is the scope, the cdr is a `treemacs-scope-shelf'.")

(define-inline treemacs-scope-shelf->kill-buffer (self)
  "Kill the buffer stored in SELF."
  (inline-letevals (self)
    (inline-quote
     (progn
       (let ((buffer (treemacs-scope-shelf->buffer ,self)))
         (when (buffer-live-p buffer) (kill-buffer buffer)))
       (setf (treemacs-scope-shelf->buffer ,self) nil)))))

(define-inline treemacs--scope-store ()
  "Return `treemacs--scope-storage'."
  (inline-quote treemacs--scope-storage))

(define-inline treemacs-current-scope-type ()
  "Return the current scope type."
  (declare (side-effect-free t))
  (inline-quote treemacs--current-scope-type))

(define-inline treemacs-current-scope ()
  "Return the current scope."
  (declare (side-effect-free t))
  (inline-quote
   (treemacs-scope->current-scope (treemacs-current-scope-type))))

(define-inline treemacs-current-scope-shelf (&optional scope)
  "Return the current scope shelf, containing the active workspace and buffer.
Use either the given SCOPE or `treemacs-current-scope' otherwise.

Can be used with `setf'."
  (declare (side-effect-free t))
  (inline-letevals (scope)
    (inline-quote
     (cdr (assoc (or ,scope (treemacs-current-scope)) treemacs--scope-storage)))))
(gv-define-setter treemacs-current-scope-shelf (val)
  `(let* ((current-scope (treemacs-current-scope))
          (shelf-mapping (assoc current-scope treemacs--scope-storage)))
     (if (cdr shelf-mapping)
         (setf (cdr shelf-mapping) ,val)
       (push (cons current-scope ,val) treemacs--scope-storage))))

(defclass treemacs-scope () () :abstract t)

(cl-defmethod treemacs-scope->current-scope ((_ (subclass treemacs-scope)))
  (error "Default `current-scope' implementation was called"))

(cl-defmethod treemacs-scope->current-scope-name ((_ (subclass treemacs-scope)) scope)
  (ignore scope)
  nil)

(cl-defmethod treemacs-scope->setup ((_ (subclass treemacs-scope)))
  nil)

(cl-defmethod treemacs-scope->cleanup ((_ (subclass treemacs-scope)))
  nil)

(defclass treemacs-frame-scope (treemacs-scope) () :abstract t)

(cl-defmethod treemacs-scope->current-scope ((_ (subclass treemacs-frame-scope)))
  (selected-frame))

(cl-defmethod treemacs-scope->current-scope-name ((_ (subclass treemacs-frame-scope)) frame)
  (prin1-to-string frame))

(cl-defmethod treemacs-scope->setup ((_ (subclass treemacs-frame-scope)))
  (add-hook 'delete-frame-functions #'treemacs--on-scope-kill))

(cl-defmethod treemacs-scope->cleanup ((_ (subclass treemacs-frame-scope)))
  (remove-hook 'delete-frame-functions #'treemacs--on-scope-kill))

(defun treemacs-set-scope-type (new-scope-type)
  "Set a NEW-SCOPE-TYPE for treemacs buffers.
Valid values for TYPE are the `car's of the elements of `treemacs-scope-types'.

This is meant for programmatic use.  For an interactive selection see
`treemacs-select-buffer-scope-type'."
  (-let [class (alist-get new-scope-type treemacs-scope-types)]
    (unless class (user-error "'%s' is not a valid scope new-scope-type.  Valid types are: %s"
                              new-scope-type
                              (-map #'car treemacs-scope-types)))
    (treemacs--do-set-scope-type class)))

(defun treemacs--do-set-scope-type (new-scope-type)
  "Set NEW-SCOPE-TYPE as the scope managing class.
Kill all treemacs buffers and windows and reset the buffer store.

NEW-SCOPE-TYPE: T: treemacs-scope"
  (treemacs-scope->cleanup treemacs--current-scope-type)
  (setf treemacs--current-scope-type new-scope-type)
  (dolist (frame (frame-list))
    (dolist (window (window-list frame))
      (when (treemacs-is-treemacs-window? window)
        (delete-window window))))
  (dolist (it treemacs--scope-storage)
    (treemacs-scope-shelf->kill-buffer (cdr it)))
  (setf treemacs--scope-storage nil)
  (treemacs-scope->setup new-scope-type))

(defun treemacs--on-buffer-kill ()
  "Cleanup to run when a treemacs buffer is killed."
  ;; stop watch must come first since we need a reference to the killed buffer
  ;; to remove it from the filewatch list
  (treemacs--stop-filewatch-for-current-buffer)
  (treemacs--tear-down-icon-highlight)
  ;; not present for extension buffers
  (-when-let (shelf (treemacs-current-scope-shelf))
    (setf (treemacs-scope-shelf->buffer shelf) nil)))

(defun treemacs--on-scope-kill (scope)
  "Kill and remove the buffer assigned to the given SCOPE."
  (-when-let (shelf (treemacs-current-scope-shelf scope))
    (treemacs-scope-shelf->kill-buffer shelf)
    (setf treemacs--scope-storage (--reject-first (equal (car it) scope) treemacs--scope-storage))))

(defun treemacs--create-buffer-for-scope (scope)
  "Create and store a new buffer for the given SCOPE."
  (-let [shelf (treemacs-current-scope-shelf scope)]
    (unless shelf
      (setf shelf (make-treemacs-scope-shelf))
      (push (cons scope shelf) treemacs--scope-storage)
      (treemacs--find-workspace (buffer-file-name)))
    (treemacs-scope-shelf->kill-buffer shelf)
    (let* ((name-suffix (or (treemacs-scope->current-scope-name treemacs--current-scope-type scope)
                            (prin1-to-string scope)))
           (name (format "%sScoped-Buffer-%s*" treemacs--buffer-name-prefix name-suffix))
           (buffer (get-buffer-create name)))
      (setf (treemacs-scope-shelf->buffer shelf) buffer)
      buffer)))

(defun treemacs--change-buffer-on-scope-change (&rest _)
  "Switch the treemacs buffer after the current scope was changed."
  (--when-let (treemacs-get-local-window)
    (save-selected-window
      (with-selected-window it
        (treemacs-quit))
      (treemacs-select-window))))

(defun treemacs--select-visible-window ()
  "Switch to treemacs buffer, given that it is currently visible."
  (-some->> treemacs--scope-storage
            (assoc (treemacs-scope->current-scope treemacs--current-scope-type))
            (cdr)
            (treemacs-scope-shelf->buffer)
            (get-buffer-window)
            (select-window))
  (run-hooks 'treemacs-select-hook))

(defun treemacs-get-local-buffer ()
  "Return the treemacs buffer local to the current scope-type.
Returns nil if no such buffer exists.."
  (declare (side-effect-free t))
  (let* ((scope (treemacs-scope->current-scope treemacs--current-scope-type))
         (buffer (-some->> treemacs--scope-storage
                           (assoc scope)
                           (cdr)
                           (treemacs-scope-shelf->buffer))))
    (and (buffer-live-p buffer) buffer)))

(defun treemacs-get-local-buffer-create ()
  "Get the buffer for the current scope, creating a new one if needed."
  (or (treemacs-get-local-buffer)
      (treemacs--create-buffer-for-scope (treemacs-scope->current-scope treemacs--current-scope-type))))

(defun treemacs-get-local-window ()
  "Return the window displaying the treemacs buffer in the current frame.
Returns nil if no treemacs buffer is visible."
  (declare (side-effect-free error-free))
  (->> (window-list (selected-frame))
       (--first (->> it
                     (window-buffer)
                     (buffer-name)
                     (s-starts-with? treemacs--buffer-name-prefix)))))

(define-inline treemacs-current-visibility ()
  "Return whether the current visibility state of the treemacs buffer.
Valid states are 'visible, 'exists and 'none."
  (declare (side-effect-free t))
  (inline-quote
   (cond
    ((treemacs-get-local-window) 'visible)
    ((treemacs-get-local-buffer) 'exists)
    (t 'none))))

(treemacs-only-during-init
 (setf treemacs--current-scope-type 'treemacs-frame-scope)
 (treemacs-scope->setup 'treemacs-frame-scope))

(provide 'treemacs-scope)

;;; treemacs-scope.el ends here
