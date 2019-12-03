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
;;; Module that handles uniquely associating treemacs buffers with a certain scope,
;;; like the selected frame, or (to be implemented later) the active eyebrowse or
;;; persp desktop.
;;; This is implemented using a (somewhat) OOP style with eieio and static functions,
;;; where each scope type is expected to know how to query the current scope (e.g. the
;;; selected frame) and how to set up and tear down itself (e.g. deleting a frames
;;; associated buffer when the frame is deleted)

;;; Code:

(require 'dash)
(require 'eieio)
(require 'treemacs-core-utils)
(require 'treemacs-macros)

(treemacs-import-functions-from "treemacs-filewatch-mode"
  treemacs--stop-filewatch-for-current-buffer)

(treemacs-import-functions-from "treemacs-visuals"
  treemacs--tear-down-icon-highlight)

(defvar treemacs--current-scope-type 'treemacs-frame-scope
  "The general type of objects/items treemacs is curretly scoped to.")

(defvar treemacs--buffer-storage nil
  "Alist of all active treemacs buffers mapped to their scopes.")

(define-inline treemacs--all-scopes-and-buffers ()
  "Return `treemacs--buffer-storage'."
  (inline-quote treemacs--buffer-storage))

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
  (add-hook 'delete-frame-functions #'treemacs--remove-buffer-for-scope))

(cl-defmethod treemacs-scope->cleanup ((_ (subclass treemacs-frame-scope)))
  (remove-hook 'delete-frame-functions #'treemacs--remove-buffer-for-scope))

(defun treemacs--on-buffer-kill ()
  "Cleanup to run when a treemacs buffer is killed."
  ;; stop watch must come first since we need a reference to the killed buffer
  ;; to remove it from the filewatch list
  (treemacs--stop-filewatch-for-current-buffer)
  (treemacs--tear-down-icon-highlight)
  (setf treemacs--buffer-storage
        (--reject-first (eq (cdr it) (current-buffer)) treemacs--buffer-storage)))

(defun treemacs--remove-buffer-for-scope (scope)
  "Kill and remove the buffer assigned to the given SCOPE."
  (--when-let (cdr (assoc scope treemacs--buffer-storage))
    (kill-buffer it))
  (setf treemacs--buffer-storage
        (--reject-first (equal (car it) scope) treemacs--buffer-storage)))

(defun treemacs--create-buffer-for-scope (scope)
  "Create and store a new buffer for the given SCOPE."
  (setf treemacs--buffer-storage (assoc-delete-all scope treemacs--buffer-storage))
  (let* ((name-suffix (or (treemacs-scope->current-scope-name treemacs--current-scope-type scope)
                          (prin1-to-string scope)))
         (name (format "%sFramebuffer-%s*" treemacs--buffer-name-prefix name-suffix))
         (buffer (get-buffer-create name)))
    (push (cons scope buffer) treemacs--buffer-storage)
    buffer))

(defun treemacs--select-visible-window ()
  "Switch to treemacs buffer, given that it is currently visible."
  (->> treemacs--buffer-storage
       (assoc (treemacs-scope->current-scope treemacs--current-scope-type))
       (cdr)
       (get-buffer-window)
       (select-window))
  (run-hooks 'treemacs-select-hook))

(defun treemacs-get-local-buffer ()
  "Return the treemacs buffer local to the current scope-type.
Returns nil if no such buffer exists.."
  (declare (side-effect-free t))
  (let* ((scope (treemacs-scope->current-scope treemacs--current-scope-type))
         (buffer (->> treemacs--buffer-storage
                      (assoc scope)
                      (cdr))))
    (and (buffer-live-p buffer) buffer)))

(defun treemacs-get-local-buffer-create ()
  "TODO."
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
