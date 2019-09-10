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
;; Basically this: https://github.com/Alexander-Miller/treemacs/issues/143

;;; Code:

(require 'ht)
(require 's)
(require 'dash)
(require 'treemacs-core-utils)
(eval-and-compile
  (require 'cl-lib)
  (require 'inline)
  (require 'treemacs-macros))

(treemacs-import-functions-from "treemacs"
  treemacs-refresh)

(treemacs-import-functions-from "treemacs-workspaces"
  treemacs-workspace->projects
  treemacs-project->path
  treemacs-current-workspace)

(defvar-local treemacs-dom nil)

(treemacs--defstruct treemacs-dom-node
  key parent children position closed refresh-flag collapse-keys)

(define-inline treemacs--insert-into-dom (node)
  "Insert NODE into the dom."
  (inline-letevals (node)
    (inline-quote
     (ht-set! treemacs-dom (treemacs-dom-node->key ,node) ,node))))

(defun treemacs--reset-dom ()
  "Reset the dom."
  (setq treemacs-dom (make-hash-table :size 300 :test #'equal)))

(defun treemacs-dom-node->print! (self)
  "Pretty print SELF.
Debug function."
  (message
   "Node %s\nChildren: %s\nOwner: %s\nPosition: %s\nClosed: %s"
   (treemacs-dom-node->key self)
   (-map #'treemacs-dom-node->key (treemacs-dom-node->children self))
   (--if-let (treemacs-dom-node->parent self) (treemacs-dom-node->key it) "NONE")
   (--if-let (treemacs-dom-node->position self) it "NONE")
   (treemacs-dom-node->closed self)))

(define-inline treemacs-get-position-of (key)
  "Get the position of node with KEY, if any."
  (declare (side-effect-free t))
  (inline-letevals (key)
    (inline-quote
     (--when-let (ht-get treemacs-dom ,key)
       (treemacs-dom-node->position it)))))

(define-inline treemacs-find-in-dom (key)
  "Get node with KEY, if any."
  (declare (side-effect-free t))
  (inline-letevals (key)
    (inline-quote
     (ht-get treemacs-dom ,key))))

(define-inline treemacs-dom-node->all-parents (self)
  "Get all parent nodes of SELF."
  (declare (side-effect-free t))
  (inline-letevals (self)
    (inline-quote
     (let ((parent (treemacs-dom-node->parent ,self))
           (ret))
       (while parent
         (push parent ret)
         (setf parent (treemacs-dom-node->parent parent)))
       ret))))

(define-inline treemacs-dom-node->invalidate-pos! (self)
  "Set the pos field of SELF to nil."
  (inline-letevals (self)
    (inline-quote
     (setf (treemacs-dom-node->position ,self) nil))))

(define-inline treemacs-dom-node->reset-refresh-flag! (self)
  "Set the refresh flag of SELF to nil."
  (inline-letevals (self)
    (inline-quote
     (setf (treemacs-dom-node->refresh-flag ,self) nil))))

(define-inline treemacs-dom-node->remove-from-dom! (self)
  "Remove single SELF from the dom."
  (inline-letevals (self)
    (inline-quote
     (progn
       (ht-remove treemacs-dom (treemacs-dom-node->key ,self))
       (dolist (coll-key (treemacs-dom-node->collapse-keys ,self))
         (ht-remove treemacs-dom coll-key))))))

(define-inline treemacs--on-expanding-existing-node (node pos)
  "Run when existing NODE is expanded.
Sets its POS info and collpase field to nil."
  (inline-letevals (node pos)
    (inline-quote
     (prog1 ,node
       (setf (treemacs-dom-node->position ,node) ,pos
             (treemacs-dom-node->closed ,node) nil)))))

(define-inline treemacs--on-expanding-new-node (key pos parent-key)
  "When node at KEY is expanded and does not yet exist in the dom.
Creates a new node for KEY at POS with parent at PARENT-KEY and inserts it in
the dom."
  (inline-letevals (key pos parent-key)
    (inline-quote
     (let* ((parent (treemacs-find-in-dom ,parent-key))
            (new-node (make-treemacs-dom-node :key ,key :parent parent :position ,pos)))
       (when parent
         (setf (treemacs-dom-node->children parent) (cons new-node (treemacs-dom-node->children parent))))
       (setf (treemacs-dom-node->parent new-node) parent)
       (ht-set! treemacs-dom ,key new-node)
       new-node))))

(defun treemacs-on-expand (key pos parent-key)
  "Routine to run when a node is expanded.
Sets up a new node for KEY and POS and parent at PARENT-KEY or resurrects an
already present node by setting its POS and marking at as no longer closed."
  (-let [dom-node
         (--if-let (ht-get treemacs-dom key)
             (treemacs--on-expanding-existing-node it pos)
           (treemacs--on-expanding-new-node key pos parent-key))]
    (dolist (step (cdr (treemacs-button-get pos :collapsed)))
      (ht-set! treemacs-dom step dom-node)
      (push step (treemacs-dom-node->collapse-keys dom-node)))))

(defun treemacs--do-for-all-child-nodes (node f)
  "Recursively iterate over NODE and its children and run F on every one of them."
  (declare (indent 1))
  (funcall f node)
  (dolist (child (treemacs-dom-node->children node))
    (treemacs--do-for-all-child-nodes child f)))

(define-inline treemacs--on-collapse-of-node-with-children (node purge)
  "Collapse a NODE that has children below it.
If PURGE is non-nil remove NODE's branch from the dom.
Otherwise mark NODE as closed and invalidate the position and refresh data of
NODE's branch."
  (inline-letevals (node purge)
    (inline-quote
     (if ,purge
         ;; almost same as if the node did not have children - throw out of dom and parent,
         ;; but remove the children as well
         (let ((parent (treemacs-dom-node->parent ,node)))
           (when parent
             (setf (treemacs-dom-node->children parent)
                   (delete ,node (treemacs-dom-node->children parent))))
           (treemacs--do-for-all-child-nodes ,node
             #'treemacs-dom-node->remove-from-dom!))
       ;; otherwise set the node to be closed and reset lower tiers' pos and refresh info
       (setf (treemacs-dom-node->closed ,node) t)
       (treemacs--do-for-all-child-nodes ,node
         (lambda (it)
           (treemacs-dom-node->invalidate-pos! it)
           (treemacs-dom-node->reset-refresh-flag! it)))))))

(defun treemacs-on-collapse (key &optional purge)
  "Routine to run when node at KEY is closed again or deleted.
Will remove NODE's parent/child link and invalidate the position and refresh
data of NODE and all its children. When PURGE is non-nil will instead remove
NODE and its children from the dom."
  ;; need to check for nil, since this code also runs on deletion of files or closed dirs
  ;; which were never part of the dom
  (-when-let (dom-node (treemacs-find-in-dom key))
    (if (null (treemacs-dom-node->children dom-node))
        ;; no children - just throw the dom-node out of the dom and its parent
        (-let [parent (treemacs-dom-node->parent dom-node)]
          (when parent
            (setf (treemacs-dom-node->children parent)
                  (delete dom-node (treemacs-dom-node->children parent))))
          (ht-remove! treemacs-dom key)
          ;; remove additional dom entries for flattened dirs
          (dolist (step (treemacs-dom-node->collapse-keys dom-node))
            (ht-remove! treemacs-dom step))
          (setf (treemacs-dom-node->collapse-keys dom-node) nil))
      (treemacs--on-collapse-of-node-with-children dom-node purge))))

(defun treemacs--on-rename (old-name new-name)
  "Routine to run after a file was renamed from OLD-NAME to NEW-NAME."
  (-when-let (node (treemacs-find-in-dom old-name))
    (treemacs--do-for-all-child-nodes node
      (lambda (it)
        (let* ((old-key (treemacs-dom-node->key it))
               (new-key nil))
          ;; keys of tags are tag paths
          (setq new-key
                (if (stringp old-key)
                    (s-replace old-name new-name old-key)
                  (-let [(tag file . path) old-key]
                    (nconc (list tag (s-replace old-name new-name file)) path))))
          (ht-remove! treemacs-dom old-key)
          (ht-set! treemacs-dom new-key it)
          (setf (treemacs-dom-node->key it) new-key))))))

(define-inline treemacs--invalidate-position-cache ()
  "Invalidate the position of all nodes in the dom."
  (inline-quote
   (treemacs--maphash treemacs-dom (_ node)
     (treemacs-dom-node->invalidate-pos! node))))

(provide 'treemacs-dom)

;;; treemacs-dom.el ends here
