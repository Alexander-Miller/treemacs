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

;; Basically this: https://github.com/Alexander-Miller/treemacs/issues/143.

;;; Code:

(require 'ht)
(require 'dash)
(require 's)

(eval-when-compile
  (require 'cl-lib)
  (require 'inline)
  (require 'treemacs-macros))

(eval-when-compile
  (cl-declaim (optimize (speed 3) (safety 0))))

(defvar-local treemacs-dom nil)

(cl-defstruct (treemacs-dom-node
               (:conc-name treemacs-dom-node->)
               (:constructor treemacs-dom-node->create!))
  key
  parent
  children
  reentry-nodes
  position
  refresh-flag
  collapse-keys)

;; needed because simple declare-function for pos slot in core-utils wont properly expand via setf
(define-inline treemacs-dom-node->set-position! (self value)
  "Set `position' field of SELF to VALUE.

SELF: Dom Node Struct
VALUE: Marker"
  (inline-letevals (self value)
    (inline-quote
     (setf (treemacs-dom-node->position ,self) ,value))))

(defun treemacs--reset-dom ()
  "Reset the dom."
  (setf treemacs-dom (make-hash-table :size 1000 :test 'equal)))

(define-inline treemacs-find-in-dom (key)
  "Get node with KEY, if any.

KEY: Node Path"
  (declare (side-effect-free t))
  (inline-letevals (key)
    (inline-quote
     (ht-get treemacs-dom ,key))))

(define-inline treemacs-dom-node->insert-into-dom! (self)
  "Insert SELF into the dom.

SELF: Dom Node Struct"
  (inline-letevals (self)
    (inline-quote
     (ht-set! treemacs-dom (treemacs-dom-node->key ,self) ,self))))

(define-inline treemacs-dom-node->add-child! (self child)
  "Add CHILD to to the children of SELF."
  (inline-letevals (self child)
    (inline-quote
     (setf (treemacs-dom-node->children ,self)
           (cons ,child (treemacs-dom-node->children ,self))))))

(define-inline treemacs-dom-node->remove-from-dom! (self)
  "Remove SELF from the dom.

SELF: Dom Node Struct"
  (inline-letevals (self)
    (inline-quote
     (progn
       (ht-remove! treemacs-dom (treemacs-dom-node->key ,self))
       (let ((parent (treemacs-dom-node->parent ,self)))
         (setf (treemacs-dom-node->children parent)
               (delete ,self (treemacs-dom-node->children parent))))
       (dolist (key (treemacs-dom-node->collapse-keys ,self))
         (ht-remove! treemacs-dom key))))))

(define-inline treemacs-dom-node->remove-collapse-keys! (self keys)
  "Remove the given collapse KEYS from both SELF and the dom."
  (inline-letevals (self keys)
    (inline-quote
     (progn
       (dolist (key ,keys)
         (ht-remove! treemacs-dom key))
       (setf (treemacs-dom-node->collapse-keys ,self)
             (--reject (member it ,keys) (treemacs-dom-node->collapse-keys ,self)))))))

(define-inline treemacs-dom-node->all-parents (self)
  "Get all parent nodes of SELF.
List will be sorted top to bottom.

SELF: Dom Node Struct"
  (declare (side-effect-free t))
  (inline-letevals (self)
    (inline-quote
     (let ((parent (treemacs-dom-node->parent ,self))
           (ret))
       (while parent
         (push parent ret)
         (setf parent (treemacs-dom-node->parent parent)))
       ret))))

(define-inline treemacs-on-expand (key pos)
  "Re-arrange the dom when node at KEY with POS is expanded.

KEY: Node Path
POS: Marker"
  (inline-letevals (key pos)
    (inline-quote
     (-if-let (dom-node (treemacs-find-in-dom ,key))
         (progn
           (setf (treemacs-dom-node->position dom-node) ,pos)
           (dolist (collapse-key (treemacs-dom-node->collapse-keys dom-node))
             (setf (treemacs-dom-node->position (treemacs-find-in-dom collapse-key)) ,pos))
           (-when-let (parent-dom-node (treemacs-dom-node->parent dom-node))
             (setf (treemacs-dom-node->reentry-nodes parent-dom-node)
                   (cons dom-node (treemacs-dom-node->reentry-nodes parent-dom-node)))))
       ;; expansion of root
       (setf dom-node (treemacs-dom-node->create! :key ,key :position ,pos))
       (treemacs-dom-node->insert-into-dom! dom-node)))))

(define-inline treemacs-on-collapse (key &optional purge)
  "Re-arrange the dom when node at KEY was collapsed.
Will remove NODE's parent/child link and invalidate the position and refresh
data of NODE and all its children.  When PURGE is non-nil will instead remove
NODE and its children from the dom.

KEY: Node Path
Purge: Boolean"
  (inline-letevals (key purge)
    (inline-quote
     (let* ((dom-node (treemacs-find-in-dom ,key))
            (children (treemacs-dom-node->children dom-node)))
       (-when-let (parent-dom-node (treemacs-dom-node->parent dom-node))
         (setf (treemacs-dom-node->reentry-nodes parent-dom-node)
               (delete dom-node (treemacs-dom-node->reentry-nodes parent-dom-node))))
       (cond
        (,purge
         (treemacs--on-purged-collapse dom-node))
        (children
         (treemacs--on-collapse-of-node-with-children dom-node))
        (t
         (treemacs--on-collapse-of-node-without-children dom-node)))))))

(define-inline treemacs--on-purged-collapse (dom-node)
  "Run when a DOM-NODE is collapsed with a purge (prefix) argument.
Will remove all the children of DOM-NODE from the dom.

DOM-NODE: Dom Node Struct"
  (inline-letevals (dom-node)
    (inline-quote
     (progn
       (treemacs-walk-dom-exclusive ,dom-node
         (lambda (it) (treemacs-dom-node->remove-from-dom! it)))
       (setf (treemacs-dom-node->children ,dom-node) nil
             (treemacs-dom-node->reentry-nodes ,dom-node) nil)))))

(define-inline treemacs--on-collapse-of-node-without-children (dom-node)
  "Run when a DOM-NODE without any children is collapsed.
Will remove DOm-NODE from its parent's reentry list.

DOM-NODE: Dom Node Struct"
  (inline-letevals (dom-node)
    (inline-quote
     (let ((parent-dom-node (treemacs-dom-node->parent ,dom-node)))
       (when parent-dom-node
         (setf (treemacs-dom-node->reentry-nodes parent-dom-node)
               (delete ,dom-node (treemacs-dom-node->reentry-nodes parent-dom-node))))))))

(define-inline treemacs--on-collapse-of-node-with-children (dom-node)
  "Run when a DOM-NODE with children is collapsed.
Will remove all entries below the one collapsed from the dom.

DOM-NODE: Dom Node Struct"
  (inline-letevals (dom-node)
    (inline-quote
     (progn
       (treemacs-walk-dom-exclusive ,dom-node
         (lambda (it)
           (treemacs-dom-node->remove-from-dom! it)
           (setf (treemacs-dom-node->children it) nil)))
       (setf (treemacs-dom-node->children ,dom-node) nil)))))

(defun treemacs--on-rename (old-name new-name dont-rename-initial)
  "Renames dom entries after a file was renamed from OLD-NAME to NEW-NAME.
Renames the initial dom entry (the one backing the file that was actually
renamed) only if DONT-RENAME-INITIAL is nil in case the entry is required for
filewatch-mode to work.

OLD-NAME: File Path | Tag Path
NEW-NAME: File Path | Tag Path
DONT-RENAME-INITIAL: Boolean"
  (-when-let (dom-node (treemacs-find-in-dom old-name))
    (-let [migrate-keys
           (lambda (it)
             (let* ((old-key (treemacs-dom-node->key it))
                    (new-key (cond
                              ((stringp old-key)
                               (s-replace old-name new-name old-key))
                              ((and (consp old-key) (stringp (car old-key)))
                               (cons (s-replace old-name new-name (car old-key)) (cdr old-key))))))
               (when new-key
                 (ht-remove! treemacs-dom old-key)
                 (ht-set! treemacs-dom new-key it)
                 (setf (treemacs-dom-node->key it) new-key))))]
      ;; when filewatch is enabled the acutally renamed file needs to keep
      ;; its dom entry until refresh actually runs so it can be deleted properly
      (if dont-rename-initial
          (progn
            (treemacs-walk-reentry-dom-exclusive dom-node migrate-keys)
            (treemacs-walk-dom-exclusive dom-node migrate-keys))
        (treemacs-walk-dom dom-node migrate-keys)
        (treemacs-walk-reentry-dom dom-node migrate-keys)))))

(defun treemacs-walk-dom (node fn)
  "Recursively walk the dom starting at NODE.
Calls FN on every node encountered in a depth-first pattern, starting with the
deepest.  This assures that FN may destructively modify the dom, at least on
levels the one currently visiting.

NODE: Dom Node Struct
FN: (Dom Node) -> Any"
  (declare (indent 1))
  (-let [children (treemacs-dom-node->children node)]
    (funcall fn node)
    (dolist (it children)
      (treemacs-walk-dom it fn))))

(defun treemacs-walk-dom-exclusive (node fn)
  "Same as `treemacs-walk-dom', but start NODE will not be passed to FN.

NODE: Dom Node Struct
FN: (Dom Node) -> Any"
  (declare (indent 1))
  (dolist (it (treemacs-dom-node->children node))
    (treemacs-walk-dom it fn)))

(defun treemacs-walk-reentry-dom (node fn)
  "Recursively walk the dom starting at NODE.
Unlike `treemacs-walk-dom' only expanded nodes are selected.

Calls FN on every node encountered in a depth-first pattern, starting with the
deepest.  This assures that FN may destructively modify the dom, at least on
levels the one currently visiting.

NODE: Dom Node Struct
FN: (Dom Node) -> Any"
  (declare (indent 1))
  (funcall fn node)
  (dolist (it (treemacs-dom-node->reentry-nodes node))
    (treemacs-walk-reentry-dom it fn)))

(defun treemacs-walk-reentry-dom-exclusive (node fn)
  "Same as `treemacs-walk-reentry-dom', but start NODE will not be passed to FN.

NODE: Dom Node Struct
FN: (Dom Node) -> Any"
  (declare (indent 1))
  (dolist (it (treemacs-dom-node->reentry-nodes node))
    (treemacs-walk-reentry-dom it fn)))

(provide 'treemacs-dom)

;;; treemacs-dom.el ends here
