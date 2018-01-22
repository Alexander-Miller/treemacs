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

;;; Code:

(require 'ht)
(require 's)
(require 'dash)
(require 'treemacs-impl)
(eval-and-compile
  (require 'cl-lib)
  (require 'treemacs-macros))

(defvar-local treemacs-shadow-index nil)

;; don't need all that extra indirection of type checking
(defsubst treemacs-shadow-node->key (node)
  "Get the key of NODE."
  (aref node 1))
(defsubst treemacs-shadow-node->parent (node)
  "Get the parent node of NODE."
  (aref node 2))
(defsubst treemacs-shadow-node->children (node)
  "Get the child nodes of NODE."
  (aref node 3))
(defsubst treemacs-shadow-node->position (node)
  "Get the position marker of NODE."
  (aref node 4))
(defsubst treemacs-shadow-node->collapsed (node)
  "Get the collapse info of NODE."
  (aref node 5))

(with-no-warnings
  (cl-defstruct (treemacs-shadow-node (:conc-name treemacs-shadow-node->))
    key parent children position collapsed))

(defun treemacs--reset-index (root)
  "Reset the node index and reinitialize it at ROOT."
  (setq treemacs-shadow-index (make-hash-table :size 300 :test #'equal))
  (-let [node (make-treemacs-shadow-node :key root :position (point-min))]
    (ht-set! treemacs-shadow-index root node)))

(defun treemacs-shadow-node->print (node)
  "Pretty print NODE.
Debug function"
  (message
   "Node %s\nChildren: %s\nOwner: %s\nPosition: %s\nCollapsed: %s"
   (treemacs-shadow-node->key node)
   (-map #'treemacs-shadow-node->key (treemacs-shadow-node->children node))
   (--if-let (treemacs-shadow-node->parent node) (treemacs-shadow-node->key it) "NONE")
   (--if-let (treemacs-shadow-node->position node) it "NONE")
   (treemacs-shadow-node->collapsed node)))

(defsubst treemacs-get-position-of (key)
  "Get the position of node with KEY, if any."
  (--when-let (ht-get treemacs-shadow-index key)
    (treemacs-shadow-node->position it)))

(defsubst treemacs-get-from-shadow-index (key)
  "Get node with KEY, if any."
  (ht-get treemacs-shadow-index key))

(defun treemacs-shadow-node->invalidate-pos (node)
  "Set the pos field of NODE to nil."
  (setf (treemacs-shadow-node->position node) nil))

(defun treemacs-shadow-node->remove-from-index (node)
  "Remove NODE from the index."
  (ht-remove treemacs-shadow-index (treemacs-shadow-node->key node)))

(defsubst treemacs--on-expanding-existing-node (node pos)
  "Run when existing NODE is expanded.
Sets its POS info and collpase field to nil."
  (setf (treemacs-shadow-node->position node) pos
        (treemacs-shadow-node->collapsed node) nil))

(defsubst treemacs--on-expanding-new-node (key pos parent-key)
  "When node at KEY is expanded and does not yet exist in the index.
Creates a new node for KEY at POS with parent at PARENT-KEY and inserts it in
the index."
  (-let*- [(parent (treemacs-get-from-shadow-index parent-key))
           (new-node (make-treemacs-shadow-node :key key :parent parent :position pos))]
    (setf (treemacs-shadow-node->children parent) (cons new-node (treemacs-shadow-node->children parent))
          (treemacs-shadow-node->parent new-node) parent)
    (ht-set! treemacs-shadow-index key new-node)))

(defun treemacs-on-expand (key pos parent-key)
  "Routine to run when a node is expanded.
Sets up a new node for KEY and POS and parent at PARENT-KEY or resurrects an
already present node by setting its POS and marking at as no longer collapsed."
  (--if-let (ht-get treemacs-shadow-index key)
      (treemacs--on-expanding-existing-node it pos)
    (treemacs--on-expanding-new-node key pos parent-key)))

(defun treemacs--do-for-all-child-nodes (node f)
  "Recursively iterate over NODE and its children and run F on every one of them."
  (declare (indent 1))
  (funcall f node)
  (dolist (child (treemacs-shadow-node->children node))
    (treemacs--do-for-all-child-nodes child f)))

(defsubst treemacs--on-collapse-of-node-with-children (node purge)
  "Collapse a NODE that has children below it.
If PURGE is non-nil remove NODE's branch from the shadow tree.
Otherwise mark NODE as collapsed and invalidate the position data of NODE's branch."
  (if purge
      ;; almost same as if the node did not have children - throw out of index and parent,
      ;; but remove the children as well
      (-let [parent (treemacs-shadow-node->parent node)]
        (setf (treemacs-shadow-node->children parent) (delete node (treemacs-shadow-node->children parent)))
        (treemacs--do-for-all-child-nodes node
          #'treemacs-shadow-node->remove-from-index))
    ;; otherwise set the node to be collapsed and delete the lower tiers' position info
    (setf (treemacs-shadow-node->collapsed node) t)
    (treemacs--do-for-all-child-nodes node
      #'treemacs-shadow-node->invalidate-pos)))

(defun treemacs-on-collapse (key &optional purge)
  "Routine to run when node at KEY is closed again or deleted.
Will remove NODE's parent/child link and invalidate the position cache of NODE
and all its children. When PURGE is non-nil will instead remove NODE and its
children from the index."
  ;; need to check for nil, since this code also runs on deletion of files or closed dirs
  ;; which were never part of the index
  (-when-let- [node (treemacs-get-from-shadow-index key)]
    (if (null (treemacs-shadow-node->children node))
        ;; no children - just throw the node out of the index and its parent
        (-let [parent (treemacs-shadow-node->parent node)]
          (setf (treemacs-shadow-node->children parent) (delete node (treemacs-shadow-node->children parent)))
          (ht-remove treemacs-shadow-index key))
      (treemacs--on-collapse-of-node-with-children node purge))))

(defun treemacs--on-rename (old-name new-name)
  "Routine to run after a file was renamed from OLD-NAME to NEW-NAME."
  (-let [root (treemacs-get-from-shadow-index (treemacs--current-root))]
    (treemacs--do-for-all-child-nodes root
      (lambda (it)
        (setf (treemacs-shadow-node->key it) (s-replace old-name new-name (treemacs-shadow-node->key it)))))))

(defun treemacs--reroot-up (old-key new-key)
  "Routine to run when root is changed upwards from OLD-KEY to NEW-KEY.
Creates a new shadow node for the new root if necessary and sets the
parent/child link."
  (-let*- [(old-root (treemacs-get-from-shadow-index old-key))
           (new-root (or (ht-get treemacs-shadow-index new-key)
                         (make-treemacs-shadow-node :key new-key :position (point-min))))]
    (setf (treemacs-shadow-node->parent old-root) new-root
          (treemacs-shadow-node->position old-root) nil)
    (ht-set! treemacs-shadow-index new-key new-root)))

(defun treemacs--reroot-down (old-key new-key)
  "Routine to run when root is changed downwards from OLD-KEY to NEW-KEY.
Creates a new shadow node for the new root if necessary and resets both new and
old root's positions."
  (-let- [(old-root (treemacs-get-from-shadow-index old-key))
          (new-root (or (treemacs-get-from-shadow-index new-key)
                        (make-treemacs-shadow-node :key)))]
    (setf (treemacs-shadow-node->position new-root) nil
          (treemacs-shadow-node->position old-root) nil)
    (ht-set! treemacs-shadow-index new-key new-root)))

(defun treemacs--invalidate-position-cache ()
  "Invalidate the position of all nodes in the index."
  (maphash
   (lambda (_ node) (treemacs-shadow-node->invalidate-pos node))
   treemacs-shadow-index))

(provide 'treemacs-structure)

;;; treemacs-structure.el ends here
