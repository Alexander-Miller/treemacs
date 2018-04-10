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
;; Basically this: https://github.com/Alexander-Miller/treemacs/issues/143

;;; Code:

(require 'ht)
(require 's)
(require 'dash)
(require 'treemacs-impl)
(eval-and-compile
  (require 'cl-lib)
  (require 'treemacs-macros))

(treemacs-import-functions-from "treemacs"
  treemacs-refresh)

(treemacs-import-functions-from "treemacs-workspaces"
  treemacs-workspace->projects
  treemacs-project->path
  treemacs-current-workspace)

(defvar-local treemacs-shadow-index nil)

;; don't need all that extra indirection of type checking
(defsubst treemacs-shadow-node->key (node)
  "Get the key of shadow NODE."
  (aref node 1))

(defsubst treemacs-shadow-node->parent (node)
  "Get the parent node of shadow NODE."
  (aref node 2))

(defsubst treemacs-shadow-node->children (node)
  "Get the child nodes of shadow NODE."
  (aref node 3))

(defsubst treemacs-shadow-node->position (node)
  "Get the position marker of shadow NODE."
  (aref node 4))

(defsubst treemacs-shadow-node->closed (node)
  "Get the closed status info of shadow NODE."
  (aref node 5))

(defsubst treemacs-shadow-node->refresh-flag (node)
  "Get the refresh flag info of shadow NODE."
  (aref node 6))

(with-no-warnings
  (cl-defstruct (treemacs-shadow-node (:conc-name treemacs-shadow-node->))
    key parent children position closed refresh-flag))

(defsubst treemacs--insert-shadow-node (node)
  "Insert NODE into the shadow index."
  (ht-set! treemacs-shadow-index (treemacs-shadow-node->key node) node))

(defun treemacs--reset-index ()
  "Reset the shadow index and."
  (setq treemacs-shadow-index (make-hash-table :size 300 :test #'equal)))

(defun treemacs-shadow-node->print (node)
  "Pretty print NODE.
Debug function."
  (message
   "Node %s\nChildren: %s\nOwner: %s\nPosition: %s\nClosed: %s"
   (treemacs-shadow-node->key node)
   (-map #'treemacs-shadow-node->key (treemacs-shadow-node->children node))
   (--if-let (treemacs-shadow-node->parent node) (treemacs-shadow-node->key it) "NONE")
   (--if-let (treemacs-shadow-node->position node) it "NONE")
   (treemacs-shadow-node->closed node)))

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

(defun treemacs-shadow-node->reset-refresh-flag (node)
  "Set the refresh flag of NODE to nil."
  (setf (treemacs-shadow-node->refresh-flag node) nil))

(defun treemacs-shadow-node->remove-from-index (node)
  "Remove NODE from the index."
  (ht-remove treemacs-shadow-index (treemacs-shadow-node->key node)))

(defsubst treemacs--on-expanding-existing-node (node pos)
  "Run when existing NODE is expanded.
Sets its POS info and collpase field to nil."
  (setf (treemacs-shadow-node->position node) pos
        (treemacs-shadow-node->closed node) nil))

(defsubst treemacs--on-expanding-new-node (key pos parent-key)
  "When node at KEY is expanded and does not yet exist in the index.
Creates a new node for KEY at POS with parent at PARENT-KEY and inserts it in
the index."
  (-let*- [(parent (treemacs-get-from-shadow-index parent-key))
           (new-node (make-treemacs-shadow-node :key key :parent parent :position pos))]
    (when parent
      (setf (treemacs-shadow-node->children parent) (cons new-node (treemacs-shadow-node->children parent))))
    (setf (treemacs-shadow-node->parent new-node) parent)
    (ht-set! treemacs-shadow-index key new-node)))

(defun treemacs-on-expand (key pos parent-key)
  "Routine to run when a node is expanded.
Sets up a new node for KEY and POS and parent at PARENT-KEY or resurrects an
already present node by setting its POS and marking at as no longer closed."
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
Otherwise mark NODE as closed and invalidate the position and refresh data of
NODE's branch."
  (if purge
      ;; almost same as if the node did not have children - throw out of index and parent,
      ;; but remove the children as well
      (-let [parent (treemacs-shadow-node->parent node)]
        (when parent
          (setf (treemacs-shadow-node->children parent)
                (delete node (treemacs-shadow-node->children parent))))
        (treemacs--do-for-all-child-nodes node
          #'treemacs-shadow-node->remove-from-index))
    ;; otherwise set the node to be closed and reset lower tiers' pos and refresh info
    (setf (treemacs-shadow-node->closed node) t)
    (treemacs--do-for-all-child-nodes node
      (lambda (it)
        (treemacs-shadow-node->invalidate-pos it)
        (treemacs-shadow-node->reset-refresh-flag it)))))

(defun treemacs-on-collapse (key &optional purge)
  "Routine to run when node at KEY is closed again or deleted.
Will remove NODE's parent/child link and invalidate the position and refresh
data of NODE and all its children. When PURGE is non-nil will instead remove
NODE and its children from the index."
  ;; need to check for nil, since this code also runs on deletion of files or closed dirs
  ;; which were never part of the index
  (-when-let- [node (treemacs-get-from-shadow-index key)]
    (if (null (treemacs-shadow-node->children node))
        ;; no children - just throw the node out of the index and its parent
        (-let [parent (treemacs-shadow-node->parent node)]
          (when parent
            (setf (treemacs-shadow-node->children parent)
                  (delete node (treemacs-shadow-node->children parent))))
          (ht-remove! treemacs-shadow-index key))
      (treemacs--on-collapse-of-node-with-children node purge))))

(defun treemacs--on-rename (old-name new-name)
  "Routine to run after a file was renamed from OLD-NAME to NEW-NAME."
  (-when-let- [node (treemacs-get-from-shadow-index old-name)]
    (treemacs--do-for-all-child-nodes node
      (lambda (it)
        (-let*- [(old-key (treemacs-shadow-node->key it))
                 (new-key nil)]
          ;; keys of tags are tag paths
          (setq new-key
                (if (stringp old-key)
                    (s-replace old-name new-name old-key)
                  (-let [(tag file . path) old-key]
                    (nconc (list tag (s-replace old-name new-name file)) path))))
          (ht-remove! treemacs-shadow-index old-key)
          (ht-set! treemacs-shadow-index new-key it)
          (setf (treemacs-shadow-node->key it) new-key))))))

(defun treemacs--invalidate-position-cache ()
  "Invalidate the position of all nodes in the index."
  (ht-each
   (lambda (_ node) (treemacs-shadow-node->invalidate-pos node))
   treemacs-shadow-index))

(defun treemacs--recursive-refresh-descent (node)
  "The recursive descent implementation of `treemacs--recursive-refresh'.
If NODE is marked for refresh and in an open state (since it could have been
collapsed in the meantime) it will simply be collapsed and re-expanded. If NODE
is node marked its children will be recursively investigated instead.
Additionally all the refreshed nodes are collected and returned so their
parents' git status can be updated."
  (-let [refreshed-nodes nil]
    (if (treemacs-shadow-node->refresh-flag node)
        (progn
          (push node refreshed-nodes)
          (treemacs--refresh-dir (treemacs-shadow-node->key node))
          (treemacs--do-for-all-child-nodes node
            #'treemacs-shadow-node->reset-refresh-flag))
      (dolist (child (treemacs-shadow-node->children node))
        (setq refreshed-nodes
              (nconc refreshed-nodes
                     (treemacs--recursive-refresh-descent child)))))
    refreshed-nodes))

(defun treemacs--recursive-refresh ()
  "Recursively descend the shadow tree, updating only the refresh-marked nodes.
If the root is marked simply reset all refresh flags and run `treemacs-refresh'
instead."
  (-let [projects (treemacs-workspace->projects (treemacs-current-workspace))]
    (dolist (project projects)
      (-when-let- [root-node (-> project (treemacs-project->path) (treemacs-get-from-shadow-index))]
        (if (treemacs-shadow-node->refresh-flag root-node)
            (progn
              (treemacs--do-for-all-child-nodes root-node #'treemacs-shadow-node->reset-refresh-flag)
              (treemacs--do-refresh (current-buffer) project))
          (dolist (root-child (treemacs-shadow-node->children root-node))
            (treemacs--recursive-refresh-descent root-child)))))))

(provide 'treemacs-structure)

;;; treemacs-structure.el ends here
