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
;;; API required for writing extensions for/with treemacs.

;;; Code:

(require 's)
(require 'dash)
(require 'treemacs)

(eval-when-compile
  (require 'treemacs-macros)
  (require 'cl-lib))

(eval-when-compile
  (cl-declaim (optimize (speed 3) (safety 0))))

(defconst treemacs-treelib-version "1.0")

(defvar treemacs--extension-registry nil
  "Alist storage of extension instances.
The car is a symbol of an extension node's state, the cdr the instance of the
`treemacs-extension' type.")

(cl-defstruct (treemacs-extension
               (:conc-name treemacs-extension->)
               (:constructor treemacs-extension->create!))

  ;; only for comparisons
  name

  ;; open/close
  closed-state
  open-state
  closed-icon
  open-icon

  ;; produces full list of child items to render
  children

  ;; produces a key for one item returned by children
  key

  ;; produces a face for one item returned by children
  face

  ;; produces a text label for one item returned by children
  label

  ;; plist of additional text properties for on item returned by children
  more-properties

  ;; Struct instance of child nodes returned by children
  child-type

  ;; used for entry-point render method selection
  variadic?

  ;; special treatment for asynchronous behavior
  async?

  ;; used as a check when the extension is enabled
  entry-point?)

(defun treemacs--compare-extensions-by-name (e1 e2)
  "Compare E1 and E2 by their names."
  (declare (side-effect-free t))
  ;; take into account cells used by functions like `tremacs-define-project-extension'
  (let ((e1 (if (consp e1) (car e1) e1))
        (e2 (if (consp e2) (car e2) e2)))
    (equal (treemacs-extension->name e1)
           (treemacs-extension->name e2))))

(defmacro treemacs-extension->get (self field &rest args)
  "Access helper for the lambda fields of `treemacs-extension' instances.
Takes SELF's given FIELD and `funcall's it with ARGS."
  (let* ((field-name (substring (symbol-name field) 1))
         (fn (intern (s-lex-format "treemacs-extension->${field-name}"))))
    `(funcall (,fn ,self) ,@args)))

(cl-macrolet
    ((build-extension-addition
      (name)
      (let ((define-function-name   (intern (s-lex-format "treemacs-enable-${name}-extension")))
            (top-extension-point    (intern (s-lex-format "treemacs--${name}-top-extensions")))
            (bottom-extension-point (intern (s-lex-format "treemacs--${name}-bottom-extensions"))))
        `(progn
           (defvar ,top-extension-point nil)
           (defvar ,bottom-extension-point nil)
           (cl-defun ,define-function-name (&key extension predicate position)
             ,(s-lex-format
               "Enable a `${name}' level EXTENSION for treemacs to use.

EXTENSION is a `treemacs-extension' instance as created by
`treemacs-define-entry-node-type'

PREDICATE is a function that will be called to determine whether the extension
should be displayed. It is invoked with a single argument, which is the
project struct or directory that is being expanded.

POSITION is either `top' or `bottom', indicating whether the extension should be
rendered as the first or last element.

See also `treemacs-disable-${name}-extension'.")
             (treemacs-static-assert (treemacs-extension-p extension)
               "Given argument is not a `treemacs-extension' instance: %s" extension)
             (treemacs-static-assert (treemacs-extension->entry-point? extension)
               "The given extension '%s' is not an entry point" (treemacs-extension->name extension))
             (-let [cell (cons extension predicate)]
               (pcase position
                 ('top    (add-to-list ',top-extension-point cell nil #'treemacs--compare-extensions-by-name))
                 ('bottom (add-to-list ',bottom-extension-point cell nil #'treemacs--compare-extensions-by-name))
                 (other   (error "Invalid extension position value `%s'" other)))
               t)))))
     (build-extension-removal
      (name)
      (let ((remove-function-name   (intern (s-lex-format "treemacs-disable-${name}-extension")))
            (top-extension-point    (intern (s-lex-format "treemacs--${name}-top-extensions")))
            (bottom-extension-point (intern (s-lex-format "treemacs--${name}-bottom-extensions"))))
        `(progn
           (cl-defun ,remove-function-name (&key extension position)
             ,(s-lex-format
               "Remove a `${name}' EXTENSION at the given POSITION.
See also `treemacs-enable-${name}-extension'.")
             (treemacs-static-assert (treemacs-extension-p extension)
               "Given argument is not a `treemacs-extension' instance: %s" extension)
             (pcase position
               ('top
                (setf ,top-extension-point
                      (--remove-first (treemacs--compare-extensions-by-name it extension)
                                      ,top-extension-point)))
               ('bottom
                (setf ,bottom-extension-point
                      (--remove-first (treemacs--compare-extensions-by-name it extension)
                                      ,bottom-extension-point)))
               (other
                (error "Invalid extension position value `%s'" other)))
             t))))
     (build-extension-application
      (name)
      (let ((apply-top-name         (intern (s-lex-format "treemacs--apply-${name}-top-extensions")))
            (apply-bottom-name      (intern (s-lex-format "treemacs--apply-${name}-bottom-extensions")))
            (top-extension-point    (intern (s-lex-format "treemacs--${name}-top-extensions")))
            (bottom-extension-point (intern (s-lex-format "treemacs--${name}-bottom-extensions"))))
        `(progn
           (defun ,apply-top-name (node data)
             ,(s-lex-format
               "Apply the top `${name}' extensions for NODE.
Also pass additional DATA to predicate function.")
             (dolist (cell ,top-extension-point)
               (let ((extension (car cell))
                     (predicate (cdr cell)))
                 (when (or (null predicate) (funcall predicate data))
                   (if (functionp extension)
                       ;; TODO(2020/05/30): old-school extensions
                       ;; to be removed eventually
                       (funcall extension node)
                     (treemacs--extension-entry-render extension node))))))

           (defun ,apply-bottom-name (node data)
             ,(s-lex-format
               "Apply the `${name}' bottom extensions for NODE.
Also pass additional DATA to predicate function.")
             (dolist (cell ,bottom-extension-point)
               (let ((extension (car cell))
                     (predicate (cdr cell)))
                 (when (or (null predicate) (funcall predicate data))
                   (if (functionp extension)
                       ;; TODO(2020/05/30): old-school extensions
                       ;; to be removed eventually
                       (funcall extension node)
                     (treemacs--extension-entry-render extension node)))))))))
     (build-top-level-extension-application
      (pos)
      (let ((name     (intern (s-lex-format "treemacs--apply-root-${pos}-extensions")))
            (variable (intern (s-lex-format "treemacs--top-level-${pos}-extensions"))))
        `(defun ,name (workspace &optional has-previous)
           ,(s-lex-format
             "Apply the ${pos} `top-level' extensions for the current WORKSPACE.
Also pass additional DATA to predicate function.")
           (let ((is-first (not has-previous)))
             (--each ,variable
               (let ((extension (car it))
                     (predicate (cdr it)))
                 (when (or (null predicate) (funcall predicate workspace))
                   (unless is-first
                     (treemacs--insert-root-separator))
                   ;; TODO(2020/05/30): old-school extensions
                   ;; to be removed eventually
                   (setf is-first (if (functionp extension)
                                      (funcall extension)
                                    (treemacs-render-extension extension))))))
             (not is-first))))))
  (build-extension-addition    "project")
  (build-extension-removal     "project")
  (build-extension-application "project")
  (build-extension-addition    "directory")
  (build-extension-removal     "directory")
  (build-extension-application "directory")
  (build-extension-addition    "top-level")
  (build-extension-removal     "top-level")
  (build-top-level-extension-application "top")
  (build-top-level-extension-application "bottom"))

(cl-defmacro treemacs-do-define-extension-type
    (name &key
          children
          more-properties
          key
          label
          face
          open-icon
          closed-icon
          child-type
          ;; visit-action
          ret-action
          no-tab?
          variadic?
          async?
          entry-point?)

  "Base building block for extension node setup.
Not meant for direct use.  Instead one of the following macros should be
employed:

 - `treemacs-define-leaf-node-type'
 - `treemacs-define-expandable-node-type'
 - `treemacs-define-entry-node-type'
 - `treemacs-define-variadic-entry-node-type'

NAME of this node type is a symbol.  An instance of the `treemacs-extension'
type with the name `treemacs-${NAME}-extension-instance' will be created for
this node type.

CHILDREN is a form to query a list of items to be rendered as children when a
node is expanded.  The node being expanded is available as a variable under the
name `btn'.  It is a `button' in the sense of the built-in button.el library
\(really just a marker to a buffer position), so its text-properties can be
extracted via `(treemacs-button-get node :property)' (see also MORE-PROPERTIES).
In addition the item (as produced by the form passed here) that was used the
create the node will also be available under the name `item'.  For entry-level
nodes at the top of the hierarchy `item' will be nil.

KEY, LABEL, FACE, OPEN-ICON, CLOSE-ICON and MORE-PROPERTIES all act on one of
the items produced by CHILDREN.  The respective item will be bound under the
name `item' in their scope (unless the node being created is at the top of the
hierarchy and has no parent with its own CHILDREN - in this case `item' will
just be nil).

KEY is a form to generate a semi-unique key for a given node for one of the
items produced by CHILDREN.  Semi-unique means that nodes' keys don't all have
to be unique, it is only necessary that a node's path - the list of all node
keys starting from the top level root leading to a specific node - must be
un-ambiguous.

LABEL is a form to query a node's text label (the text after the icon) for one
of the items produced by CHILDREN.  The return value should be a string.

FACE is a form the query to face of a node's LABEL for one of the items produced
by children.  The return value should be a face symbol.

OPEN-ICON and CLOSED-ICON are forms to determine the icons used for the node's
open and closed states.  The return value should be a string.

MORE-PROPERTIES is a form to produce a plist that will be saved as additional
text-properties in a given node.  These properties can later be accessed when
querying the node's CHILDREN (see above).

CHILD-TYPE is, unlike all the other arguments, not a form, but a quoted symbol.
It must refer to the NAME argument of a another (or the same) extension type and
determines the behaviours (LABEL, FACE etc.) used to create the children of the
node type being defined here.

RET-ACTION is the function that is called when RET is pressed on a node of this
be able to handle both a closed and open state.  If no explicit RET-ACTION type.
The function is called with a single argument - the prefix arg - and must
argument is given RET will do the same as TAB.

NO-TAB indicates that pressing TAB on this node type should do nothing.  It will
be set by `treemacs-define-leaf-node'.

VARIADIC is only relevant for entry-point nodes and indicates that the extension
will produces multiple nodes when first rendered.

ASYNC will enable an asynchronous, callback-based fetching of CHILDREN.  When it
is non-nil the function passed to children will be called with the 3rd argument
`callback'.  It should be invoked via `funcall' with the items that were
produced asynchronously.

ENTRY-POINT indicates that the node type defined here is an entry-point for an
extension, it will be used as a type-check when enabling an extension with e.g.
`treemacs-enable-top-level-extension'."

  (declare (indent 1))

  (let* ((child-type   (cadr child-type))
         (child-name   (intern (s-lex-format "treemacs-${child-type}-extension-instance")))
         (struct-name  (intern (s-lex-format "treemacs-${name}-extension-instance")))
         (open-state   (intern (s-lex-format "treemacs-${name}-open-state")))
         (closed-state (intern (s-lex-format "treemacs-${name}-closed-state")))
         (children-fn  (if async?
                           `(lambda (btn item callback) (ignore btn item callback) ,children)
                         `(lambda (btn item) (ignore btn item) ,children))))
    `(progn
       (defconst ,struct-name
         (treemacs-extension->create!
          :name            ',name
          :variadic?       ,variadic?
          :async?          ,async?
          :children        ,children-fn
          :entry-point?    ,entry-point?
          :label           (lambda (&optional item) "" (ignore item) ,label)
          :key             (lambda (&optional item) "" (ignore item) ,key)
          :open-icon       (lambda (&optional item) "" (ignore item) ,open-icon)
          :closed-icon     (lambda (&optional item) "" (ignore item) ,closed-icon)
          :face            (lambda (&optional item) "" (ignore item) ,face)
          :more-properties (lambda (item)           "" (ignore item) ,more-properties)
          :child-type      (lambda ()               "" (symbol-value ',child-name))
          :open-state      (lambda ()               "" ',open-state)
          :closed-state    (lambda ()               "" ',closed-state)))

       (treemacs-define-TAB-action ',closed-state ,(if no-tab? '#'ignore '#'treemacs-expand-extension-node))
       (treemacs-define-TAB-action ',open-state   ,(if no-tab? '#'ignore '#'treemacs-collapse-extension-node))
       (treemacs-define-RET-action ',closed-state ,(or ret-action (if no-tab? '#'ignore '#'treemacs-expand-extension-node)))
       (treemacs-define-RET-action ',open-state   ,(or ret-action (if no-tab? '#'ignore '#'treemacs-collapse-extension-node)))

       (add-to-list 'treemacs--extension-registry (cons ',closed-state ,struct-name))
       (add-to-list 'treemacs--extension-registry (cons ',open-state   ,struct-name))

       (add-to-list 'treemacs--closed-node-states ',closed-state)
       (add-to-list 'treemacs--open-node-states   ',open-state)
       (add-to-list 'treemacs-valid-button-states ',closed-state)
       (add-to-list 'treemacs-valid-button-states ',open-state)

       ',struct-name)))

(cl-defmacro treemacs-define-leaf-node-type
    (name &key
          icon
          label
          key
          face
          more-properties
          ret-action)

  "Define a type of node that is a leaf and cannot be further expanded.
The NAME, ICON, LABEL, KEY and FACE arguments are mandatory.

MORE-PROPERTIES and RET-ACTION are optional.

For a detailed description of all arguments see
`treemacs-do-define-extension-type'."

  (declare (indent 1))

  (treemacs-static-assert icon  ":icon parameter is mandatory")
  (treemacs-static-assert label ":label parameter is mandatory")
  (treemacs-static-assert key   ":key parameter is mandatory")
  (treemacs-static-assert face  ":face parameter is mandatory")

  `(treemacs-do-define-extension-type ,name
     :key ,key
     :label ,label
     :face ,face
     :more-properties ,more-properties
     :closed-icon ,icon
     :ret-action ,ret-action
     :no-tab? t
     :children (lambda () (error "Called :children of leaf node"))
     :child-type (lambda () (error "Called :child-type of leaf node"))))

(cl-defmacro treemacs-define-expandable-node-type
    (name &key
          closed-icon
          open-icon
          label
          key
          face
          children
          child-type
          more-properties
          async?)

  "Define a general-purpose expandable node-type.
The NAME, CLOSED-ICON, OPEN-ICON LABEL, KEY, FACE, CHILDREN and CHILD-TYPE
arguments are mandatory.

MORE-PROPERTIES and ASYNC are optional.

For a detailed description of all arguments see
`treemacs-do-define-extension-type'."

  (declare (indent 1))

  (treemacs-static-assert closed-icon ":closed-icon parameter is mandatory")
  (treemacs-static-assert open-icon   ":open-icon parameter is mandatory")
  (treemacs-static-assert label       ":label parameter is mandatory")
  (treemacs-static-assert face        ":face parameter is mandatory")
  (treemacs-static-assert key         ":key parameter is mandatory")
  (treemacs-static-assert children    ":children parameter is mandatory")
  (treemacs-static-assert child-type  ":child-type parameter is mandatory")

  `(treemacs-do-define-extension-type ,name
     :closed-icon ,closed-icon
     :open-icon   ,open-icon
     :label ,label
     :face ,face
     :key ,key
     :children ,children
     :child-type ,child-type
     :more-properties ,more-properties
     :async? ,async?))

(cl-defmacro treemacs-define-entry-node-type
    (name &key
          key
          label
          open-icon
          closed-icon
          children
          face
          child-type
          more-properties
          ret-action
          async?)

  "Define a node type with NAME that serves as an entry-point for an extension.
The `treemacs-${NAME}-extension-instance' created by this macro can be passed to
the `treemacs-enable-*-extension' family of functions.

The KEY, LABEL, OPEN-ICON CLOSED-ICON, CHILDREN, FACE and CHILD-TYPE arguments
are mandatory.

MORE-PROPERTIES, RET-ACTION and ASYNC are optional.

For a detailed description of all arguments see
`treemacs-do-define-extension-type'."

  (declare (indent 1))

  (treemacs-static-assert key         ":key parameter is mandatory")
  (treemacs-static-assert label       ":label parameter is mandatory")
  (treemacs-static-assert open-icon   ":open-icon parameter is mandatory")
  (treemacs-static-assert closed-icon ":closed-icon parameter is mandatory")
  (treemacs-static-assert children    ":childen parameter is mandatory")
  (treemacs-static-assert face        ":face parameter is mandatory")
  (treemacs-static-assert child-type  ":child-type parameter is mandatory")

  `(treemacs-do-define-extension-type ,name
     :key ,key
     :label ,label
     :open-icon ,open-icon
     :closed-icon ,closed-icon
     :children ,children
     :face ,face
     :child-type ,child-type
     :more-properties ,more-properties
     :async? ,async?
     :ret-action ,ret-action
     :entry-point? t))

(cl-defmacro treemacs-define-variadic-entry-node-type
    (name &key
          key
          label
          open-icon
          closed-icon
          children
          face
          child-type
          more-properties
          async?)

  "Define a node type that serves as an entry-point for a variadic extension.
'Variadic' means that the extension will produce multiple nodes when it is first
rendered instead of just one (e.g. a single 'Buffer List' node vs multiple nodes
each grouping buffers by major mode).

The `treemacs-${NAME}-extension-instance' created by this macro can be passed to
the `treemacs-enable-*-extension' family of functions.

The KEY, LABEL, OPEN-ICON CLOSED-ICON, CHILDREN, FACE and CHILD-TYPE arguments
are mandatory.

MORE-PROPERTIES and ASYNC are optional.

For a detailed description of all arguments see
`treemacs-do-define-extension-type'."

  (declare (indent 1))

  (treemacs-static-assert key         ":key parameter is mandatory")
  (treemacs-static-assert label       ":label parameter is mandatory")
  (treemacs-static-assert open-icon   ":open-icon parameter is mandatory")
  (treemacs-static-assert closed-icon ":closed-icon parameter is mandatory")
  (treemacs-static-assert children    ":childen parameter is mandatory")
  (treemacs-static-assert face        ":face parameter is mandatory")
  (treemacs-static-assert child-type  ":child-type parameter is mandatory")

  `(treemacs-do-define-extension-type ,name
     :key ,key
     :label ,label
     :open-icon ,open-icon
     :closed-icon ,closed-icon
     :children ,children
     :face ,face
     :child-type ,child-type
     :more-properties ,more-properties
     :async? ,async?
     :variadic? t
     :entry-point? t))

(defun treemacs-render-extension (ext)
  "Render the entry point of the given extension EXT.
Also serves as an entry point to render an extension in an independent buffer
outside of treemacs proper.

EXT: `treemacs-extension' instance"
  (if (treemacs-extension->variadic? ext)
      (treemacs--variadic-extension-entry-render ext)
    (treemacs--singular-extension-entry-render ext)))

(defun treemacs--singular-extension-entry-render (ext)
  "Render the entry point of the given singular top level extension EXT.
Will create and insert the required strings and make a new dom entry.

EXT: `treemacs-extension' instance"
  (treemacs-with-writable-buffer
   (let* ((label (treemacs-extension->get ext :label))
          (key (treemacs-extension->get ext :key))
          (project (treemacs-project->create!
                    :name label
                    :path key
                    :path-status 'extension))
          (dom-node (treemacs-dom-node->create! :key key)))
     (treemacs-dom-node->insert-into-dom! dom-node)
     (insert (treemacs-extension->get ext :closed-icon))
     (treemacs--set-project-position key (point-marker))
     (setf (treemacs-dom-node->position dom-node) (point-marker))
     (insert (propertize
              label
              'button '(t)
              'category 'default-button
              'face (treemacs-extension->get ext :face)
              :custom t
              :key key
              :path key
              :depth 0
              :project project
              :state (treemacs-extension->get ext :closed-state)))))
  nil)

(defun treemacs--variadic-extension-entry-render (ext)
  "Render the entry point of the given variadic top level extension EXT.
Will create and insert the required strings and make a new dom entry.

EXT: `treemacs-extension' instance"
  (save-excursion
    (treemacs-with-writable-buffer
     ;; When the extension is variadic it will be managed by a hidden top-level
     ;; node. Its depth is -1 and it is not visible, but can still be used to update
     ;; the entire extension without explicitly worrying about complex dom changes.
     (let* ((key (treemacs-extension->get ext :key))
            (pr (treemacs-project->create!
                 :name (treemacs-extension->get ext :label)
                 :path key
                 :path-status 'extension))
            (button-start (point-marker))
            (dom-node (treemacs-dom-node->create!
                       :key key
                       :position button-start)))
       ;; TODO(2020/07/08): remove project cache
       (treemacs--set-project-position key (point-marker))
       (treemacs-dom-node->insert-into-dom! dom-node)
       (insert (propertize "Hidden Node\n"
                           'button '(t)
                           'category 'default-button
                           'invisible t
                           'skip t
                           :custom t
                           :key key
                           :path key
                           :depth -1
                           :project pr
                           :state (treemacs-extension->get ext :closed-state)))
       (let ((marker (copy-marker (point) t)))
         (treemacs--expand-variadic-parent button-start ext)
         (goto-char marker)))))
  t)

(cl-defmacro treemacs--create-node-strings
    (&key parent
          parent-path
          parent-dom-node
          more-properties
          icon
          state
          key
          depth
          face
          label)

  "Create the strings needed to render an extension node.

PARENT: Button
PARENT-PATH: List<?>
PARENT-DOM-NODE: Dom Node Struct
MORE-PROPERTIES: Plist
ICON: String
STATE: Symbol
KEY: Any
DEPTH: Int
FACE: Face
LABEL: String"

  (macroexp-let2* nil
      ((key key))
    `(let* ((path (append ,parent-path (list ,key)))
            (dom-node (treemacs-dom-node->create! :key path :parent parent-dom-node))
            (props ,more-properties))
       (treemacs-dom-node->insert-into-dom! dom-node)
       (when ,parent-dom-node
         (treemacs-dom-node->add-child! parent-dom-node dom-node))
       (list (unless (zerop depth) prefix)
             ,icon
             (apply
              #'propertize ,label
              'button '(t)
              'category 'default-button
              ,@(when face `((quote face) ,face))
              'help-echo nil
              :custom t
              :state ,state
              :parent ,parent
              :depth ,depth
              :path path
              :key ,key
              :no-git t
              props)
             (when (zerop depth) (if treemacs-space-between-root-nodes "\n\n" "\n"))))))

;; render method for extensions by the old version of the api
(defun treemacs--extension-entry-render (ext parent)
  "Render the entry point of the given extension EXT under PARENT.
Will create and insert the required strings and make a new dom entry.

EXT: `treemacs-extension' instance
PARENT: Button"
  (let* ((key (treemacs-extension->get ext :key))
         (depth (1+ (treemacs-button-get parent :depth)))
         (path (list (treemacs-button-get parent :path) key))
         (parent-dom-node (treemacs-find-in-dom (treemacs-button-get parent :path)))
         (new-dom-node (treemacs-dom-node->create! :key path :parent parent-dom-node)))
    (treemacs-dom-node->insert-into-dom! new-dom-node)
    (when parent-dom-node
      (treemacs-dom-node->add-child! parent-dom-node new-dom-node))
    (insert "\n")
    (setf (treemacs-dom-node->position new-dom-node) (point-marker))
    (insert
     (treemacs--get-indentation depth)
     (treemacs-extension->get ext :closed-icon)
     (propertize (treemacs-extension->get ext :label)
                 'button '(t)
                 'category 'default-button
                 'face (treemacs-extension->get ext :face)
                 :custom t
                 :key key
                 :path path
                 :depth depth
                 :no-git t
                 :parent parent
                 :state (treemacs-extension->get ext :closed-state))))
  nil)

(defun treemacs-expand-extension-node (&optional arg)
  "Expand a node created with the extension api.
If a prefix ARG is provided expand recursively."
  (interactive "P")
  (let* ((btn (treemacs-node-at-point))
         (state (treemacs-button-get btn :state))
         (ext (alist-get state treemacs--extension-registry)))
    (when (null ext)
      (error "No extension is registered for state '%s'" state))
    (if (treemacs-extension->async? ext)
        (treemacs--do-expand-async-extension-node btn ext arg)
      (treemacs--do-expand-extension-node btn ext arg))))

(defun treemacs-collapse-extension-node (&optional arg)
  "Collapse a node created with the extension api.
If a prefix ARG is provided expand recursively."
  (interactive "P")
  (let* ((btn (treemacs-node-at-point))
         (state (treemacs-button-get btn :state))
         (ext (alist-get state treemacs--extension-registry)))
    (when (null ext)
      (error "No extension is registered for state '%s'" state))
    (treemacs--do-collapse-extension-node btn ext arg)))

(defun treemacs--do-expand-async-extension-node (btn ext &optional _arg)
  "Expand an async extension node BTN for the given extension EXT.

BTN: Button
EXT: `treemacs-extension' instance
ARG: Prefix Arg"
  (interactive)
  (treemacs-block
   (treemacs-error-return-if (not (eq (treemacs-extension->get ext :closed-state)
                                      (treemacs-button-get btn :state)))
     "This function cannot expand a node of type '%s'."
     (propertize (format "%s" (treemacs-button-get btn :state)) 'face 'font-lock-type-face))
   (let* ((depth           (1+ (treemacs-button-get btn :depth)))
          (btn-path        (treemacs-button-get btn :path))
          (parent-path     (if (listp btn-path) btn-path (list btn-path)))
          (parent-dom-node (treemacs-find-in-dom btn-path))
          (child-ext       (treemacs-extension->get ext :child-type))
          (child-state     (treemacs-extension->get child-ext :closed-state)))
     (prog1 1
       (save-excursion
         (let ((prefix (concat "\n" (treemacs--get-indentation depth))))
           (treemacs-with-writable-buffer
            (treemacs-button-put btn :state (treemacs-extension->get ext :open-state))
            (beginning-of-line)
            (treemacs--button-symbol-switch (treemacs-extension->get ext :open-icon btn))
            (goto-char (treemacs-button-end btn))
            (insert
             (apply
              #'concat
              (treemacs--create-node-strings
               :parent btn
               :parent-path parent-path
               :parent-dom-node parent-dom-node
               :more-properties nil
               :icon ""
               :state child-state
               :key 'ASYNC-CALL
               :depth depth
               :face 'font-lock-comment-face
               :label "Loading ...")))
            (treemacs-on-expand (treemacs-button-get btn :path) btn)
            (treemacs--reentry (treemacs-button-get btn :path))))))
     (funcall
      (treemacs-extension->children ext)
      btn
      (treemacs-button-get btn :item)
      (lambda (items) (treemacs--complete-async-expand-callback items btn ext))))))

(defun treemacs--complete-async-expand-callback (items btn ext)
  "Properly expand an async node after its children were computed.

ITEMS are the node's children.
BTN is the button leading to the node.
EXT is the node's extension instance."
  (goto-char btn)
  (treemacs--do-collapse-extension-node btn ext)
  (treemacs--do-expand-extension-node btn ext nil items))

(defun treemacs--do-expand-extension-node (btn ext &optional _arg items)
  "Expand an extension node BTN for the given extension EXT.
ITEMS will override the node's normal `children' function.  This is only used
when the node is asynchronous and this call is used to complete the async
computation.

BTN: Button
EXT: `treemacs-extension' instance
ARG: Prefix Arg
ITEMS: List<Any>"
  (treemacs-block
   (treemacs-error-return-if (not (eq (treemacs-extension->get ext :closed-state)
                                      (treemacs-button-get btn :state)))
     "This function cannot expand a node of type '%s'."
     (propertize (format "%s" (treemacs-button-get btn :state)) 'face 'font-lock-type-face))
   (let* ((items           (or items (treemacs-extension->get ext :children btn (treemacs-button-get btn :item))))
          (depth           (1+ (treemacs-button-get btn :depth)))
          (btn-path        (treemacs-button-get btn :path))
          (parent-path     (if (listp btn-path) btn-path (list btn-path)))
          (parent-dom-node (treemacs-find-in-dom btn-path))
          (child-ext       (treemacs-extension->get ext :child-type))
          (child-state     (treemacs-extension->get child-ext :closed-state))
          (closed-icon-fn  (treemacs-extension->closed-icon child-ext))
          (label-fn        (treemacs-extension->label child-ext))
          (properties-fn   (treemacs-extension->more-properties child-ext))
          (face-fn         (treemacs-extension->face child-ext))
          (key-fn          (treemacs-extension->key child-ext)))
     (treemacs--button-open
      :button btn
      :new-state (treemacs-extension->get ext :open-state)
      :new-icon (treemacs-extension->get ext :open-icon btn)
      :immediate-insert t
      :open-action
      (treemacs--create-buttons
       :nodes items
       :depth depth
       :node-name item
       :node-action
       (treemacs--create-node-strings
        :parent btn
        :parent-path parent-path
        :parent-dom-node parent-dom-node
        :more-properties (nconc `(:item ,item) (funcall properties-fn item))
        :icon (funcall closed-icon-fn item)
        :state child-state
        :key (funcall key-fn item)
        :depth depth
        :face (funcall face-fn item)
        :label (funcall label-fn item)))
      :post-open-action
      (progn
        (treemacs-on-expand (treemacs-button-get btn :path) btn)
        (treemacs--reentry (treemacs-button-get btn :path)))))))

(defun treemacs--expand-variadic-parent (btn ext)
  "Expand the hidden parent BTN of a variadic extension instance EXT.

BTN: Button
EXT: `treemacs-extension' instance"
  (let* ((items           (treemacs-extension->get ext :children btn))
         (btn-path        (treemacs-button-get btn :path))
         (parent-path     (list btn-path))
         (parent-dom-node (treemacs-find-in-dom btn-path))
         (child-ext       (treemacs-extension->get ext :child-type))
         (child-state     (treemacs-extension->get child-ext :closed-state))
         (closed-icon-fn  (treemacs-extension->closed-icon child-ext))
         (label-fn        (treemacs-extension->label child-ext))
         (properties-fn   (treemacs-extension->more-properties child-ext))
         (face-fn         (treemacs-extension->face child-ext))
         (key-fn          (treemacs-extension->key child-ext)))
    (treemacs--button-open
     :button btn
     :new-state (treemacs-extension->get ext :open-state)
     :immediate-insert t
     :open-action
     (treemacs--create-buttons
      :nodes items
      :depth 0
      :node-name item
      :node-action
      (treemacs--create-node-strings
       :parent btn
       :parent-path parent-path
       :parent-dom-node parent-dom-node
       :more-properties (funcall properties-fn item)
       :icon (funcall closed-icon-fn item)
       :state child-state
       :key (funcall key-fn item)
       :depth depth
       :face (funcall face-fn item)
       :label (funcall label-fn item)))
     :post-open-action
     (progn
       (treemacs-on-expand (treemacs-button-get btn :path) btn)
       (treemacs--reentry (treemacs-button-get btn :path))))))

(defun treemacs--do-collapse-extension-node (btn ext &optional __arg)
  "Collapse an extension button BTN for the given EXT.

BTN: Button
EXT: `treemacs-extensions' instance
ARG: Prefix Arg"
  (treemacs--button-close
   :button btn
   :new-state (treemacs-extension->get ext :closed-state)
   :new-icon (treemacs-extension->get ext :closed-icon)
   :post-close-action
   (treemacs-on-collapse (treemacs-button-get btn :path))))

(defun treemacs-initialize ()
  "Initialise treemacs in an external buffer for extension use."
  (treemacs--disable-fringe-indicator)
  (treemacs-with-writable-buffer
   (erase-buffer))
  ;; make sure the fringe indicator is enabled later, otherwise treemacs attempts
  ;; to move it right after the `treemacs-mode' call
  ;; the indicator cannot be created before either since the major-mode activation
  ;; wipes out buffer-local variables' values
  (let ((treemacs-fringe-indicator-mode nil)
        (treemacs--in-this-buffer t))
    (treemacs-mode))
  (setq-local treemacs--in-this-buffer :extension))

;;;; REDEFINITIONS -----------------------------------------------------------------------------------

(cl-defmacro treemacs-with-path-2 (path &key file-action extension-action no-match-action)
  "Execute an action depending on the type of PATH.

FILE-ACTION is the action to perform when PATH is a regular file node.
EXTENSION-ACTION is performed on extension-created nodes.

If none of the path types matches, NO-MATCH-ACTION is executed."
  (declare (indent 1))
  (let ((path-symbol (make-symbol "path")))
    `(let ((,path-symbol ,path))
       (cond
        ,@(when file-action
            `(((stringp ,path-symbol) ,file-action)))
        ,@(when extension-action
            `(((or (symbolp ,path)
                   (symbolp (car ,path))
                   (stringp (car ,path)))
               ,extension-action)))
        (t
         ,(if no-match-action
              no-match-action
            `(error "Path type did not match: %S" ,path-symbol)))))))

(defun treemacs--find-custom-node (path)
  "Specialisation to find a custom node at the given PATH."
  (let* (;; go back here if the search fails
         (start (point))
         ;; (top-pos (treemacs-dom-node->position (treemacs-find-in-dom (car path))))
         ;; making a copy since the variable is a reference to a node actual path
         ;; and will be changed in-place here
         (goto-path (if (listp path) (copy-sequence path) (list path)))
         ;; manual as in to be expanded manually after we moved to the next closest node we can find
         ;; in the dom
         (manual-parts nil)
         (dom-node nil))
    (-let [continue t]
      (while continue
        (setf dom-node (treemacs-find-in-dom goto-path))
        (if (or (null dom-node)
                ;; dom node might exist, but a leaf's position is not always known
                (null (treemacs-dom-node->position dom-node)))
            (if (cdr goto-path)
                (progn
                  (push (-last-item  goto-path) manual-parts)
                  (setf goto-path (-butlast goto-path)))
              (setf goto-path (car goto-path)))
          (setf continue nil))))
    (let* ((btn (treemacs-dom-node->position dom-node))
           ;; do the rest manually
           (search-result (if manual-parts (treemacs--follow-path-elements btn manual-parts)
                            (goto-char btn))))
      (if (eq 'follow-failed search-result)
          (prog1 nil
            (goto-char start))
        (treemacs-dom-node->set-position! (treemacs-find-in-dom path) search-result)
        search-result))))

(defun treemacs-find-node (path &optional project)
  "Find position of node identified by PATH under PROJECT in the current buffer.

In spite of the signature this function effectively supports two different calling
conventions.

The first one is for movement towards a node that identifies a normal file.  In
this case the signature is applied as is, and this function diverges simply into
`treemacs-goto-file-node'.  PATH is a file path string while PROJECT is a
`treemacs-project' struct instance and fully optional, as treemacs is able to
determine which project, if any, a given file belongs to.  Providing the project
when it happens to be available is therefore only a small optimisation.  If
PROJECT is not given it will be found with `treemacs--find-project-for-path'.
No attempt is made to verify that PATH actually falls under a project in the
workspace.  It is assumed that this check has already been made.

The second calling convention deals with custom nodes defined by an extension
for treemacs.  In this case the PATH is made up of all the node keys that lead to
the node to be moved to and PROJECT is not used.

Either way this function will return a marker to the moved-to position if it was
successful.

PATH: Filepath | Node Path
PROJECT Project Struct"

  (treemacs-with-path-2 path
    :file-action (when (file-exists-p path) (treemacs-find-file-node path project))
    :extension-action (treemacs--find-custom-node path)))

(define-inline treemacs-goto-extension-node (path)
  "Move to an extension node at the given PATH.
Small short-cut over `treemacs-goto-node' if you know for certain that PATH
leads to an extension node."
  (inline-letevals (path)
    (inline-quote
     (-when-let (result (treemacs--find-custom-node ,path))
       (treemacs--evade-image)
       (hl-line-highlight)
       ;; Only change window point if the current buffer is actually visible
       (-when-let (window (get-buffer-window))
         (set-window-point window (point)))
       result))))

(defun treemacs-goto-node (path &optional project ignore-file-exists)
  "Move point to button identified by PATH under PROJECT in the current buffer.
Falls under the same constraints as `treemacs-find-node', but will actually move
point.  Will do nothing if file at PATH does not exist, unless IGNORE-FILE-EXISTS
is non-nil.

PATH: Filepath | Node Path
PROJECT Project Struct
IGNORE-FILE-EXISTS Boolean"
  (treemacs-with-path-2 path
    :file-action (when (or ignore-file-exists (file-exists-p path)) (treemacs-goto-file-node path project))
    :extension-action (treemacs-goto-extension-node path)))

(provide 'treemacs-treelib)

;;; treemacs-treelib.el ends here
