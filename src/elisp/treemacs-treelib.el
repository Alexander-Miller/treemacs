;;; treemacs.el --- A tree style file viewer package -*- lexical-binding: t -*-

;; Copyright (C) 2024 Alexander Miller

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

(defconst treemacs-treelib-version "1.1")

(defconst treemacs--treelib-async-load-string
  (propertize " Loading …"
              'face 'treemacs-async-loading-face
              'treemacs-async-string t))

(defvar treemacs--extension-registry nil
  "Alist storage of extension instances.
The car is a symbol of an extension node's state, the cdr the instance of the
`treemacs-extension' type.")

(defvar treemacs--async-update-count (make-hash-table :size 5 :test 'equal)
  "Holds the count of nodes an async update needs to process.
The count is used as finish condition in `treemacs--async-update-part-complete'.")

(defvar treemacs--async-update-cache (make-hash-table :size 20 :test 'equal)
  "Holds to pre-computed cache for async updates.
Set by `treemacs--async-update-part-complete'.")

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
  entry-point?

  ;; callback to run when a node is expanded
  on-expand

  ;; callback to run when a node is collapsed
  on-collapse)

(define-inline treemacs--ext-symbol-to-instance (symbol)
  "Derive an extension instance from the given SYMBOL."
  (declare (side-effect-free t))
  (inline-letevals (symbol)
    (inline-quote
     (symbol-value (intern (format "treemacs-%s-extension-instance" ,symbol))))))

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
             (let* ((ext-instance (treemacs--ext-symbol-to-instance extension))
                    (cell (cons ext-instance predicate)))
               (treemacs-static-assert (treemacs-extension-p ext-instance)
                 "Given argument is not a valid `treemacs-extension': %s" extension)
               (treemacs-static-assert (treemacs-extension->entry-point? ext-instance)
                 "The given extension '%s' is not an entry point" extension)
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
             (-let [ext-instance (treemacs--ext-symbol-to-instance extension)]
               (treemacs-static-assert (treemacs-extension-p ext-instance)
                 "Given argument is not a valid `treemacs-extension': %s" extension)
               (pcase position
                 ('top
                  (setf ,top-extension-point
                        (--remove-first (treemacs--compare-extensions-by-name it ext-instance)
                                        ,top-extension-point)))
                 ('bottom
                  (setf ,bottom-extension-point
                        (--remove-first (treemacs--compare-extensions-by-name it ext-instance)
                                        ,bottom-extension-point)))
                 (other
                  (error "Invalid extension position value `%s'" other))))
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
                                    (treemacs--render-extension extension))))))
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
          open-icon
          closed-icon
          child-type
          ret-action
          visit-action
          double-click-action
          no-tab?
          variadic?
          async?
          entry-point?
          on-expand
          on-collapse)

  "Base building block for extension node setup.
Not meant for direct use.  Instead one of the following macros should be
employed:

 - `treemacs-define-leaf-node-type'
 - `treemacs-define-expandable-node-type'
 - `treemacs-define-entry-node-type'
 - `treemacs-define-variadic-entry-node-type'

NAME of this node type is a symbol.  After an extension is defined the NAME
symbol can be passed to a function like `treemacs-initialize' or
`treemacs-enable-top-level-extension' to make start using it.

CHILDREN is a form to query a list of items to be rendered as children when a
node is expanded.  The node being expanded is available as a variable under the
name `btn'.  It is a `button' in the sense of the built-in button.el library
\(really just a marker to a buffer position), so its text-properties can be
extracted via `(treemacs-button-get btn :property)' (see also MORE-PROPERTIES).
In addition the item (as produced by the form passed here) that was used to
create the node will also be available under the name `item'.

KEY, LABEL, OPEN-ICON, CLOSE-ICON and MORE-PROPERTIES all act on one of the
items produced by CHILDREN.  The node and the item that produced it will be
bound under the names `btn' and `item' respectively.

KEY is a form to generate a semi-unique key for a given node for one of the
items produced by CHILDREN.  Semi-unique means that nodes' keys don't all have
to be unique on their own, it is only necessary that a node's path - the list of
all node keys starting from the top level root leading to a specific node - must
be un-ambiguous.

LABEL is a form to query a node's text label (the text after the icon) for one
of the items produced by CHILDREN.  The return value should be a string.

OPEN-ICON and CLOSED-ICON are forms to determine the icons used for the node's
open and closed states.  The return value should be a string.

MORE-PROPERTIES is a form to produce a plist that will be saved as additional
text-properties in a given node.  These properties can later be accessed when
querying the node's CHILDREN (see above).

CHILD-TYPE is, unlike all the other arguments, not a form, but a quoted symbol.
It must refer to the NAME argument of a another (or the same) extension type and
determines the behaviours (LABEL etc.) used to create the children of the node
type being defined here.

RET-ACTION is the function that is called when RET is pressed on a node of this
be able to handle both a closed and open state.  If no explicit RET-ACTION type
argument is given RET will do the same as TAB.  The function is called with a
single argument - the prefix arg - and must be able to handle both a closed and
and expanded node state.

VISIT-ACTION is a function that is called when a node is to be opened with a
command like `treemacs-visit-node-ace'.  It is called with the current `btn' and
must be able to handle both an open and a closed state.  It will most likely be
called in a window that is not the one where the button resides, so if you need
to extract text properties from the button you to must use
`treemacs-safe-button-get', e.g.  \(treemacs-safe-button-get btn :path\).

DOUBLE-CLICK-ACTION is similar to RET-ACTION, but will be called without any
arguments.  There is no default click behaviour, if no DOUBLE-CLICK-ACTION is
given then treemacs will do nothing for double-clicks.

NO-TAB indicates that pressing TAB on this node type should do nothing.  It will
be set by `treemacs-define-leaf-node'.

VARIADIC is only relevant for entry-point nodes and indicates that the extension
will produces multiple nodes when first rendered.

ASYNC will enable an asynchronous, callback-based fetching of CHILDREN.  When it
is non-nil the function passed to children will be called with the 3rd argument
`callback'.  It should be invoked via `funcall' with the items that were
produced asynchronously.

If the asynchronous execution fails the `callback' should be called with a list
in the form \(`:async-error' error-message\).  Treemacs will take care of
cleanup and logging the error.

ENTRY-POINT indicates that the node type defined here is an entry-point for an
extension, it will be used as a type-check when enabling an extension with e.g.
`treemacs-enable-top-level-extension'.

ON-EXPAND and ON-COLLAPSE are forms to be invoked at the very end of the
expand/collapse process.  They are invoked with the current `btn' as their sole
argument."

  (declare (indent 1))

  (let* ((child-type   (cadr child-type))
         (child-name   (intern (s-lex-format "treemacs-${child-type}-extension-instance")))
         (struct-name  (intern (s-lex-format "treemacs-${name}-extension-instance")))
         (open-state   (intern (s-lex-format "treemacs-${name}-open")))
         (closed-state (intern (s-lex-format "treemacs-${name}-closed")))
         (children-fn  (if async?
                           `(lambda (&optional btn item callback) (ignore btn item callback) ,children)
                         `(lambda (&optional btn item) (ignore btn item) ,children))))
    `(progn
       (defconst ,struct-name
         (treemacs-extension->create!
          :name            ',name
          :variadic?       ,variadic?
          :async?          ,async?
          :children        ,children-fn
          :entry-point?    ,entry-point?
          :label           (lambda (&optional btn item) "" (ignore item) (ignore btn) ,label)
          :key             (lambda (&optional btn item) "" (ignore item) (ignore btn) ,key)
          :open-icon       (lambda (&optional btn item) "" (ignore item) (ignore btn) ,open-icon)
          :closed-icon     (lambda (&optional btn item) "" (ignore item) (ignore btn) ,closed-icon)
          :more-properties (lambda (&optional btn item) "" (ignore item) (ignore btn) ,more-properties)
          :child-type      (lambda ()                   "" (symbol-value ',child-name))
          :open-state      (lambda ()                   "" ',open-state)
          :closed-state    (lambda ()                   "" ',closed-state)
          :on-expand       (lambda (&optional btn )     "" (ignore btn) ,on-expand)
          :on-collapse     (lambda (&optional btn )     "" (ignore btn) ,on-collapse)))

       (with-eval-after-load 'treemacs-mouse-interface
         (treemacs-define-doubleclick-action ',closed-state ,(or double-click-action '#'ignore))
         (treemacs-define-doubleclick-action ',open-state ,(or double-click-action '#'ignore)))

       (treemacs-define-TAB-action
        ',closed-state
        ,(cond
          (no-tab?   '#'ignore)
          (variadic? '#'treemacs--expand-variadic-parent)
          (t         '#'treemacs-expand-extension-node)))
       (treemacs-define-TAB-action ',open-state   ,(if no-tab? '#'ignore '#'treemacs-collapse-extension-node))
       (treemacs-define-RET-action ',closed-state ,(or ret-action (if no-tab? '#'ignore '#'treemacs-expand-extension-node)))
       (treemacs-define-RET-action ',open-state   ,(or ret-action (if no-tab? '#'ignore '#'treemacs-collapse-extension-node)))

       (when ,visit-action
         (put ',open-state   :treemacs-visit-action ,visit-action)
         (put ',closed-state :treemacs-visit-action ,visit-action))

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
          more-properties
          ret-action
          visit-action
          double-click-action)

  "Define a type of node that is a leaf and cannot be further expanded.
The NAME, ICON, LABEL and KEY arguments are mandatory.

MORE-PROPERTIES, RET-ACTION, VISIT-ACTION and DOUBLE-CLICK-ACTION are optional.

For a detailed description of all arguments see
`treemacs-do-define-extension-type'."

  (declare (indent 1))

  (treemacs-static-assert icon  ":icon parameter is mandatory")
  (treemacs-static-assert label ":label parameter is mandatory")
  (treemacs-static-assert key   ":key parameter is mandatory")

  `(treemacs-do-define-extension-type ,name
     :key ,key
     :label ,label
     :more-properties (append '(:leaf t) ,more-properties)
     :closed-icon ,icon
     :ret-action ,ret-action
     :visit-action ,visit-action
     :double-click-action ,double-click-action
     :no-tab? t
     :children (lambda () (error "Called :children of leaf node"))
     :child-type (lambda () (error "Called :child-type of leaf node"))))

(cl-defmacro treemacs-define-expandable-node-type
    (name &key
          closed-icon
          open-icon
          label
          key
          children
          child-type
          more-properties
          ret-action
          double-click-action
          on-expand
          on-collapse
          async?)

  "Define a general-purpose expandable node-type.
The NAME, CLOSED-ICON, OPEN-ICON LABEL, KEY, CHILDREN and CHILD-TYPE arguments
are mandatory.

MORE-PROPERTIES, RET-ACTION, DOUBLE-CLICK-ACTION, ON-EXPAND, ON-COLLAPSE and
ASYNC are optional.

For a detailed description of all arguments see
`treemacs-do-define-extension-type'."

  (declare (indent 1))

  (treemacs-static-assert closed-icon ":closed-icon parameter is mandatory")
  (treemacs-static-assert open-icon   ":open-icon parameter is mandatory")
  (treemacs-static-assert label       ":label parameter is mandatory")
  (treemacs-static-assert key         ":key parameter is mandatory")
  (treemacs-static-assert children    ":children parameter is mandatory")
  (treemacs-static-assert child-type  ":child-type parameter is mandatory")

  `(treemacs-do-define-extension-type ,name
     :closed-icon ,closed-icon
     :open-icon   ,open-icon
     :label ,label
     :key ,key
     :children ,children
     :child-type ,child-type
     :more-properties ,more-properties
     :ret-action ,ret-action
     :double-click-action ,double-click-action
     :async? ,async?
     :on-expand ,on-expand
     :on-collapse ,on-collapse))

(cl-defmacro treemacs-define-entry-node-type
    (name &key
          key
          label
          open-icon
          closed-icon
          children
          child-type
          more-properties
          ret-action
          double-click-action
          on-expand
          on-collapse
          async?)

  "Define a node type with NAME that serves as an entry-point for an extension.

The KEY, LABEL, OPEN-ICON CLOSED-ICON, CHILDREN and CHILD-TYPE arguments are
mandatory.

MORE-PROPERTIES, RET-ACTION, DOUBLE-CLICK-ACTION, ON-EXPAND, ON-COLLAPSE and
ASYNC are optional.

For a detailed description of all arguments see
`treemacs-do-define-extension-type'."

  (declare (indent 1))

  (treemacs-static-assert key         ":key parameter is mandatory")
  (treemacs-static-assert label       ":label parameter is mandatory")
  (treemacs-static-assert open-icon   ":open-icon parameter is mandatory")
  (treemacs-static-assert closed-icon ":closed-icon parameter is mandatory")
  (treemacs-static-assert children    ":childen parameter is mandatory")
  (treemacs-static-assert child-type  ":child-type parameter is mandatory")

  `(treemacs-do-define-extension-type ,name
     :key ,key
     :label ,label
     :open-icon ,open-icon
     :closed-icon ,closed-icon
     :children ,children
     :child-type ,child-type
     :more-properties ,more-properties
     :async? ,async?
     :ret-action ,ret-action
     :double-click-action ,double-click-action
     :on-expand ,on-expand
     :on-collapse ,on-collapse
     :entry-point? t))

(cl-defmacro treemacs-define-variadic-entry-node-type
    (name &key
          key
          children
          child-type)

  "Define a node type that serves as an entry-point for a variadic extension.
'Variadic' means that the extension will produce multiple nodes when it is first
rendered instead of just one (e.g. a single 'Buffer List' node vs multiple nodes
each grouping buffers by major mode).

The NAME symbol can be passed to `treemacs-initialize' to render this extension
in a buffer.

The KEY, CHILDREN and CHILD-TYPE arguments are mandatory.

For a detailed description of all arguments see
`treemacs-do-define-extension-type'."

  (declare (indent 1))

  (treemacs-static-assert key         ":key parameter is mandatory")
  (treemacs-static-assert children    ":childen parameter is mandatory")
  (treemacs-static-assert child-type  ":child-type parameter is mandatory")

  `(treemacs-do-define-extension-type ,name
     :open-icon ""
     :key ,key
     :closed-icon ""
     :children ,children
     :child-type ,child-type
     :variadic? t
     :entry-point? t))

(defun treemacs--render-extension (ext &optional expand-depth)
  "Render the entry point of the given extension EXT.

Also serves as an entry point to render an extension in an independent buffer
outside of treemacs proper.

EXPAND-DEPTH indicates the additional recursion depth.

EXT: `treemacs-extension' instance
EXPAND-DEPTH: Int"
  (when (symbolp ext)
    (setf ext (treemacs--ext-symbol-to-instance ext)))
  (if (treemacs-extension->variadic? ext)
      (treemacs--variadic-extension-entry-render ext expand-depth)
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
          (dom-node (treemacs-dom-node->create!
                     :key key
                     :position (point-marker))))
     (treemacs-dom-node->insert-into-dom! dom-node)
     (insert (treemacs-extension->get ext :closed-icon))
     (setf (treemacs-dom-node->position dom-node) (point-marker))
     (insert (propertize
              label
              'button '(t)
              'category t
              :custom t
              :key key
              :path key
              :depth 0
              :project project
              :state (treemacs-extension->get ext :closed-state)))))
  nil)

(defun treemacs--variadic-extension-entry-render (ext &optional expand-depth)
  "Render the entry point of the given variadic top level extension EXT.
Will create and insert the required strings and make a new dom entry.

EXPAND-DEPTH indicates the additional recursion depth.

EXT: `treemacs-extension' instance
EXPAND-DEPTH: Int"
  (save-excursion
    (treemacs-with-writable-buffer
     ;; When the extension is variadic it will be managed by a hidden top-level
     ;; node. Its depth is -1 and it is not visible, but can still be used to update
     ;; the entire extension without explicitly worrying about complex dom changes.
     (let* ((key (treemacs-extension->get ext :key nil nil))
            (path (list key))
            (pr (treemacs-project->create!
                 :name (treemacs-extension->get ext :label nil nil)
                 :path path
                 :path-status 'extension))
            (button-start (point-marker))
            (dom-node (treemacs-dom-node->create!
                       :key path
                       :position (point-marker))))
       (treemacs-dom-node->insert-into-dom! dom-node)
       (insert (propertize "Hidden node"
                           'button '(t)
                           'category t
                           'invisible t
                           'skip t
                           :custom t
                           :key key
                           :path path
                           :depth -1
                           :project pr
                           :state (treemacs-extension->get ext :closed-state)))
       (let ((marker (copy-marker (point) t)))
         (treemacs--do-expand-variadic-parent button-start ext expand-depth)
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
LABEL: String"

  (macroexp-let2* nil
      ((key key))
    `(let* ((path (append ,parent-path (list ,key)))
            (dom-node (treemacs-dom-node->create! :key path :parent parent-dom-node))
            (props ,more-properties)
            (ann (treemacs-get-annotation path)))
       (treemacs-dom-node->insert-into-dom! dom-node)
       (when ,parent-dom-node
         (treemacs-dom-node->add-child! parent-dom-node dom-node))
       (list (unless (zerop depth) prefix)
             ,icon
             (apply
              #'propertize ,label
              'button '(t)
              'category t
              :custom t
              :state ,state
              :parent ,parent
              :depth ,depth
              :path path
              :key ,key
              :no-git t
              props)
             (and ann (treemacs-annotation->suffix-value ann))
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
    (insert
     (treemacs--get-indentation depth)
     (treemacs-extension->get ext :closed-icon)
     (propertize (treemacs-extension->get ext :label)
                 'button '(t)
                 'category t
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
         (path (treemacs-button-get btn :path))
         (ext (alist-get state treemacs--extension-registry))
         (eol (line-end-position))
         (already-loading
          (/= eol
             (next-single-property-change
              (line-beginning-position) 'treemacs-async-string nil eol))))
    (when (null ext)
      (error "No extension is registered for state '%s'" state))
    (unless (or (treemacs-button-get btn :leaf) already-loading)
      (-let [async-cache (ht-get treemacs--async-update-cache path)]
        (treemacs-with-writable-buffer
         (cond
          (async-cache
           ;; IMPORTANT
           ;; Asynchronous updates must be directed *very* carefully.
           ;; If there is no pre-computed cache the expand happens normally with
           ;; a "Loading..." string and normal async delay.
           ;; If the cache already exists then we at first open the async nodes
           ;; as if they were normal nodes, using the cache as their items.
           ;; Then, in the background, we start an asynchronous update via
           ;; `treemacs-update-async-node'.
           ;; However this first mundane node expansion will trigger re-entry, so
           ;; every child that is re-entered might trigger its own async update,
           ;; which is bad for 2 reasons:
           ;; 1. `treemacs-update-async-node' already updates a full path
           ;; including all its children
           ;; 2. At the end of every update is a call to `treemacs-update-node'
           ;; with a new async cache, causing another round of re-entry and
           ;; potential async updates.
           ;; In short: there is strong potential for an infinite loop!
           ;;
           ;; To keep this from happening the async background update must be
           ;; suppressed for all nodes but the very first, both for re-entry and
           ;; the final update.
           ;; This is achieved by setting the `busy' flag at the first async
           ;; node, and only starting the second update if, and only if, neither
           ;; the current node, nor any of its parents, are marked as busy.
           (let* ((busy? (treemacs-button-get btn :busy))
                  (parent btn))
             (unless busy?
               (while (and (not busy?)
                           (setf parent (treemacs-button-get parent :parent)))
                 (setf busy? (treemacs-button-get parent :busy))))
             (unless busy?
               (treemacs-button-put btn :busy t))
             (treemacs--do-expand-extension-node
              btn ext async-cache arg)
             (unless busy?
               (treemacs-update-async-node path (marker-buffer btn)))))
          ((treemacs-extension->async? ext)
           (treemacs--do-expand-async-extension-node btn ext arg))
          (t
           (treemacs--do-expand-extension-node btn ext nil arg)))
         (treemacs-extension->get ext :on-expand btn))))))

(defun treemacs-collapse-extension-node (&optional arg)
  "Collapse a node created with the extension api.
If a prefix ARG is provided expand recursively."
  (interactive "P")
  (let* ((btn (treemacs-node-at-point))
         (state (treemacs-button-get btn :state))
         (ext (alist-get state treemacs--extension-registry)))
    (when (null ext)
      (error "No extension is registered for state '%s'" state))
    (treemacs--do-collapse-extension-node btn ext arg)
    (treemacs-extension->get ext :on-collapse btn)))

(defun treemacs--do-expand-async-extension-node (btn ext &optional arg)
  "Expand an async extension node BTN for the given extension EXT.
Prefix ARG is used to set expand recursion depth.

BTN: Button
EXT: `treemacs-extension' instance
ARG: Prefix Arg"
  (interactive)
  (treemacs-block
   (treemacs-error-return-if (not (eq (treemacs-extension->get ext :closed-state)
                                      (treemacs-button-get btn :state)))
     "This function cannot expand a node of type '%s'."
     (propertize (format "%s" (treemacs-button-get btn :state))
                 'face 'font-lock-type-face))
   (save-excursion
     (treemacs-with-writable-buffer
      (goto-char (line-end-position))
      (insert treemacs--treelib-async-load-string)))
   (funcall
    (treemacs-extension->children ext)
    btn
    (treemacs-button-get btn :item)
    (lambda (items)
      (ht-set! treemacs--async-update-cache (treemacs-button-get btn :path)
               (or items 'nothing))
      (treemacs--complete-async-expand-callback btn items ext arg)))))

(defun treemacs--complete-async-expand-callback (btn items ext arg)
  "Properly expand an async node at BTN after its child ITEMS were computed.

BTN: Button
ITEMS: List<Any>
EXT: `treemacs-extension' instance
ARG: Prefix Arg"
  (treemacs-with-button-buffer btn
    (save-excursion
      (goto-char btn)
      (treemacs-with-writable-buffer
       (delete-region
        (next-single-char-property-change (point) 'treemacs-async-string)
        (line-end-position)))
      (if (eq :async-error (car items))
          (treemacs-log-err "Something went wrong in an asynchronous context: %s" (cadr items))
        (treemacs--do-expand-extension-node btn ext (or items 'nothing) arg)))
    (hl-line-highlight)))

(defun treemacs--do-expand-extension-node (btn ext &optional items arg)
  "Expand an extension node BTN for the given extension EXT.

ITEMS will override the node's normal `children' function.  This is only used
when the node is asynchronous and this call is used to complete the async
computation.

Prefix ARG is used to set expand recursion depth.

BTN: Button
EXT: `treemacs-extension' instance
ARG: Prefix Arg
ITEMS: List<Any>"
  (treemacs-block
   (treemacs-error-return-if (not (eq (treemacs-extension->get ext :closed-state)
                                      (treemacs-button-get btn :state)))
     "This function cannot expand a node of type '%s'. Current node type is %s"
     (propertize (format "%s" (treemacs-button-get btn :state))
                 'face 'font-lock-type-face)
     (propertize (format "%s" (treemacs-extension->get ext :closed-state))
                 'face 'font-lock-type-face))
   (let* ((items          (pcase items
                            (`nil
                             (treemacs-extension->get
                              ext :children btn (treemacs-button-get btn :item)))
                            (`nothing nil)
                            (_ items)))
          (depth           (1+ (treemacs-button-get btn :depth)))
          (btn-path        (treemacs-button-get btn :path))
          (parent-path     (if (listp btn-path) btn-path (list btn-path)))
          (parent-dom-node (treemacs-find-in-dom btn-path))
          (child-ext       (treemacs-extension->get ext :child-type))
          (child-state     (treemacs-extension->get child-ext :closed-state))
          (closed-icon-fn  (treemacs-extension->closed-icon child-ext))
          (label-fn        (treemacs-extension->label child-ext))
          (properties-fn   (treemacs-extension->more-properties child-ext))
          (key-fn          (treemacs-extension->key child-ext))
          (recursive       (treemacs--prefix-arg-to-recurse-depth arg)))
     (treemacs--button-open
      :button btn
      :new-state (treemacs-extension->get ext :open-state)
      :new-icon (treemacs-extension->get
                 ext :open-icon btn (treemacs-button-get btn :item))
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
        :more-properties (append `(:item ,item) (funcall properties-fn btn item))
        :icon (funcall closed-icon-fn btn item)
        :state child-state
        :key (funcall key-fn btn item)
        :depth depth
        :label (funcall label-fn btn item)))
      :post-open-action
      (progn
        (treemacs-on-expand btn-path btn)
        (treemacs--reentry btn-path)
        (when (> recursive 0)
          (cl-decf recursive)
          (--each (treemacs-collect-child-nodes btn)
            (when (treemacs-is-node-collapsed? it)
              (goto-char (treemacs-button-start it))
              (treemacs-expand-extension-node recursive)))))))))

(defun treemacs--expand-variadic-parent (&optional arg)
  "Interactive command to expand a variadic parent.
Only required to set up `treemacs-TAB-actions-config'.

ARG: Prefix Arg"
  (interactive "P")
  (let* ((btn (treemacs-node-at-point))
         (state (treemacs-button-get btn :state))
         (ext (alist-get state treemacs--extension-registry)))
    (when (null ext)
      (error "No extension is registered for state '%s'" state))
    (treemacs-with-writable-buffer
     (treemacs--do-expand-variadic-parent btn ext arg))))

(defun treemacs--do-expand-variadic-parent (btn ext &optional expand-depth)
  "Expand the hidden parent BTN of a variadic extension instance EXT.

EXPAND-DEPTH indicates the additional recursion depth.

BTN: Button
EXT: `treemacs-extension' instance
EXPAND-DEPTH: Int"
  (let* ((items           (treemacs-extension->get ext :children))
         (parent-path     (treemacs-button-get btn :path))
         (parent-dom-node (treemacs-find-in-dom parent-path))
         (child-ext       (treemacs-extension->get ext :child-type))
         (child-state     (treemacs-extension->get child-ext :closed-state))
         (closed-icon-fn  (treemacs-extension->closed-icon child-ext))
         (label-fn        (treemacs-extension->label child-ext))
         (properties-fn   (treemacs-extension->more-properties child-ext))
         (key-fn          (treemacs-extension->key child-ext))
         (expand-depth    (treemacs--prefix-arg-to-recurse-depth expand-depth)))
    (goto-char (button-end btn))
    (insert (apply #'propertize "\n" (text-properties-at btn)))
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
       :parent nil
       :parent-path parent-path
       :parent-dom-node parent-dom-node
       :more-properties
       (append `(:item ,item)
              `(:project ,(treemacs-project->create!
                           :name (funcall label-fn btn item)
                           :path path
                           :path-status 'extension))
              (funcall properties-fn btn item))
       :icon (funcall closed-icon-fn btn item)
       :state child-state
       :key (funcall key-fn btn item)
       :depth depth
       :label (funcall label-fn btn item)))
     :post-open-action
     (progn
       ;; projects' positions must always be known, so in this one
       ;; case they have to be collected very manually
       (save-excursion
         (goto-char (point-min))
         (-let [btn (point)]
           (while (setf btn (next-button btn))
             (let* ((path (treemacs-button-get btn :path))
                    (dom-node (treemacs-find-in-dom path)))
               (setf (treemacs-dom-node->position dom-node) btn)))))
       (treemacs-on-expand (treemacs-button-get btn :path) btn)
       (treemacs--reentry (treemacs-button-get btn :path))
       (when (> expand-depth 0)
         (cl-decf expand-depth)
         (--each (treemacs--all-buttons-with-depth 0)
           (when (treemacs-is-node-collapsed? it)
             (goto-char (treemacs-button-start it))
             (treemacs-expand-extension-node expand-depth))))))))

(defun treemacs-update-async-node (path buffer)
  "Update an asynchronous node at PATH in the given BUFFER.

The update process will asynchronously pre-compute the children for every node
currently expanded under PATH.  The results of this computation will be cached
and then used to update the UI in one go."
  (-let [items-to-update (treemacs--get-async-update-items path)]
    (ht-set! treemacs--async-update-count path (length items-to-update))
    (dolist (item items-to-update)
      (let* ((item-path (car item))
             (ext (cdr item))
             (btn (treemacs-find-node item-path))
             (item (treemacs-button-get btn :item))
             (children-fn (treemacs-extension->children ext)))
        (funcall
         children-fn btn item
         (lambda (items)
           (treemacs--async-update-part-complete
            path item-path items buffer)))))))

(defun treemacs--get-async-update-items (path)
  "Get the items needed for an async update at the given PATH.
Every item in the returned list will consist of the node's key and its
extensions instance."
  (-let [items nil]
    (treemacs-walk-reentry-dom (treemacs-find-in-dom path)
      (lambda (dom-node)
        (let* ((key (treemacs-dom-node->key dom-node))
               (state (treemacs-button-get (treemacs-find-node key) :state))
               (ext (alist-get state treemacs--extension-registry)))
          (push (cons key ext) items))))
    items))

(defun treemacs--async-update-part-complete (top-path updated-path items buffer)
  "Partial completion for an asynchronous update.
TOP-PATH is the path of the node the update was called for.
UPDATED-PATH is the path of one of top node's children (may also be TOP-PATH)
whose content has just been computed.
ITEMS are the new items for the UPDATED-PATH that will be cached for the next
update.
BUFFER is the buffer where the node is located."
  (ht-set! treemacs--async-update-cache updated-path (or items 'nothing))
  (-let [count (cl-decf (ht-get treemacs--async-update-count top-path))]
    (when (= 0 count)
      (--when-let (buffer-live-p buffer)
        (with-current-buffer buffer
          (treemacs-with-writable-buffer
           (treemacs-update-node top-path)
           (treemacs-button-put (treemacs-find-node updated-path) :busy nil)))))))

(defun treemacs--do-collapse-extension-node (btn ext &optional __arg)
  "Collapse an extension button BTN for the given EXT.

BTN: Button
EXT: `treemacs-extensions' instance
ARG: Prefix Arg"
  (treemacs--button-close
   :button btn
   :new-state (treemacs-extension->get ext :closed-state)
   :new-icon
   (treemacs-extension->get ext :closed-icon btn (treemacs-button-get btn :item))
   :post-close-action
   (treemacs-on-collapse (treemacs-button-get btn :path))))

(cl-defmacro treemacs-initialize
    (extension
     &key
     (with-expand-depth 0)
     and-do)
  "Initialise an external buffer for use with the given EXTENSION.

EXTENSION is the same symbol that was passed as a `:key' argument
to `treemacs-define-variadic-entry-node-type'.

WITH-EXPAND-DEPTH indicates the number of nodes that should be expanded *in
addition* to the default.  If a value is given that is not a number then
treemacs will assume that *all* possible nodes should be expanded.

AND-DO can be used to set up buffer-local variables after the buffer has
switched over to `treemacs-mode'."
  (declare (indent 1))
  `(progn
     (treemacs--disable-fringe-indicator)
     (treemacs-with-writable-buffer
      (erase-buffer))
     ;; make sure the fringe indicator is enabled later, otherwise treemacs
     ;; attempts to move it right after the `treemacs-mode' call the indicator
     ;; cannot be created before either since the major-mode activation wipes
     ;; out buffer-local variables' values
     (let ((treemacs-fringe-indicator-mode nil)
           (treemacs--in-this-buffer t))
       (treemacs-mode))
     (setq-local treemacs-space-between-root-nodes nil)
     (setq-local treemacs--in-this-buffer :extension)
     ,and-do
     (treemacs--render-extension
      (let ((instance (treemacs--ext-symbol-to-instance ',extension)))
        (treemacs-static-assert
            (and instance (treemacs-extension->variadic? instance))
          "%s is not a variadic extension" ',extension)
        instance)
      (if (numberp ,with-expand-depth) ,with-expand-depth 999))
     (goto-char 1)
     (treemacs--evade-image)))

(provide 'treemacs-treelib)

;;; treemacs-treelib.el ends here
