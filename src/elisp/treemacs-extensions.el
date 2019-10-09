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
;;; API required for writing extensions for/with treemacs.

;;; Code:

(require 'dash)
(require 's)
(require 'treemacs-rendering)
(require 'treemacs-core-utils)
(require 'treemacs-fringe-indicator)
(eval-when-compile
  (require 'treemacs-macros)
  (require 'cl-lib))

(treemacs-import-functions-from "treemacs-mode"
  treemacs-mode)
(treemacs-import-functions-from "treemacs-rendering"
  treemacs--insert-root-separator)
(treemacs-import-functions-from "treemacs-visuals"
  treemacs--get-indentation)

(defmacro treemacs--build-extension-addition (name)
  "Internal building block.
Creates a `treemacs-define-${NAME}-extension' function and the necessary helpers."
  (let ((define-function-name  (intern (s-lex-format "treemacs-define-${name}-extension")))
        (top-extension-point (intern (s-lex-format "treemacs--${name}-top-extensions")))
        (bottom-extension-point   (intern (s-lex-format "treemacs--${name}-bottom-extensions"))))
    `(progn
       (defvar ,top-extension-point nil)
       (defvar ,bottom-extension-point nil)
       (cl-defun ,define-function-name (&key extension predicate position)
         ,(s-lex-format
           "Define an extension of type `${name}' for treemacs to use.
EXTENSION is an extension function, as created by `treemacs-define-expandable-node'
when a `:root' argument is given.

PREDICATE is a function that will be called to determine whether the extension
should be displayed. It is invoked with a single argument, which is the treemacs
project struct that is being expanded. All methods that can be invoked on this
type start with the `treemacs-project->' prefix.

POSITION is either `top' or `bottom', indicating whether the extension should be
rendered as the first or last element of a project.

See also `treemacs-remove-${name}-extension'.")
         (-let [cell (cons extension predicate)]
           (pcase position
             ('top    (add-to-list ',top-extension-point cell))
             ('bottom (add-to-list ',bottom-extension-point cell))
             (other   (error "Invalid extension position value `%s'" other)))
           t)))))

(defmacro treemacs--build-extension-removal (name)
  "Internal building block.
Creates a `treemacs-remove-${NAME}-extension' function and the necessary helpers."
  (let ((remove-function-name  (intern (s-lex-format "treemacs-remove-${name}-extension")))
        (top-extension-point (intern (s-lex-format "treemacs--${name}-top-extensions")))
        (bottom-extension-point   (intern (s-lex-format "treemacs--${name}-bottom-extensions"))))
    `(progn
       (cl-defun ,remove-function-name (extension posistion)
         ,(s-lex-format
           "Remove an EXTENSION of type `${name}' at a given POSITION.
   See also `treemacs-define-${name}-extension'.")
         (pcase posistion
           ('top
            (setq ,top-extension-point
                  (--reject (equal extension (car it)) ,top-extension-point)))
           ('bottom
            (setq ,bottom-extension-point
                  (--reject (equal extension (car it)) ,bottom-extension-point)))
           (other
            (error "Invalid extension position value `%s'" other)))
         t))))

(defmacro treemacs--build-extension-application (name)
  "Internal building block.
Creates treemacs--apply-${NAME}-top/bottom-extensions functions."
  (let ((apply-top-name         (intern (s-lex-format "treemacs--apply-${name}-top-extensions")))
        (apply-bottom-name      (intern (s-lex-format "treemacs--apply-${name}-bottom-extensions")))
        (top-extension-point    (intern (s-lex-format "treemacs--${name}-top-extensions")))
        (bottom-extension-point (intern (s-lex-format "treemacs--${name}-bottom-extensions"))))
    `(progn
       (defsubst ,apply-top-name (node data)
         ,(s-lex-format
           "Apply the top extensions for NODE of type `${name}'
Also pass additional DATA to predicate function.")
         (dolist (cell ,top-extension-point)
           (let ((extension (car cell))
                 (predicate (cdr cell)))
             (when (or (null predicate) (funcall predicate data))
               (funcall extension node)))))

       (defsubst ,apply-bottom-name (node data)
         ,(s-lex-format
           "Apply the bottom extensions for NODE of type `${name}'
Also pass additional DATA to predicate function.")
         (dolist (cell ,bottom-extension-point)
           (let ((extension (car cell))
                 (predicate (cdr cell)))
             (when (or (null predicate) (funcall predicate data))
               (funcall extension node))))))))

(treemacs--build-extension-addition "project")
(treemacs--build-extension-removal "project")
(treemacs--build-extension-application "project")
(treemacs--build-extension-addition "directory")
(treemacs--build-extension-removal "directory")
(treemacs--build-extension-application "directory")
(treemacs--build-extension-addition "top-level")
(treemacs--build-extension-removal "top-level")
(define-obsolete-function-alias 'treemacs-define-root-extension #'treemacs-define-top-level-extension "v2.4")
(define-obsolete-function-alias 'treemacs-remove-root-extension #'treemacs-remove-top-level-extension "v2.4")

;; slighty non-standard application for root extensions
(cl-macrolet
    ((define-root-extension-application (name variable doc)
       `(defun ,name (workspace &optional has-previous)
          ,doc
          (let ((is-first (not has-previous)))
            (--each ,variable
              (let ((extension (car it))
                    (predicate (cdr it)))
                (when (or (null predicate) (funcall predicate workspace))
                  (unless is-first
                    (treemacs--insert-root-separator))
                  (setq is-first (funcall extension)))))
            (not is-first)))))

  (define-root-extension-application
    treemacs--apply-root-top-extensions
    treemacs--top-level-top-extensions
    "Apply the top extensions for NODE of type `root' for the current WORKSPACE.

Returns t if extensions were inserted.")
  (define-root-extension-application
    treemacs--apply-root-bottom-extensions
    treemacs--top-level-bottom-extensions
    "Apply the bottom extensions for NODE of type `root' for the current WORKSPACE.

Returns t if extensions were inserted."))

(defsubst treemacs-as-icon (string &rest more-properties)
  "Turn STRING into an icon for treemacs.
Optionally include MORE-PROPERTIES (like `face' or `display')."
  (declare (indent 1))
  (apply #'propertize string 'icon t more-properties))

(cl-defmacro treemacs-render-node
    (&key icon
          label-form
          state
          face
          key-form
          more-properties)
  "Macro that produces the strings required to render a single treemacs node.
Meant to be used as a `:render-action' for `treemacs-define-expandable-node'.

ICON is a simple string serving as the node's icon, and must be created with
`treemacs-as-icon'. If the icon is for a file you can also use
`treemacs-icon-for-file'.

LABEL-FORM must return the string that will serve as the node's label text,
based on the element that should be rendered being bound as `item'. So for
example if rendering a list of buffers RENDER-FORM would look like
`(buffer-name item)'.

STATE is the symbol that will identify the type of the node.

FACE is its face.

KEY-FORM is the form that will give the node a unique key, necessary for
the node's (and the full custom tree's) ability to stay expanded and visible
when the project is refreshed, but also for compatiblity and integration with
`follow-mode' and `filewatch-mode.'

MORE-PROPERTIES is a plist of text properties that can arbitrarily added to the
node for quick retrieval later."
  (treemacs-static-assert (and icon label-form state key-form)
    "All values except :more-properties and :face are mandatory")
  `(let* ((path (append (treemacs-button-get node :path) (list ,key-form)))
          (dom-node (make-treemacs-dom-node :key path :parent parent-dom-node)))
     (treemacs-dom-node->insert-into-dom! dom-node)
     (when parent-dom-node
       (treemacs-dom-node->add-child! parent-dom-node dom-node))
     (list (unless (zerop depth) prefix)
           ,icon
           (propertize ,label-form
                       'button '(t)
                       'category 'default-button
                       ,@(when face `((quote face) ,face))
                       'help-echo nil
                       :custom t
                       :state ,state
                       :parent node
                       :depth depth
                       :path path
                       :key ,key-form
                       ,@more-properties)
           (when (zerop depth) (if treemacs-space-between-root-nodes "\n\n" "\n")))))

(cl-defmacro treemacs-define-leaf-node (name icon &key ret-action tab-action mouse1-action visit-action)
  "Define a type of node that is a leaf and cannot be further expanded.

Based on the given NAME this macro will define a `treemacs-${name}-state' state
variable and a `treemacs-${name}-icon' icon variable. If the icon should not be
static, and should be instead computed every time this node is rendered in its
parent's :render-action use 'dynamic-icon as a value for ICON.

The ICON is a string that should be created with `treemacs-as-icon'. If the icon
is for a file you can also use `treemacs-icon-for-file'.

RET-ACTION, TAB-ACTION and MOUSE1-ACTION are function references that will be
invoked when RET or TAB are pressed or mouse1 is double-clicked a node of this
type. VISIT-ACTION is used in `treemacs-visit-node-no-split' actions."
  (declare (indent 1))
  (let ((state-name (intern (format "treemacs-%s-state" name)))
        (icon-name  (intern (format "treemacs-%s-icon" name))))
    `(progn
       (defvar ,state-name ',state-name)
       ,(unless (equal icon (quote 'dynamic-icon))
          `(defvar ,icon-name ,icon))
       ,(when (or ret-action visit-action)
          `(treemacs-define-RET-action ,state-name ,(or ret-action '(quote treemacs-visit-node-default))))
       ,(when tab-action
          `(treemacs-define-TAB-action ,state-name ,tab-action))
       ,(when mouse1-action
          `(treemacs-define-doubleclick-action ,state-name ,mouse1-action))
       ,(when visit-action
          `(put ',state-name :treemacs-visit-action ,visit-action))
       t)))

(cl-defmacro treemacs-define-expandable-node
    (name &key
          icon-open
          icon-closed
          icon-open-form
          icon-closed-form
          query-function
          render-action
          ret-action
          visit-action
          after-expand
          after-collapse
          top-level-marker
          root-marker
          root-label
          root-face
          root-key-form)
  "Define a type of node that can be further expanded.

ICON-OPEN and ICON-CLOSED are strings and must be created by `treemacs-as-icon'.
They will be defvar'd as 'treemacs-icon-${name}-open/closed'.
As an alternative to static icons you can also supply ICON-OPEN-FORM and
ICON-CLOSED-FORM that will be dynamically executed whenever a new icon is
needed. Keep in mind that, since child nodes are first rendered by their
parents, a CLOSED-ICON-FORM will need to be repeated in the parent's
RENDER-ACTION.

QUERY-FUNCTION is a form and will be invoked when the node is expanded. It must
provide the list of elements that will be rendered with RENDER-ACTION.

RENDER-ACTION is another form that will render the single items provided by
QUERY-FUNCTION. For every RENDER-FORM invocation the element to be rendered is
bound under the name `item'. The form itself should end in a call to
`treemacs-render-node'.

RET-ACTION will define what function is called when RET is pressed on this type
of node. Only RET, without TAB and mouse1 can be defined since for expandable
nodes both TAB and RET should toggle expansion/collapse. VISIT-ACTION is used in
`treemacs-visit-node-no-split' actions.

AFTER-EXPAND and AFTER-COLLAPSE are optional forms that will be called after a
node has been expanded or collapsed. The closed or opened node marker will be
visible under the name `node' in their scope.

ROOT-MARKER is a simple boolean. It indicates the special case that the node
being defined is a top level entry point. When this value is non-nil this macro
will create an additional function in the form `treemacs-${NAME}-extension'
that can be passed to `treemacs-define-project-extension'. It also means that
the following pieces of additional information are required to render this node:

ROOT-LABEL is the displayed label of the root node.

ROOT-FACE is its face.

ROOT-KEY-FORM is the form that will give the root node its unique key, the same
way as the KEY-FORM argument in `treemacs-render-node'.

TOP-LEVEL-MARKER works much the same way as ROOT-MARKER (and is mutually
exclusive with it). The difference is that it declares the node defined here to
a top-level element with nothing above it, like a project, instead of a
top-level node *inside* a project. Other than that things work the same. Setting
TOP-LEVEL-MARKER will define a function named `treemacs-${NAME}-extension' that
can be passed to `treemacs-define-root-extension', and it requires the same
additional keys."
  (declare (indent 1))
  ;; TODO(2019/01/29): simplify
  (treemacs-static-assert
      (or (when top-level-marker (not root-marker))
          (when root-marker (not top-level-marker))
          (and (not root-marker) (not top-level-marker)))
    "Root and top-level markers cannot both be set.")
  (treemacs-static-assert (and (or icon-open-form icon-open)
                               (or icon-closed-form icon-closed)
                               query-function render-action)
    "All values (except additional root information) are mandatory")
  (treemacs-static-assert (or (null icon-open) (null icon-open-form))
    ":icon-open and :icon-open-form are mutually exclusive.")
  (treemacs-static-assert (or (null icon-closed) (null icon-closed-form))
    ":icon-closed and :icon-closed-form are mutually exclusive.")
  (let ((variadic?         (equal top-level-marker (quote 'variadic)))
        (open-icon-name    (intern (format "treemacs-icon-%s-open"    (symbol-name name))))
        (closed-icon-name  (intern (format "treemacs-icon-%s-closed"  (symbol-name name))))
        (open-state-name   (intern (format "treemacs-%s-open-state"   (symbol-name name))))
        (closed-state-name (intern (format "treemacs-%s-closed-state" (symbol-name name))))
        (expand-name       (intern (format "treemacs-expand-%s"       (symbol-name name))))
        (collapse-name     (intern (format "treemacs-collapse-%s"     (symbol-name name))))
        (do-expand-name    (intern (format "treemacs--do-expand-%s"   (symbol-name name))))
        (do-collapse-name  (intern (format "treemacs--do-collapse-%s" (symbol-name name)))))
    `(progn
       ,(when open-icon-name
          `(defvar ,open-icon-name ,icon-open))
       ,(when closed-icon-name
          `(defvar ,closed-icon-name ,icon-closed))
       (defvar ,open-state-name ',open-state-name)
       (defvar ,closed-state-name ',closed-state-name)

       (add-to-list 'treemacs--open-node-states ,open-state-name)
       (add-to-list 'treemacs--closed-node-states ,closed-state-name)

       (add-to-list 'treemacs-valid-button-states ,closed-state-name)
       (add-to-list 'treemacs-valid-button-states ,open-state-name)

       ,(when (or ret-action visit-action)
          `(progn
             (treemacs-define-RET-action ,open-state-name ,(or ret-action '(quote treemacs-visit-node-default)))
             (treemacs-define-RET-action ,closed-state-name ,(or ret-action '(quote treemacs-visit-node-default)))))

       ,@(when visit-action
           `((put ',open-state-name :treemacs-visit-action ,visit-action)
             (put ',closed-state-name :treemacs-visit-action ,visit-action)))

       (defun ,expand-name (&optional _)
         ,(format "Expand treemacs nodes of type `%s'." name)
         (interactive)
         (treemacs-block
          (-let [node (treemacs-node-at-point)]
            (when (null node)
              (treemacs-return
               (treemacs-pulse-on-failure "There is nothing to do here.")))
            (when (not (eq ',closed-state-name (treemacs-button-get node :state)))
              (treemacs-return
               (treemacs-pulse-on-failure "This function cannot expand a node of type '%s'."
                 (propertize (format "%s" (treemacs-button-get node :state)) 'face 'font-lock-type-face))))
            (,do-expand-name node))))

       (defun ,do-expand-name (node)
         ,(format "Execute expansion of treemacs nodes of type `%s'." name)
         (let ((items ,query-function)
               (depth (1+ (treemacs-button-get node :depth)))
               ;; must be implicitly in scope for calls to `treemacs-render-node'
               (parent-dom-node (treemacs-find-in-dom (treemacs-button-get node :path))))
           (treemacs--button-open
            :button node
            :new-state ',open-state-name
            :new-icon ,(unless variadic? (if icon-open open-icon-name icon-open-form))
            :immediate-insert t
            :open-action
            (treemacs--create-buttons
             :nodes items
             :depth depth
             :node-name item
             :node-action ,render-action)
            :post-open-action
            (progn
              (treemacs-on-expand (treemacs-button-get node :path) node)
              (treemacs--reentry (treemacs-button-get node :path))
              ,after-expand))))

       (defun ,collapse-name (&optional _)
         ,(format "Collapse treemacs nodes of type `%s'." name)
         (interactive)
         (treemacs-block
          (-let [node (treemacs-node-at-point)]
            (when (null node)
              (treemacs-return
               (treemacs-pulse-on-failure "There is nothing to do here.")))
            (when (not (eq ',open-state-name (treemacs-button-get node :state)))
              (treemacs-return
               (treemacs-pulse-on-failure "This function cannot collapse a node of type '%s'."
                 (propertize (format "%s" (treemacs-button-get node :state)) 'face 'font-lock-type-face))))
            (,do-collapse-name node))))

       (defun ,do-collapse-name (node)
         ,(format "Collapse treemacs nodes of type `%s'." name)
         (treemacs--button-close
          :button node
          :new-state ',closed-state-name
          :new-icon ,(unless variadic? (if icon-closed closed-icon-name icon-closed-form))
          :post-close-action
          (progn
            (treemacs-on-collapse (treemacs-button-get node :path))
            ,after-collapse)))

       (treemacs-define-TAB-action ',open-state-name #',collapse-name)
       (treemacs-define-TAB-action ',closed-state-name #',expand-name)

       ,(when root-marker
          (treemacs-static-assert (and root-label root-face root-key-form)
            ":root-label, :root-face and :root-key-form must be provided when `:root-marker' is non-nil")
          `(cl-defun ,(intern (format "treemacs-%s-extension" (upcase (symbol-name name)))) (parent)
             (let* ((depth (1+ (treemacs-button-get parent :depth)))
                    (path (list (or (treemacs-button-get parent :project)
                                    (treemacs-button-get parent :key))
                                ,root-key-form))
                    (parent-dom-node (treemacs-find-in-dom (treemacs-button-get parent :path)))
                    (new-dom-node (make-treemacs-dom-node :key path :parent parent-dom-node)))
               (treemacs-dom-node->insert-into-dom! new-dom-node)
               (when parent-dom-node
                 (treemacs-dom-node->add-child! parent-dom-node new-dom-node))
               (insert
                "\n"
                (treemacs--get-indentation depth)
                ,(if icon-closed closed-icon-name icon-closed-form)
                (propertize ,root-label
                            'button '(t)
                            'category 'default-button
                            'face ,root-face
                            :custom t
                            :key ,root-key-form
                            :path path
                            :depth depth
                            :no-git t
                            :parent parent
                            :state ,closed-state-name)))
             nil))

       ,(when top-level-marker
          (treemacs-static-assert (and root-label root-face root-key-form)
            ":root-label :root-face :root-key-form must be provided when `:top-level-marker' is non-nil")
          (let ((ext-name (intern (format "treemacs-%s-extension" (upcase (symbol-name name))))))
            (put ext-name :defined-in (or load-file-name (buffer-name)))
            `(progn
               ,(if variadic?
                    ;; When the extension is variadic it will be managed by a hidden top-level
                    ;; node. Its depth is -1 and it is not visible, but can still be used to update
                    ;; the entire extension without explicitly worrying about complex dom changes.
                    `(defun ,ext-name ()
                         (treemacs-with-writable-buffer
                          (save-excursion
                            (let* ((pr (make-treemacs-project
                                        :name ,root-label
                                        :path ,root-key-form
                                        :path-status 'extension))
                                   (button-start (point-marker))
                                   (dom-node (make-treemacs-dom-node
                                              :key ,root-key-form
                                              :position button-start)))
                              (treemacs--set-project-position ,root-key-form (point-marker))
                              (treemacs-dom-node->insert-into-dom! dom-node)
                              (insert (propertize "Hidden Node\n"
                                                  'button '(t)
                                                  'category 'default-button
                                                  'invisible t
                                                  'skip t
                                                  :custom t
                                                  :key ,root-key-form
                                                  :path (list :custom ,root-key-form)
                                                  :depth -1
                                                  :project pr
                                                  :state ,closed-state-name))
                              (let ((marker (copy-marker (point) t)))
                                (funcall ',do-expand-name button-start)
                                (goto-char marker)))))
                         t)
                  `(progn
                     (defun ,ext-name (&rest _)
                       (treemacs-with-writable-buffer
                        (-let [pr (make-treemacs-project
                                   :name ,root-label
                                   :path ,root-key-form
                                   :path-status 'extension)]
                          (insert ,(if icon-closed closed-icon-name icon-closed-form))
                          (treemacs--set-project-position ,root-key-form (point-marker))
                          (insert (propertize ,root-label
                                              'button '(t)
                                              'category 'default-button
                                              'face ,root-face
                                              :custom t
                                              :key ,root-key-form
                                              :path (list :custom ,root-key-form)
                                              :depth 0
                                              :project pr
                                              :state ,closed-state-name))))
                       nil)))))))))

(cl-defmacro treemacs-define-variadic-node
    (name &key
          query-function
          render-action
          root-key-form)
  "Define a variadic top-level node with given NAME.
The term \"variadic\" means that the node will produce an unknown amount of
child nodes when expanded. For example think of an extension that groups buffers
based on the major mode, with each major-mode being its own top-level group, so
it is not known which (if any) major-mode groupings exist.

Works the same as `treemacs-define-expandable-node', so the same restrictions and
rules apply for QUERY-FUNCTION, RENDER-ACTION and ROOT-KEY-FORM."
  (declare (indent 1))
  `(treemacs-define-expandable-node ,name
     :icon-closed ""
     :icon-open ""
     :root-label ""
     :root-face ""
     :top-level-marker 'variadic
     :query-function ,query-function
     :render-action ,render-action
     :root-key-form ,root-key-form))

(defun treemacs-initialize ()
  "Initialize treemacs in an external buffer for extension use."
  (setq-local treemacs--in-this-buffer :extension)
  (treemacs--disable-fringe-indicator)
  (treemacs-with-writable-buffer
   (erase-buffer))
  ;; make sure the fringe indicator is enabled later, otherwise treemacs attempts
  ;; to move it right after the `treemacs-mode' call
  ;; the indicator cannot be created before either since the major-mode activation
  ;; wipes out buffer-local variables' values
  (-let [treemacs-fringe-indicator-mode nil]
    (treemacs-mode))
  (when treemacs-fringe-indicator-mode
    (treemacs--enable-fringe-indicator)))

(provide 'treemacs-extensions)

;;; treemacs-extensions.el ends here
