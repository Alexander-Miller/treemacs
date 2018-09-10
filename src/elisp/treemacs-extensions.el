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
;;; API required for writing extensions for/with treemacs.

;;; Code:

(require 'dash)
(require 'treemacs-branch-creation)
(require 'treemacs-impl)
(require 'treemacs-interface)
(eval-when-compile
  (require 'cl-lib))

(defvar treemacs--project-start-extensions nil)
(defvar treemacs--project-end-extensions nil)

(defun treemacs-define-extension (ext pos)
  "Define an extension for treemacs to use.
EXT is an extension function, as created by `treemacs-define-expandable-node'
when a `:root' argument is given.
POS is either `project-start' or `project-end', indicating whether the
extension should be rendered as the first or last element of a project.

See also `treemacs-remove-extension'."
  (pcase pos
    ('project-start (add-to-list 'treemacs--project-start-extensions ext))
    ('project-end   (add-to-list 'treemacs--project-end-extensions ext))
    (other          (error "Invalid extension point value `%s'" other))))

(defun treemacs-remove-extension (ext pos)
  "Remove an extension EXT at position POS.
See also `treemacs-define-extension'."
  (pcase pos
    ('project-start (setq treemacs--project-start-extensions (delete ext treemacs--project-start-extensions)))
    ('project-end   (setq treemacs--project-end-extensions   (delete ext treemacs--project-end-extensions)))
    (other          (error "Invalid extension point value `%s'" other))))

(defsubst treemacs--apply-project-start-extensions (project-btn)
  "Apply the extension for PROJECT-BTN at the start of the project."
  (dolist (ext treemacs--project-start-extensions)
    (funcall ext project-btn)))

(defsubst treemacs--apply-project-end-extensions (project-btn)
  "Apply the extension for PROJECT-BTN at the end of the project."
  (dolist (ext treemacs--project-end-extensions)
    (funcall ext project-btn)))

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
`treemacs-as-icon'.

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
  (cl-assert (and icon label-form state face key-form)
             :show-args "All values except `more-properties' are mandatory")
  `(list prefix ,icon
         (propertize ,label-form
                     'button '(t)
                     'category 'default-button
                     'face ,face
                     'help-echo nil
                     :state ,state
                     :parent btn
                     :depth depth
                     :key ,key-form
                     ,@more-properties)))

(cl-defmacro treemacs-define-leaf-node (name icon &key ret-action)
  "Define a type of node that is a leaf and cannot be further expanded.

Based on the given NAME this macro will define a `treemacs-${name}-state' state
variable and a `treemacs-${name}-icon' icon variable.

The ICON is a string that should be created with `treemacs-as-icon'.

RET-ACTION is a function reference that will be invoked when RET is pressed on
a node of this type."
  (declare (indent 1))
  (let ((state-name (intern (format "treemacs-%s-state" name)))
        (icon-name  (intern (format "treemacs-%s-icon" name))))
    `(progn
       (defvar ,state-name ',state-name)
       (defvar ,icon-name ,icon)
       ,(when ret-action
          `(treemacs-define-RET-action ,state-name ,ret-action)))))

(cl-defmacro treemacs-define-expandable-node
    (name &key
          icon-open
          icon-closed
          query-function
          render-action
          root-marker
          root-label
          root-face
          root-key-form)
  "Define a type of node that can be further expanded.

ICON-OPEN and ICON-CLOSED are strings and must be created by `treemacs-as-icon'.

QUERY-FUNCTION is a form and will be invoked when the node is expanded. It must
provide the list of elements that will be rendered with RENDER-ACTION.

RENDER-ACTION is another form that will render the single items provided by
QUERY-FUNCTION. For every RENDER-FORM invocation the element to be rendered is
bound under the name `item'. The form itself should end in a call to
`treemacs-render-node'.

ROOT-MARKER is a simple boolean. It indicates the special case that the node
being defined is a top level entry point. When this value is non-nil this macro
will created an additional function in the form `treemacs-${NAME}-extension'
that can be passed to `treemacs-define-extension'. It also means that the
following pieces of additional information are required to render this node:

ROOT-LABEL is the displayed label of the root node.

ROOT-FACE is its face.

ROOT-KEY-FORM is the form that will give the root node its unique key, the same
way as the KEY-FORM argument in `treemacs-render-node'."
  (declare (indent 1))
  (cl-assert (and icon-open icon-closed query-function render-action)
             :show-args "All values (except additional root information) are mandatory")
  (let ((open-icon-name    (intern (format "treemacs-icon-%s-open"    (symbol-name name))))
        (closed-icon-name  (intern (format "treemacs-icon-%s-closed"  (symbol-name name))))
        (open-state-name   (intern (format "treemacs-%s-open-state"   (symbol-name name))))
        (closed-state-name (intern (format "treemacs-%s-closed-state" (symbol-name name))))
        (expand-name       (intern (format "treemacs-expand-%s"       (symbol-name name))))
        (collapse-name     (intern (format "treemacs-collapse-%s"     (symbol-name name))))
        (do-expand-name    (intern (format "treemacs--do-expand-%s"   (symbol-name name))))
        (do-collapse-name  (intern (format "treemacs--do-collapse-%s" (symbol-name name)))))
    `(progn
       (defvar ,open-icon-name ,icon-open)
       (defvar ,closed-icon-name ,icon-closed)
       (defvar ,open-state-name ',open-state-name)
       (defvar ,closed-state-name ',closed-state-name)

       (add-to-list 'treemacs--open-node-states ,open-state-name)
       (add-to-list 'treemacs--closed-node-states ,closed-state-name)

       (add-to-list 'treemacs-valid-button-states ,closed-state-name)
       (add-to-list 'treemacs-valid-button-states ,open-state-name)

       (defun ,expand-name (&optional _)
         ,(format "Expand treemacs nodes of type `%s'." name)
         (interactive)
         (cl-block body
           (-let [btn (treemacs-current-button)]
             (when (null btn)
               (cl-return-from body
                 (treemacs-pulse-on-failure "There is nothing to do here.")))
             (when (not (eq ',closed-state-name (button-get btn :state)))
               (cl-return-from body
                 (treemacs-pulse-on-failure "This function cannot expand a node of type '%s'."
                   (propertize (format "%s" (button-get btn :state)) 'face 'font-lock-type-face))))
             (,do-expand-name btn))))

       (defun ,do-expand-name (btn)
         ,(format "Execute expansion of treemacs nodes of type `%s'." name)
         (let ((items ,query-function)
               (depth (1+ (button-get btn :depth))))
           (treemacs--button-open
            :button btn
            :new-state ',open-state-name
            :new-icon ,open-icon-name
            :immediate-insert t
            :open-action
            (treemacs--create-buttons
             :nodes items
             :depth depth
             :node-name item
             :node-action ,render-action)
            :post-open-action
            (progn
              (treemacs-on-expand
               (button-get btn :key) btn (-some-> (button-get btn :parent) (button-get :key)))))))

       (defun ,collapse-name (&optional _)
         ,(format "Collapse treemacs nodes of type `%s'." name)
         (interactive)
         (cl-block body
           (-let [btn (treemacs-current-button)]
             ;; TODO(2018/09/02): automate return from as macro
             (when (null btn)
               (cl-return-from body
                 (treemacs-pulse-on-failure "There is nothing to do here.")))
             (when (not (eq ',open-state-name (button-get btn :state)))
               (cl-return-from body
                 (treemacs-pulse-on-failure "This function cannot collapse a node of type '%s'."
                   (propertize (format "%s" (button-get btn :state)) 'face 'font-lock-type-face))))
             (,do-collapse-name btn))))

       (defun ,do-collapse-name (btn)
         ,(format "Collapse treemacs nodes of type `%s'." name)
         (treemacs--button-close
          :button btn
          :new-state ',closed-state-name
          :new-icon ,closed-icon-name
          :post-close-action
          (treemacs-on-collapse (button-get btn :key))))

       (treemacs-define-TAB-action ',open-state-name #',collapse-name)
       (treemacs-define-TAB-action ',closed-state-name #',expand-name)

       ,(when root-marker
          (cl-assert (and root-label root-face root-key-form)
                     :show-args "Root information must be provided when `:root-marker' is non-nil")
          `(cl-defun ,(intern (format "treemacs-%s-extension" (upcase (symbol-name name)))) (parent)
             (insert
              "\n"
              (s-repeat treemacs-indentation treemacs-indentation-string)
              ,closed-icon-name
              (propertize ,root-label
                          'button '(t)
                          'category 'default-button
                          'face ,root-face
                          :key ,root-key-form
                          :depth 1
                          :no-git t
                          :parent parent
                          :state ,closed-state-name)))))))


(provide 'treemacs-extensions)

;;; treemacs-extensions.el ends here
