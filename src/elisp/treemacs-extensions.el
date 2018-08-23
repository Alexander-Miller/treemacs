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

(defsubst treemacs-as-icon (string &rest more-properties)
  "Turn STRING into an icon for treemacs.
Optionally include MORE-PROPERTIES (like `face' or `display')."
  (declare (indent 1))
  (apply #'propertize string 'icon t more-properties))

(defmacro treemacs-render-node (icon label-form state face &rest more-properties)
  "Macro that produces the strings required to render a single treemacs node.
To be used as a `:render-action' for `treemacs-define-expandable-node'.

ICON is a simple string serving as the node's icon, and must be created with
`treemacs-as-icon'.

LABEL-FORM must return the string that will servce as the node's label text,
based on the element that should be rendered being bound as `item'. So for
example if rendering a list of buffers RENDER-FORM would look like
`(buffer-name item)'.

STATE is the symbol that will identify the type of the node.

FACE is its face.

MORE-PROPERTIES can arbitrarily appended for quick retrieval later."
  (declare (indent 2))
  `(list prefix ,icon
         (propertize ,label-form
                     'button '(t)
                     'category 'default-button
                     'face ,face
                     'help-echo nil
                     :state ,state
                     :parent btn
                     :depth depth
                     ,@more-properties)))

(cl-defmacro treemacs-define-leaf-node (name icon)
  "Define a type of node that is a leaf and cannot be further expanded.
Based on the given NAME this macro will define a `treemacs-%s-state' state
variable and a `treemacs-%s-icon' icon variable.
The ICON is a string that should be created with `treemacs-as-icon'."
  (declare (indent 1))
  (let ((state-name (intern (format "treemacs-%s-state" name)))
        (icon-name  (intern (format "treemacs-%s-icon" name))))
    `(progn
       (defvar ,state-name ',state-name)
       (defvar ,icon-name ,icon))))

(cl-defmacro treemacs-define-expandable-node
    (name &key
          icon-open
          icon-closed
          query-function
          render-action
          root)
  "Define a type of node that can be further expanded.

ICON-OPEN and ICON-CLOSED are strings and must be created by `treemacs-as-icon'.

QUERY-FUNCTION is a form and will be invoked when the node is expanded. It must
provide the list of elements that will be rendered with RENDER-ACTION.

RENDER-ACTION is another form that will render the single items provided by
QUERY-FUNCTION. For every RENDER-FORM invocation the element to be rendered is
bound under the name `item'. The form itself should end in a call to
`treemacs-render-node'."
  (declare (indent 1))
  (let ((open-icon-name    (intern (concat "treemacs-icon-" (symbol-name name) "-open")))
        (closed-icon-name  (intern (concat "treemacs-icon-" (symbol-name name) "-closed")))
        (open-state-name   (intern (concat "treemacs-" (symbol-name name) "-open-state")))
        (closed-state-name (intern (concat "treemacs-" (symbol-name name) "-closed-state")))
        (expand-name       (intern (concat "treemacs--expand-" (symbol-name name))))
        (collapse-name     (intern (concat "treemacs--collapse-" (symbol-name name)))))
    `(progn
       (defvar ,open-icon-name ,icon-open)
       (defvar ,closed-icon-name ,icon-closed)
       (defvar ,open-state-name ',open-state-name)
       (defvar ,closed-state-name ',closed-state-name)

       (defun ,expand-name (_)
         ,(format "Expand treemacs nodes of type `%s'." name)
         (-when-let (btn (treemacs-current-button))
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
               :node-action ,render-action)))))

       (defun ,collapse-name (_)
         ,(format "Collapse treemacs nodes of type `%s'." name)
         (-when-let (btn (treemacs-current-button))
           (treemacs--button-close
            :button btn
            :new-state ',closed-state-name
            :new-icon ,closed-icon-name)))

       (treemacs-define-TAB-action ',open-state-name #',collapse-name)
       (treemacs-define-TAB-action ',closed-state-name #',expand-name)

       ,(when root
          (-let [(label face) root]
            `(cl-defun ,(intern (format "treemacs--render-%s-node" name)) (&optional (depth 0) parent)
               (insert
                ,closed-icon-name
                (propertize ,label
                            'button '(t)
                            'category 'default-button
                            'state ,closed-state-name
                            :depth depth
                            :parent parent
                            :state ,closed-state-name
                            'face ,face))))))))

(provide 'treemacs-extensions)

;;; treemacs-extensions.el ends here
