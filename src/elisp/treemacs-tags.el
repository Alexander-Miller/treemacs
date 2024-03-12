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

;; Tags display functionality.

;; Need to be very careful here - many of the functions in this module
;; need to be run inside the treemacs buffer, while the
;; `treemacs--execute-button-action' macro that runs them will switch
;; windows before doing so.  Heavy use of `treemacs-safe-button-get'
;; or `treemacs-with-button-buffer' is necessary.

;; NOTE: This module is lazy-loaded.

;;; Code:

(require 'xref)
(require 'imenu)
(require 'dash)
(require 'treemacs-core-utils)
(require 'treemacs-rendering)
(require 'treemacs-customization)
(require 'treemacs-faces)
(require 'treemacs-visuals)
(require 'treemacs-dom)
(require 'treemacs-icons)
(require 'treemacs-logging)

(eval-when-compile
  (require 'inline)
  (require 'cl-lib)
  (require 'treemacs-macros))

(treemacs-import-functions-from "treemacs"
  treemacs-select-window)

(treemacs-import-functions-from "org-comat"
  org-imenu-get-tree)

;; TODO(2019/10/17): rebuild this module using the extension api
;; TODO(2020/12/14): Improve special-casing of org-mode & especially pdf-tools

(defun treemacs--partition-imenu-index (index default-name)
  "Put top level leaf nodes in INDEX under DEFAULT-NAME."
  (declare (pure t) (side-effect-free t))
  (let ((ret)
        (rest index))
    (while rest
      (let ((item (car rest)))
        (if (imenu--subalist-p item)
            (progn
              (setq ret (cons item ret))
              (setq rest (cdr rest)))
          (progn
            (setq ret (cons (cons default-name rest) ret)
                  rest nil)))))
    (nreverse ret)))

;; this function was recently added in emacs 26 (as of august 2017)
;; code copied here for earler releases
(defun treemacs--provided-mode-derived-p (mode &rest modes)
  "Non-nil if MODE is derived from one of MODES.
Uses the `derived-mode-parent' property of the symbol to trace backwards.
If you just want to check `major-mode', use `derived-mode-p'."
  (while (and (not (memq mode modes))
              (setq mode (get mode 'derived-mode-parent))))
  mode)

(defun treemacs--post-process-index (index index-mode)
  "Post process a tags INDEX for the major INDEX-MODE the tags were gathered in.
As of now this only decides which (if any) section name the top level leaves
should be placed under."
  (declare (pure t) (side-effect-free t))
  (pcase index-mode
    ((or 'markdown-mode 'org-mode 'python-mode)
     index)
    ((guard (treemacs--provided-mode-derived-p index-mode 'conf-mode))
     (treemacs--partition-imenu-index index "Sections"))
    (_
     (treemacs--partition-imenu-index index "Functions"))))

(defun treemacs--get-imenu-index (file)
  "Fetch imenu index of FILE."
  (let ((buff)
        (result)
        (mode)
        (existing-buffer (get-file-buffer
                          (or (file-symlink-p file) file)))
        (org-imenu-depth (max 10 (or 0 (and (boundp 'org-imenu-depth) org-imenu-depth)))))
    (ignore org-imenu-depth)
    (if existing-buffer
        (setq buff existing-buffer)
      (cl-letf (((symbol-function 'run-mode-hooks) (symbol-function 'ignore)))
        (setq buff (find-file-noselect file))))
    (condition-case e
        (when (buffer-live-p buff)
          (with-current-buffer buff
            (let ((imenu-generic-expression
                   (if (eq major-mode 'emacs-lisp-mode)
                       (or treemacs-elisp-imenu-expression
                           imenu-generic-expression)
                     imenu-generic-expression))
                  (imenu-create-index-function
                   (if (eq major-mode 'org-mode)
                       #'org-imenu-get-tree
                     imenu-create-index-function)))
              (setf result (and (or imenu-generic-expression imenu-create-index-function)
                                (imenu--make-index-alist t))
                    mode major-mode)))
          (unless existing-buffer (kill-buffer buff))
          (when result
            (when (string= "*Rescan*" (caar result))
              (setf result (cdr result)))
            (unless (equal result '(nil))
              (treemacs--post-process-index result mode))))
      (imenu-unavailable (ignore e))
      (error (prog1 nil (treemacs-log-err "Encountered error while following tag at point: %s" e))))))

(define-inline treemacs--insert-tag-leaf (item path prefix parent depth)
  "Return the text to insert for a tag leaf ITEM with given PATH.
Use PREFIX for indentation.
Set PARENT and DEPTH button properties.
ITEM: String . Marker
PREFIX: String
PARENT: Button
DEPTH: Int"
  (inline-letevals (item prefix parent depth)
    (inline-quote
     (list
      ,prefix
      (propertize (car ,item)
                  'button '(t)
                  'category t
                  'face 'treemacs-tags-face
                  'help-echo nil
                  :path ,path
                  :key (car ,item)
                  :state 'tag-node
                  :parent ,parent
                  :depth ,depth
                  :marker (cdr ,item))))))

(define-inline treemacs--insert-tag-node (node path prefix parent depth)
  "Return the text to insert for a tag NODE with given tag PATH.
Use PREFIX for indentation.
Set PARENT and DEPTH button properties.

NODE: String & List of (String . Marker)
PATH: Tag Path
PREFIX: String
PARENT: Button
DEPTH: Int"
  (inline-letevals (node prefix parent depth)
    (inline-quote
     (list
      ,prefix
      (propertize (car ,node)
                  'button '(t)
                  'category t
                  'face 'treemacs-tags-face
                  'help-echo nil
                  :path ,path
                  :key (car ,node)
                  :state 'tag-node-closed
                  :parent ,parent
                  :depth ,depth
                  :index (cdr ,node))))))

;;;###autoload
(defun treemacs--expand-file-node (btn &optional recursive)
  "Open tag items for file BTN.
Recursively open all tags below BTN when RECURSIVE is non-nil."
  (let* ((path (treemacs-button-get btn :path))
         (parent-dom-node (treemacs-find-in-dom path))
         (recursive (treemacs--prefix-arg-to-recurse-depth recursive)))
    (-if-let (index (treemacs--get-imenu-index path))
        (treemacs--button-open
         :button btn
         :immediate-insert t
         :new-state 'file-node-open
         :open-action
         (treemacs--create-buttons
          :nodes index
          :extra-vars
          ((node-prefix (concat prefix treemacs-icon-tag-closed))
           (leaf-prefix (concat prefix treemacs-icon-tag-leaf)))
          :depth (1+ (treemacs-button-get btn :depth))
          :node-name item
          :node-action (if (imenu--subalist-p item)
                           (treemacs--insert-tag-node item (list path (car item)) node-prefix btn depth)
                         (treemacs--insert-tag-leaf item (list path (car item)) leaf-prefix btn depth)))
         :post-open-action
         (progn
           (-let [dom-nodes
                  (--map (treemacs-dom-node->create!
                          :key (list path (car it))
                          :parent parent-dom-node)
                         index)]
             (-each dom-nodes #'treemacs-dom-node->insert-into-dom!)
             (setf (treemacs-dom-node->children parent-dom-node)
                   (nconc dom-nodes (treemacs-dom-node->children parent-dom-node))))
           (treemacs-on-expand path btn)
           (treemacs--reentry path)
           (end-of-line)
           (when (> recursive 0)
             (cl-decf recursive)
             (--each (treemacs-collect-child-nodes btn)
               (when (eq 'tag-node-closed (treemacs-button-get it :state))
                 (goto-char (treemacs-button-start it))
                 (treemacs--expand-tag-node it t))))))
      (treemacs-pulse-on-failure "No tags found for %s" (propertize path 'face 'font-lock-string-face)))))

;;;###autoload
(defun treemacs--collapse-file-node (btn &optional recursive)
  "Close node given by BTN.
Remove all open tag entries under BTN when RECURSIVE."
  (treemacs--button-close
   :button btn
   :new-state 'file-node-closed
   :post-close-action (treemacs-on-collapse (treemacs-button-get btn :path) recursive)))

;;;###autoload
(defun treemacs--visit-or-expand/collapse-tag-node (btn arg find-window)
  "Visit tag section BTN if possible, expand or collapse it otherwise.
Pass prefix ARG on to either visit or toggle action.

FIND-WINDOW is a special provision depending on this function's invocation
context and decides whether to find the window to display in (if the tag is
visited instead of the node being expanded).

On the one hand it can be called based on `treemacs-RET-actions-config' (or
TAB).  The functions in these configs are expected to find the windows they need
to display in themselves, so FIND-WINDOW must be t. On the other hand this
function is also called from the top level vist-node functions like
`treemacs-visit-node-vertical-split' which delegates to the
`treemacs--execute-button-action' macro which includes the determination of
the display window."
  (let* ((path (treemacs--nearest-path btn))
         (extension (file-name-extension path)))
    (pcase extension
      ("py"
       (let* ((first-child (car (treemacs-button-get btn :index)))
              (name (car first-child))
              (marker (cdr first-child)))
         ;; name of first subelement of a section node ends in "definition" means we have a function
         ;; nested inside a function, so we move to the definition here instead of expanding
         (if (not (s-ends-with? " definition*" name))
             (treemacs--expand-tag-node btn arg)
           ;; select the window as visit-no-split would
           (when find-window
             (--if-let (-some-> path (get-file-buffer) (get-buffer-window))
                 (select-window it)
               (other-window 1)))
           (find-file path)
           (if (buffer-live-p (marker-buffer marker))
               (goto-char marker)
             ;; marker is stale and we need a live child button to grab its tag path to re-call imenu, but the
             ;; child button may not be there so we briefly expand the button to grab the first child to whose
             ;; position we need to move
             (-let [need-to-close-section nil]
               (treemacs-with-button-buffer btn
                 (when (eq 'tag-node-closed (treemacs-button-get btn :state))
                   (setq need-to-close-section t)
                   (treemacs--expand-tag-node btn)))
               (treemacs--call-imenu-and-goto-tag
                (treemacs-with-button-buffer btn (treemacs-button-get (next-button (treemacs-button-end btn)) :path)))
               (when need-to-close-section
                 (treemacs-with-button-buffer btn
                   (treemacs--collapse-tag-node btn))))
             (when arg (treemacs-select-window))))))
      ("org"
       (treemacs-unless-let (pos (treemacs-button-get btn 'org-imenu-marker))
           (treemacs--expand-tag-node btn arg)
         ;; select the window as visit-no-split would
         (when find-window
           (--if-let (-some-> path (get-file-buffer) (get-buffer-window))
               (select-window it)
             (other-window 1)))
         (find-file path)
         (if (marker-position pos)
             (goto-char pos)
           (treemacs--call-imenu-and-goto-tag (treemacs-with-button-buffer btn (treemacs-button-get btn :path)) t))))
      (_ (pcase (treemacs-button-get btn :state)
           ('tag-node-open   (treemacs--collapse-tag-node btn arg))
           ('tag-node-closed (treemacs--expand-tag-node btn arg)))))))

;;;###autoload
(defun treemacs--expand-tag-node (btn &optional recursive)
  "Open tags node items for BTN.
Open all tag section under BTN when call is RECURSIVE."
  (let* ((index (treemacs-button-get btn :index))
         (tag-path (treemacs-button-get btn :path))
         (parent-dom-node (treemacs-find-in-dom tag-path))
         (recursive (treemacs--prefix-arg-to-recurse-depth recursive)))
    (treemacs--button-open
     :button btn
     :immediate-insert t
     :new-state 'tag-node-open
     :new-icon treemacs-icon-tag-open
     :open-action (treemacs--create-buttons
                   :nodes index
                   :depth (1+ (treemacs-button-get btn :depth))
                   :node-name item
                   :extra-vars ((leaf-prefix (concat prefix treemacs-icon-tag-leaf))
                                (node-prefix (concat prefix treemacs-icon-tag-closed)))
                   :node-action (if (imenu--subalist-p item)
                                    (treemacs--insert-tag-node
                                     item (append tag-path (list (car item))) node-prefix btn depth)
                                  (treemacs--insert-tag-leaf
                                   item (append tag-path (list (car item))) leaf-prefix btn depth)))
     :post-open-action
     (progn
       (-let [dom-nodes
              (--map (treemacs-dom-node->create!
                      :key (append tag-path (list (car it)))
                      :parent parent-dom-node)
                     index)]
         (-each dom-nodes #'treemacs-dom-node->insert-into-dom!)
         (setf (treemacs-dom-node->children parent-dom-node)
               (nconc dom-nodes (treemacs-dom-node->children parent-dom-node))))
       (treemacs-on-expand tag-path btn)
       (if (> recursive 0)
           (progn
             (cl-decf recursive)
             (--each (treemacs-collect-child-nodes btn)
               (when (eq 'tag-node-closed (treemacs-button-get it :state))
                 (goto-char (treemacs-button-start it))
                 (treemacs--expand-tag-node it t))))
         (treemacs--reentry tag-path))))))

(defun treemacs--collapse-tag-node-recursive (btn)
  "Recursively close tag section BTN.
Workaround for tag section having no easy way to purge all open tags below a
button from cache.  Easiest way is to just do it manually here."
  (--each (treemacs-collect-child-nodes btn)
    (when (eq 'tag-node-open (treemacs-button-get it :state))
      (treemacs--collapse-tag-node-recursive it)
      (goto-char (treemacs-button-start it))
      (treemacs--collapse-tag-node it)))
  (goto-char (treemacs-button-start btn))
  (treemacs--collapse-tag-node btn))

;;;###autoload
(defun treemacs--collapse-tag-node (btn &optional recursive)
  "Close tags node at BTN.
Remove all open tag entries under BTN when RECURSIVE."
  (if recursive
      (treemacs--collapse-tag-node-recursive btn)
    (treemacs--button-close
     :button btn
     :new-state 'tag-node-closed
     :new-icon treemacs-icon-tag-closed
     :post-close-action
     (treemacs-on-collapse (treemacs-button-get btn :path)))))

(defun treemacs--extract-position (item file)
  "Extract a tag's position stored in ITEM and FILE.
The position can be stored in the following ways:

* ITEM is a marker pointing to a tag provided by imenu.
* ITEM is an overlay pointing to a tag provided by imenu with semantic mode.
* ITEM is a raw number pointing to a buffer position.
* ITEM is a cons: special case for imenu elements of an `org-mode' buffer.
  ITEM is an imenu sub-tree and the position is stored as a marker in the first
  element's \\='org-imenu-marker text property.
* ITEM is a cons: special case for imenu elements of an `pdfview-mode' buffer.
  In this case no position is stored directly, navigation to the tag must happen
  via callback

FILE is the path the tag is in, so far it is only relevant for `pdfview-mode'
tags."
  (declare (side-effect-free t))
  (pcase (type-of item)
    ('marker
     (cons (marker-buffer item) (marker-position item)))
    ('overlay
     (cons (overlay-buffer item) (overlay-start item)))
    ('integer
     (cons nil item))
    ('cons
     (cond
      ((eq 'pdf-outline-imenu-activate-link (cadr item))
       (with-no-warnings
         (cons (find-buffer-visiting file) (lambda () (apply #'pdf-outline-imenu-activate-link item)))))
      ((get-text-property 0 'org-imenu-marker (car item))
       (-let [org-marker (get-text-property 0 'org-imenu-marker (car item))]
         (cons (marker-buffer org-marker) (marker-position org-marker))))))))

(defun treemacs--call-imenu-and-goto-tag (tag-path &optional org?)
  "Call the imenu index of the tag at TAG-PATH and go to its position.
ORG? should be t when this function is called for an org buffer and index since
org requires a slightly different position extraction because the position of a
headline with sub-elements is saved in an `org-imenu-marker' text property."
  (let* ((file (car tag-path))
         (path (-butlast (cdr tag-path)))
         (tag (-last-item tag-path)))
    (condition-case e
        (progn
          (find-file-noselect file)
          (let ((index (treemacs--get-imenu-index file)))
            (dolist (path-item path)
              (setq index (cdr (assoc path-item index))))
            (-let [(buf . pos) (treemacs--extract-position
                                (-let [item (--first
                                             (equal (car it) tag)
                                             index)]
                                  (if org? item (cdr item)))
                                (car tag-path))]
              ;; some imenu implementations, like markdown, will only provide
              ;; a raw buffer position (an int) to move to
              (switch-to-buffer (or buf (get-file-buffer file)))
              (if (functionp pos)
                  (funcall pos)
                (goto-char pos))
              ;; a little bit of convenience - reveal those nested headlines
              (when (and (eq major-mode 'org-mode)
                         (fboundp 'org-reveal))
                (org-reveal)))))
      (error
       (treemacs-log-err "Something went wrong when finding tag '%s': %s"
         (propertize tag 'face 'treemacs-tags-face)
         e)))))

;;;###autoload
(defun treemacs--goto-tag (btn)
  "Go to the tag at BTN."
  ;; The only code currently calling this is run through `treemacs--execute-button-action' which always
  ;; switches windows before running it, so we need to be really careful here when querying any button
  ;; properties.
  (let* ((tag-buffer) (tag-pos))
    (treemacs-with-button-buffer btn
      (-let [info (treemacs--extract-position
                   (treemacs-button-get btn :marker)
                   (car (treemacs-button-get btn :path)))]
        (setf tag-buffer (car info)
              tag-pos (cdr info))))
    (if (not (buffer-live-p tag-buffer))
        (pcase treemacs-goto-tag-strategy
          ('refetch-index
           (treemacs--call-imenu-and-goto-tag
            (treemacs-safe-button-get btn :path)))
          ('call-xref
           (xref-find-definitions
            (treemacs-with-button-buffer btn
              (treemacs--get-label-of btn))))
          ('issue-warning
           (treemacs-pulse-on-failure
               "Tag '%s' is located in a buffer that does not exist."
             (propertize (treemacs-with-button-buffer btn (treemacs--get-label-of btn)) 'face 'treemacs-tags-face)))
          (_ (error "[Treemacs] '%s' is an invalid value for treemacs-goto-tag-strategy" treemacs-goto-tag-strategy)))
      (progn
        (switch-to-buffer tag-buffer nil t)
        ;; special case for pdf mode buffers - their imenu tags do not store a marker
        ;; movement must happen via a special callback
        (cond
         ((numberp tag-pos)
          (goto-char tag-pos))
         ((functionp tag-pos)
          (funcall tag-pos)))
        ;; a little bit of convenience - reveal those nested headlines
        (when (and (eq major-mode 'org-mode) (fboundp 'org-reveal))
          (org-reveal))))))

;;;###autoload
(defun treemacs--create-imenu-index-function ()
  "The `imenu-create-index-function' for treemacs buffers."
  (declare (side-effect-free t))
  (let (index)
    (pcase treemacs-imenu-scope
      ('everything
       (dolist (project (treemacs-workspace->projects (treemacs-current-workspace)))
         (let ((project-name (treemacs-project->name project))
               (root-dom-node (treemacs-find-in-dom (treemacs-project->path project))))
           (-when-let (index-items (treemacs--get-imenu-index-items root-dom-node))
             (push (cons project-name index-items) index)))))
      ('current-project
       (treemacs-unless-let (project (treemacs-project-at-point))
           (treemacs-pulse-on-failure "Cannot create imenu index because there is no project at point")
         (let ((project-name (treemacs-project->name project))
               (root-dom-node (treemacs-find-in-dom (treemacs-project->path project))))
           (-when-let (index-items (treemacs--get-imenu-index-items root-dom-node))
             (push (cons project-name index-items) index)))))
      (other (error "Invalid imenu scope value `%s'" other)))
    (nreverse index)))

(defun treemacs--get-imenu-index-items (project-dom-node)
  "Collects the imenu index items for the given PROJECT-DOM-NODE."
  (declare (side-effect-free t))
  (let (result)
    (treemacs-walk-dom project-dom-node
      (lambda (node)
        (-let [node-btn (or (treemacs-dom-node->position node)
                            (treemacs-find-node (treemacs-dom-node->key node)))]
          (push (list (if (treemacs-button-get node-btn :custom)
                          (treemacs--get-label-of node-btn)
                        (file-relative-name (treemacs-dom-node->key node) (treemacs-dom-node->key project-dom-node)))
                      (or node-btn -1)
                      #'treemacs--imenu-goto-node-wrapper
                      (treemacs-dom-node->key node))
                result))))
    (nreverse result)))

(define-inline treemacs--imenu-goto-node-wrapper (_name _pos key)
  "Thin wrapper around `treemacs-goto-node'.
Used by imenu to move to the node with the given KEY."
  (inline-letevals (key)
    (inline-quote
     (treemacs-goto-node ,key))))

(provide 'treemacs-tags)

;;; treemacs-tags.el ends here
