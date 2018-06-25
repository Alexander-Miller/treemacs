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
;;; Tags display functionality.
;;; Need to be very careful here - many of the functions in this module need to be run inside the treemacs buffer, while
;;; the `treemacs--execute-button-action' macro that runs them will switch windows before doing so. Heavy use of
;;; `treemacs-safe-button-get' or `treemacs-with-button-buffer' is necessary.

;;; Code:

(require 'xref)
(require 'imenu)
(require 'dash)
(require 'f)
(require 'treemacs-impl)
(require 'treemacs-branch-creation)
(require 'treemacs-customization)
(require 'treemacs-faces)
(require 'treemacs-visuals)
(require 'treemacs-structure)
(require 'treemacs-icons)
(eval-and-compile
  (require 'cl-lib)
  (require 'treemacs-macros))

(treemacs-import-functions-from "treemacs"
  treemacs-select-window)

(defsubst treemacs--tags-path-of (btn)
  "Return the path of tag labels leading to BTN.

The car of the returned list is the label of BTN while its cdr is the top down
path starting at the absolute path of the file the tags belong to.

These paths are used to give tag nodes a unique key in the shadow tree."
  (-if-let (path (button-get btn :path))
      path
    (let ((lbl (treemacs--get-label-of btn))
          (btn (button-get btn :parent))
          (ret))
      (while (and btn (null (button-get btn :path)))
        (push (treemacs--get-label-of btn) ret)
        (setq btn (button-get btn :parent)))
      (push (button-get btn :path) ret)
      (cons lbl ret))))

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
  (-pcase index-mode
    [(or 'markdown-mode 'org-mode 'python-mode)
     index]
    [(guard (treemacs--provided-mode-derived-p index-mode 'conf-mode))
     (treemacs--partition-imenu-index index "Sections")]
    [_
     (treemacs--partition-imenu-index index "Functions")]))

(defun treemacs--get-imenu-index (file)
  "Fetch imenu index of FILE."
  (let ((buff)
        (result)
        (mode)
        (existing-buffer (get-file-buffer file)))
    (if existing-buffer
        (setq buff existing-buffer)
      (cl-letf (((symbol-function 'run-mode-hooks) (symbol-function 'ignore)))
        (setq buff (find-file-noselect file))))
    (when (buffer-live-p buff)
      (with-current-buffer buff
        (when (eq major-mode 'emacs-lisp-mode)
          (setq-local imenu-generic-expression treemacs-elisp-imenu-expression))
        (setq result (imenu--make-index-alist t)
              mode major-mode))
      (unless existing-buffer (kill-buffer buff))
      (when result
        (when (string= "*Rescan*" (car (car result)))
          (setq result (cdr result)))
        (unless (equal result '(nil))
          (treemacs--post-process-index result mode))))))

(defsubst treemacs--insert-tag-leaf (item prefix parent depth)
  "Return the text to insert for a tag leaf ITEM.
Use PREFIX for indentation.
Set PARENT and DEPTH button properties.
ITEM: String . Marker
PREFIX: String
PARENT: Button
DEPTH: Int"
  (list
   prefix
   (propertize (car item)
               'button '(t)
               'category 'default-button
               'face 'treemacs-tags-face
               'help-echo nil
               :state 'tag-node
               :parent parent
               :depth depth
               :marker (cdr item))))

(defsubst treemacs--insert-tag-node (node prefix parent depth)
  "Return the text to insert for a tag NODE.
Use PREFIX for indentation.
Set PARENT and DEPTH button properties.

NODE: String & List of (String . Marker)
PREFIX: String
PARENT: Button
DEPTH: Int"
  (list
   prefix
   (propertize (car node)
               'button '(t)
               'category 'default-button
               'face 'treemacs-tags-face
               'help-echo nil
               :state 'tag-node-closed
               :parent parent
               :depth depth
               :index (cdr node))))

(defun treemacs--expand-file-node (btn &optional recursive)
  "Open tag items for file BTN.
Recursively open all tag below BTN when RECURSIVE is non-nil."
  (-let [path (button-get btn :path)]
    (-if-let- [index (treemacs--get-imenu-index path)]
        (treemacs--button-open
         :button btn
         :immediate-insert t
         :new-state 'file-node-open
         :open-action (treemacs--create-buttons
                       :nodes index
                       :extra-vars
                       ((node-prefix (concat prefix treemacs-icon-tag-node-closed))
                        (leaf-prefix (concat prefix treemacs-icon-tag-leaf)))
                       :depth (1+ (button-get btn :depth))
                       :node-name item
                       :node-action (if (imenu--subalist-p item)
                                        (treemacs--insert-tag-node item node-prefix btn depth)
                                      (treemacs--insert-tag-leaf item leaf-prefix btn depth)))
         :post-open-action (progn
                             (treemacs-on-expand path btn (treemacs-parent-of btn))
                             (treemacs--reopen-tags-under btn)
                             (end-of-line)
                             (when recursive
                               (--each (treemacs--get-children-of btn)
                                 (when (eq 'tag-node-closed (button-get it :state))
                                   (goto-char (button-start it))
                                   (treemacs--expand-tag-node it t))))))
      (treemacs-pulse-on-failure "No tags found for %s" (propertize path 'face 'font-lock-string-face)))))

(defun treemacs--collapse-file-node (btn &optional recursive)
  "Close node given by BTN.
Remove all open tag entries under BTN when RECURSIVE."
  (treemacs--button-close
   :button btn
   :new-state 'file-node-closed
   :post-close-action (treemacs-on-collapse (button-get btn :path) recursive)))

(defun treemacs--visit-or-expand/collapse-tag-node (btn arg find-window)
  "Visit tag section BTN if possible, expand or collapse it otherwise.
Pass prefix ARG on to either visit or toggle action.

FIND-WINDOW is a special provision depending on this function's invocation
context and decides whether to find the window to display in (if the tag is
visited instead of the node being expanded).

On the one hand it can be called based on `treemacs-RET-actions-config' (or
TAB). The functions in these configs  are expected to find the windows they need
to display in themselves, so FIND-WINDOW must be t. On the other hand this
function is also called from the top level vist-node functions like
`treemacs-visit-node-vertical-split' which delegates to the
`treemacs--execute-button-action' macro which includes the determination of
the display window."
  (-let*- [(path (treemacs--nearest-path btn))
           (extension (f-ext path))]
    (-pcase extension
      ["py"
       (-let*- [(first-child (car (button-get btn :index)))
                (name (car first-child))
                (marker (cdr first-child))]
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
                 (when (eq 'tag-node-closed (button-get btn :state))
                   (setq need-to-close-section t)
                   (treemacs--expand-tag-node btn)))
               (treemacs--call-imenu-and-goto-tag
                (treemacs-with-button-buffer btn (treemacs--tags-path-of (next-button (button-end btn)))))
               (when need-to-close-section
                 (treemacs-with-button-buffer btn
                   (treemacs--collapse-tag-node btn))))
             (when arg (treemacs-select-window)))))]
      ["org"
       (-unless-let [pos (button-get btn 'org-imenu-marker)]
           (treemacs--expand-tag-node btn arg)
         ;; select the window as visit-no-split would
         (when find-window
           (--if-let (-some-> path (get-file-buffer) (get-buffer-window))
               (select-window it)
             (other-window 1)))
         (find-file path)
         (if (marker-position pos)
             (goto-char pos)
           (treemacs--call-imenu-and-goto-tag (treemacs-with-button-buffer btn (treemacs--tags-path-of btn)) t)))]
      [_ (-pcase (button-get btn :state)
           ['tag-node-open   (treemacs--collapse-tag-node btn arg)]
           ['tag-node-closed (treemacs--expand-tag-node btn arg)])])))

(defun treemacs--expand-tag-node (btn &optional recursive)
  "Open tags node items for BTN.
Open all tag section under BTN when call is RECURSIVE."
  (-let [index (button-get btn :index)]
    (treemacs--button-open
     :button btn
     :immediate-insert t
     :new-state 'tag-node-open
     :new-icon treemacs-icon-tag-node-open
     :open-action (treemacs--create-buttons
                   :nodes index
                   :depth (1+ (button-get btn :depth))
                   :node-name item
                   :extra-vars ((leaf-prefix (concat prefix treemacs-icon-tag-leaf))
                                (node-prefix (concat prefix treemacs-icon-tag-node-closed)))
                   :node-action (if (imenu--subalist-p item)
                                    (treemacs--insert-tag-node item node-prefix btn depth)
                                  (treemacs--insert-tag-leaf item leaf-prefix btn depth)))
     :post-open-action (progn
                         (treemacs-on-expand
                          (treemacs--tags-path-of btn) btn
                          (-let [parent (button-get btn :parent)]
                            (-pcase (button-get parent :state)
                              ['file-node-open (button-get parent :path)]
                              ['tag-node-open  (treemacs--tags-path-of parent)]
                              [other (error "Impossible state of parent: %s" other)])))
                         (if recursive
                             (--each (treemacs--get-children-of btn)
                               (when (eq 'tag-node-closed (button-get it :state))
                                 (goto-char (button-start it))
                                 (treemacs--expand-tag-node it t)))
                           (treemacs--reopen-tags-under btn))))))

(defun treemacs--collapse-tag-node-recursive (btn)
  "Recursively close tag section BTN.
Workaround for tag section having no easy way to purge all open tags below a
button from cache. Easiest way is to just do it manually here."
  (--each (treemacs--get-children-of btn)
    (when (eq 'tag-node-open (button-get it :state))
      (treemacs--collapse-tag-node-recursive it)
      (goto-char (button-start it))
      (treemacs--collapse-tag-node it)))
  (goto-char (button-start btn))
  (treemacs--collapse-tag-node btn))

(defun treemacs--collapse-tag-node (btn &optional recursive)
  "Close tags node at BTN.
Remove all open tag entries under BTN when RECURSIVE."
  (if recursive
      (treemacs--collapse-tag-node-recursive btn)
    (treemacs--button-close
     :button btn
     :new-state 'tag-node-closed
     :new-icon treemacs-icon-tag-node-closed
     :post-close-action
     (treemacs-on-collapse (treemacs--tags-path-of btn)))))


(defsubst treemacs--extract-position (item)
  "Extract a tag's buffer and position stored in ITEM.
The position can be stored in the following ways:

* ITEM is a marker pointing to a tag provided by imenu.
* ITEM is an overlay pointing to a tag provided by imenu with semantic mode.
* ITEM is a raw number pointing to a buffer position.
* ITEM is a cons: special case for imenu elements of an `org-mode' buffer.
  ITEM is an imenu subtree and the position is stored as a marker in the first
  element's 'org-imenu-marker text property.

Either way the return value is a const consisting of the buffer and the position
of the tag. They might also be nil if the pointed-to buffer does not exist."
  (-pcase (type-of item)
    ['marker
     (cons (marker-buffer item) (marker-position item))]
    ['overlay
     (cons (overlay-buffer item) (overlay-start item))]
    ['integer
     (cons nil item)]
    ['cons
     (-when-let- [org-marker (get-text-property 0 'org-imenu-marker (car item))]
       (cons (marker-buffer org-marker) (marker-position org-marker)))]))

(defun treemacs--call-imenu-and-goto-tag (tag-path &optional org?)
  "Call the imenu index of the tag at TAG-PATH and go to its position.
ORG? should be t when this function is called for an org buffer and index since
org requires a slightly different position extraction because the position of a
headline with subelements is saved in an 'org-imenu-marker' text property."
  (let ((file (cadr tag-path))
        (tag (car tag-path))
        (path (cddr tag-path)))
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
                                  (if org? item (cdr item))))]
              ;; some imenu implementations, like markdown, will only provide
              ;; a raw buffer position (an int) to move to
              (switch-to-buffer (or buf (get-file-buffer file)))
              (goto-char pos))))
      (error
       (treemacs-log "Something went wrong when finding tag '%s': %s"
              (propertize tag 'face 'treemacs-tags-face)
              e)))))

(defun treemacs--goto-tag (btn)
  "Go to the tag at BTN."
  ;; The only code currently calling this is run through `treemacs--execute-button-action' which always
  ;; switches windows before running it, so we need to be really careful here when querying any button
  ;; properties.
  (-let [(tag-buf . tag-pos)
         (treemacs-with-button-buffer btn
           (-> btn (button-get :marker) (treemacs--extract-position)))]
    (if tag-buf
        (progn
          (switch-to-buffer tag-buf nil t)
          (goto-char tag-pos))
      (-pcase treemacs-goto-tag-strategy
        ['refetch-index
         (treemacs--call-imenu-and-goto-tag
          (with-current-buffer (marker-buffer btn)
            (treemacs--tags-path-of btn)))]
        ['call-xref
         ;; for emacs24
         (with-no-warnings
           (xref-find-definitions
            (treemacs-with-button-buffer btn
              (treemacs--get-label-of btn))))]
        ['issue-warning
         (treemacs-pulse-on-failure
          "Tag '%s' is located in a buffer that does not exist."
          (propertize (treemacs-with-button-buffer btn (treemacs--get-label-of btn)) 'face 'treemacs-tags-face))]
        [_ (error "[Treemacs] '%s' is an invalid value for treemacs-goto-tag-strategy" treemacs-goto-tag-strategy)]))))

(cl-defun treemacs--goto-tag-button-at (tag-path)
  "Goto tag given by TAG-PATH.
Will return the found tag node, or nil if no such node exists (anymore). In this
case point will be left at the next highest node available."
  (-let [(tag file . path) tag-path]
    (-when-let (file-node (treemacs-goto-button file))
      (when (eq 'file-node-closed (button-get file-node :state))
        (goto-char (button-start file-node))
        (treemacs--expand-file-node file-node))
      (dolist (tag-path-item path)
        (-if-let (tag-path-node (--first
                                 (string= (treemacs--get-label-of it) tag-path-item)
                                 (treemacs--get-children-of file-node)))
            (progn
              (setq file-node tag-path-node)
              (when (eq 'tag-node-closed (button-get file-node :state))
                (goto-char (button-start file-node))
                (treemacs--expand-tag-node file-node)))
          (goto-char file-node)
          (cl-return-from treemacs--goto-tag-button-at nil)))
      (-if-let- [pos (--first (string= (treemacs--get-label-of it) tag)
                              (treemacs--get-children-of file-node))]
          (progn
            (goto-char pos)
            (treemacs--button-at pos))
        (goto-char file-node)
        (cl-return-from treemacs--goto-tag-button-at nil)))))

(defun treemacs--reopen-tags-under (btn)
  "Reopen previously openeded tags under BTN."
  (save-excursion
    (-let*- [(tag-path (treemacs--tags-path-of btn))
             (sh-node (treemacs-get-from-shadow-index tag-path))
             (children (->> sh-node
                            (treemacs-shadow-node->children)
                            (-reject #'treemacs-shadow-node->closed)))
             (btns-under-btn (treemacs--get-children-of btn))]
      (dolist (sh-child children)
        (-if-let- [child-btn (--first (equal (treemacs-shadow-node->key sh-child)
                                             (treemacs--tags-path-of it))
                                      btns-under-btn)]
            (when (eq 'tag-node-closed (button-get child-btn :state))
              (goto-char (button-start child-btn))
              (treemacs--expand-tag-node child-btn))
          (setf (treemacs-shadow-node->children sh-node)
                (delete sh-child (treemacs-shadow-node->children sh-node)))
          (treemacs--do-for-all-child-nodes sh-child
            #'treemacs-shadow-node->remove-from-index))))))

(provide 'treemacs-tags)

;;; treemacs-tags.el ends here
