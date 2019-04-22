(require 'treemacs)
(require 'dash)

(defun showcase--get-buffer-groups ()
  "Get the list of buffers, grouped by their major mode."
  (->> (buffer-list)
       (--reject (eq ?\ (aref (buffer-name it) 0)))
       (--group-by (buffer-local-value 'major-mode it))))

; ((org-mode
;   #<buffer Extensions.org>)
;  (emacs-lisp-mode
;   #<buffer init.el>
;   #<buffer treemacs-customization.el>
;   #<buffer *scratch*>
;   #<buffer treemacs-extensions.el>)
;  (spacemacs-buffer-mode
;   #<buffer *spacemacs*>)
;  (messages-buffer-mode
;   #<buffer *Messages*>)
;  (compilation-mode
;   #<buffer *Compile-Log*>)
;  (magit-status-mode
;   #<buffer magit: treemacs>))

(defun showcase-visit-buffer (&rest _)
  "Switch to the buffer saved in node at point."
  (let* ((node (treemacs-current-button))
         (buffer (treemacs-button-get node :buffer)))
    (when (buffer-live-p buffer)
      (select-window (next-window))
      (switch-to-buffer buffer))))

(treemacs-define-leaf-node buffer-leaf
  (treemacs-as-icon "• " 'face 'font-lock-builtin-face)
  :ret-action #'showcase-visit-buffer
  :tab-action #'showcase-visit-buffer
  :mouse1-action (lambda (&rest args) (interactive) (showcase-visit-buffer args)))

(treemacs-define-expandable-node buffer-group
  :icon-open (treemacs-as-icon "- " 'face 'font-lock-string-face)
  :icon-closed (treemacs-as-icon "+ " 'face 'font-lock-string-face)
  :query-function (treemacs-button-get btn :buffers)
  :render-action
  (treemacs-render-node
   :icon treemacs-buffer-leaf-icon
   :label-form (buffer-name item)
   :state treemacs-buffer-leaf-state
   :face 'font-lock-string-face
   :key-form item
   :more-properties (:buffer item)))

(treemacs-define-expandable-node buffers-root
  :icon-open (treemacs-as-icon "E " 'face 'font-lock-string-face)
  :icon-closed (treemacs-as-icon "W " 'face 'font-lock-string-face)
  :query-function (showcase--get-buffer-groups)
  :render-action
  (treemacs-render-node
   :icon treemacs-icon-buffer-group-closed
   :label-form (symbol-name (car item))
   :state treemacs-buffer-group-closed-state
   :face 'font-lock-keyword-face
   :key-form (car item)
   :more-properties (:buffers (cdr item)))
  :root-marker t
  :root-label "Buffers"
  :root-face 'font-lock-type-face
  :root-key-form 'Buffers)

(defun showcase-extension-predicate (project)
  (eq project
      (-> (treemacs-current-workspace)
          (treemacs-workspace->projects)
          (car))))

(treemacs-define-project-extension
 :extension #'treemacs-BUFFERS-ROOT-extension
 :predicate #'showcase-extension-predicate
 :position 'top)

(treemacs-define-top-level-extension
 :extension #'treemacs-BUFFERS-ROOT-extension
 :position 'top)

(treemacs-define-expandable-node buffers-root-top
  ;; :icon-open (treemacs-as-icon "A " 'face 'font-lock-string-face)
  ;; :icon-closed (treemacs-as-icon "B " 'face 'font-lock-string-face)
  :query-function (showcase--get-buffer-groups)
  :render-action
  (treemacs-render-node
   :icon treemacs-icon-buffer-group-closed
   :label-form (symbol-name (car item))
   :state treemacs-buffer-group-closed-state
   :face 'font-lock-keyword-face
   :key-form (car item)
   :more-properties (:buffers (cdr item)))
  :top-level-marker 'variadic
  :root-label "Buffers"
  :root-face 'font-lock-type-face
  :root-key-form 'Buffers)
;; (global-set-key (kbd "C-x M-ö") (lambda () (interactive) (message "%s" (point))))
(global-set-key (kbd "C-x M-ö") (lambda () (interactive) (message "%s %s" (point) (text-properties-at (point) ))))
(defun showcase-display-buffer-list ()
  (interactive)
  (let* ((buffer (get-buffer-create "*Showcase Buffer List*"))
         (window (display-buffer-in-side-window buffer '((side . right)))))
    (select-window window)
    (treemacs-initialize)
    (treemacs-BUFFERS-ROOT-TOP-extension)))
