;;; treemacs.el --- A tree style file viewer package -*- lexical-binding: t -*-

;; Copyright (C) 2022 Alexander Miller

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

;; Code for adding, removing, and displaying "annotations" for treemacs'
;; nodes.  As of now only suffix annotations in extensions are implemented.

;;; Code:

(require 'ht)
(require 'dash)
(require 'treemacs-async)
(require 'treemacs-core-utils)
(require 'treemacs-workspaces)
(require 'treemacs-async)

(eval-when-compile
  (require 'treemacs-macros)
  (require 'inline)
  (require 'cl-lib))

(eval-when-compile
  (cl-declaim (optimize (speed 3) (safety 0))))

(defconst treemacs--annotation-store (make-hash-table :size 200 :test 'equal))

;; TODO(2022/02/23): clear on file delete

(cl-defstruct (treemacs-annotation
               (:conc-name treemacs-annotation->)
               (:constructor treemacs-annotation->create!)
               (:copier nil))
  suffix
  suffix-value
  git-face
  face
  face-value)

(define-inline treemacs-get-annotation (path)
  "Get annotation data for the given PATH.
Will return nil if no annotations exists.

PATH: Node Path"
  (declare (side-effect-free t))
  (inline-letevals (path)
    (inline-quote
     (ht-get treemacs--annotation-store ,path))))

(define-inline treemacs--remove-annotation-if-empty (ann path)
  "Remove annotation ANN for PATH from the store if it is empty."
  (inline-letevals (ann path)
    (inline-quote
     (when (and (null (treemacs-annotation->face ,ann))
                (null (treemacs-annotation->git-face ,ann))
                (null (treemacs-annotation->suffix ,ann)))
       (ht-remove! treemacs--annotation-store ,path)))))

(define-inline treemacs--delete-annotation (path)
  "Complete delete annotation information for PATH."
  (inline-letevals (path)
    (inline-quote
     (ht-remove! treemacs--annotation-store ,path))))

;;; Faces

(define-inline treemacs-set-annotation-face (path face source)
  "Annotate PATH with the given FACE.

Will save the FACE as coming from SOURCE so it can be combined with faces coming
from other sources.

Source must be a *string* so that multiple face annotations on the same node can
be sorted to always be applied in the same order, regardless of when they were
added.

PATH: Node Path
FACE: Face
SOURCE: String"
  (inline-letevals (source path face)
    (inline-quote
     (-if-let* ((ann (treemacs-get-annotation ,path)))
         (let* ((face-list (treemacs-annotation->face ann))
                (old-face (--first (string= ,source (car it)) face-list)))
           (if old-face
               (setcdr old-face ,face)
             (setf (treemacs-annotation->face ann)
                   (--sort (string< (car it) (car other))
                           (cons (cons ,source ,face) face-list))))
           (setf (treemacs-annotation->face-value ann)
                 (append (mapcar #'cdr (treemacs-annotation->face ann))
                         (treemacs-annotation->git-face ann))))
       (ht-set! treemacs--annotation-store ,path
                (treemacs-annotation->create!
                 :face (list (cons ,source ,face))
                 :face-value (list ,face)))))))

(define-inline treemacs-remove-annotation-face (path source)
  "Remove PATH's face annotation for the given SOURCE.

PATH: Node Path
SOURCE: String"
  (inline-letevals (path source)
    (inline-quote
     (-when-let (ann (treemacs-get-annotation ,path))
       (let* ((git-face (treemacs-annotation->git-face ann))
              (old-faces (treemacs-annotation->face ann))
              (new-faces (--reject-first
                          (string= ,source (car it))
                          old-faces)))
         (if new-faces
             (setf
              (treemacs-annotation->face ann)
              new-faces
              (treemacs-annotation->face-value ann)
              (append (mapcar #'cdr new-faces) git-face))
           (setf
            (treemacs-annotation->face ann) 'deleted
            (treemacs-annotation->face-value ann) git-face)))))))

(defun treemacs-clear-annotation-faces (source)
  "Remove all face annotations of the given SOURCE."
  (treemacs--maphash treemacs--annotation-store (path ann)
    (-when-let (face-list (treemacs-annotation->face ann))
      (setf
       (treemacs-annotation->face ann)
       (--reject-first (string= source (car it)) face-list)
       (treemacs-annotation->face-value ann)
       (append
        (mapcar #'cdr (treemacs-annotation->face ann))
        (treemacs-annotation->git-face ann)))
      (treemacs--remove-annotation-if-empty ann path))))

;; Suffixes

(define-inline treemacs-set-annotation-suffix (path suffix source)
  "Annotate PATH with the given SUFFIX.

Will save the SUFFIX as coming from SOURCE so it can be combined with suffixes
coming from other sources.

Source must be a *string* so that multiple suffix annotations on the same node
can be sorted to always appear in the same order, regardless of when they were
added.

Treemacs does not prescribe using a specific face for suffix annotations, users
of this api can propertize suffixes as they see fit.

PATH: Node Path
SUFFIX: String
SOURCE: String"
  (inline-letevals (source path suffix)
    (inline-quote
     (progn
       (put-text-property 0 (length ,suffix) 'treemacs-suffix-annotation t ,suffix)
       (-if-let (ann (treemacs-get-annotation ,path))
           (let* ((suffix-list (treemacs-annotation->suffix ann))
                  (old-suffix (--first (string= ,source (car it)) suffix-list)))
             (if old-suffix
                 (setcdr old-suffix ,suffix)
               (setf (treemacs-annotation->suffix ann)
                     (--sort (string< (car it) (car other))
                             (cons (cons ,source ,suffix) suffix-list))))
             (setf (treemacs-annotation->suffix-value ann)
                   (mapconcat #'identity (mapcar #'cdr (treemacs-annotation->suffix ann)) " ")))
         (ht-set! treemacs--annotation-store ,path
                  (treemacs-annotation->create!
                   :suffix (list (cons ,source ,suffix))
                   :suffix-value ,suffix)))))))

(define-inline treemacs-remove-annotation-suffix (path source)
  "Remove PATH's suffix annotation for the given SOURCE.

PATH: Node Path
SOURCE: String"
  (inline-letevals (path source)
    (inline-quote
     (-when-let (ann (treemacs-get-annotation ,path))
       (let* ((old-suffixes (treemacs-annotation->suffix ann))
              (new-suffixes (--reject-first
                             (string= ,source (car it))
                             old-suffixes)))
         (if new-suffixes
             (setf
              (treemacs-annotation->suffix ann)
              new-suffixes
              (treemacs-annotation->suffix-value ann)
              (mapconcat #'identity (mapcar #'cdr (treemacs-annotation->suffix ann)) " "))
           (setf
            (treemacs-annotation->suffix ann) nil
            (treemacs-annotation->suffix-value ann) nil)))))))

(defun treemacs-clear-annotation-suffixes (source)
  "Remove all suffix annotations of the given SOURCE."
  (treemacs--maphash treemacs--annotation-store (path ann)
    (-when-let (suffix-list (treemacs-annotation->suffix ann))
      (setf
       (treemacs-annotation->suffix ann)
       (--reject-first (string= source (car it)) suffix-list)
       (treemacs-annotation->suffix-value ann)
       (mapconcat #'identity (mapcar #'cdr (treemacs-annotation->suffix ann)) " "))
      (treemacs--remove-annotation-if-empty ann path))))

(defun treemacs--apply-annotations-deferred (btn path buffer git-future)
  "Deferred application for annotations for BTN and PATH.

Runs on a timer after BTN was expanded and will apply annotations for all of
BTN's *immediate* children.

Change will happen in BUFFER, given that it is alive.

GIT-FUTURE is only awaited when `deferred' git-mode is used.

BTN: Button
PATH: Node Path
BUFFER: Buffer
GIT-FUTURE: Pfuture"
  (when (eq 'deferred treemacs--git-mode)
    (ht-set! treemacs--git-cache path
             (treemacs--get-or-parse-git-result git-future)))
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (save-excursion
        (treemacs-with-writable-buffer
         (let* ((depth (1+ (treemacs-button-get btn :depth)))
                (git-info (or (ht-get treemacs--git-cache (treemacs-button-get btn :key))
                              treemacs--empty-table)))
           ;; the depth check ensures that we only iterate over the nodes that
           ;; are below parent-btn and stop when we've moved on to nodes that
           ;; are above or belong to the next project
           (while (and (setq btn (next-button btn))
                       (>= (treemacs-button-get btn :depth) depth))
             (when (= depth (treemacs-button-get btn :depth))
               (treemacs--do-apply-annotation
                btn
                (ht-get git-info (treemacs-button-get btn :key)))))))))))

(define-inline treemacs--do-apply-annotation (btn git-face)
  "Apply a single BTN's annotations.
GIT-FACE is taken from the latest git cache, or nil if it's not known."
  (inline-letevals (btn git-face)
    (inline-quote
     (let* ((path (treemacs-button-get ,btn :key))
            (ann (treemacs-get-annotation path))
            (btn-start (treemacs-button-start ,btn))
            (btn-end (treemacs-button-end ,btn)))
       (if (null ann)

           ;; No annotation - just put git face
           (when ,git-face
             (put-text-property btn-start btn-end 'face ,git-face)
             ;; git face must be known for initial render
             (ht-set!
              treemacs--annotation-store
              path
              (treemacs-annotation->create!
               :git-face ,git-face
               :face-value ,git-face)))

         (let ((face-value (treemacs-annotation->face-value ann))
               (suffix-value (treemacs-annotation->suffix-value ann))
               (faces (treemacs-annotation->face ann))
               (old-git-face (treemacs-annotation->git-face ann)))

           ;; Faces
           (if (eq 'deleted faces)
               ;; face annotation was deleted - only the git face remains
               ;; as the annotation value
               (progn
                 (setf
                  (treemacs-annotation->face ann) nil
                  (treemacs-annotation->face-value ann) ,git-face
                  (treemacs-annotation->git-face ann) ,git-face)
                 (unless ,git-face
                   (treemacs--remove-annotation-if-empty ann path))
                 (put-text-property
                  btn-start btn-end 'face
                  ,git-face))
             ;; annotations are present, value needs updating if the git face
             ;; has changed
             (let ((new-face-value
                    (or
                     (cond
                      ((and ,git-face (not (equal ,git-face old-git-face)))
                       (append (mapcar #'cdr faces)
                               (list ,git-face)))
                      ((and old-git-face (null ,git-face))
                       (mapcar #'cdr faces))
                      (t face-value))
                     (treemacs-button-get ,btn :default-face))))
               (setf (treemacs-annotation->face-value ann)
                     new-face-value
                     (treemacs-annotation->git-face ann)
                     ,git-face)
               (put-text-property
                btn-start btn-end 'face
                new-face-value)))

           ;; Suffix
           (goto-char ,btn)
           (goto-char (or (next-single-property-change
                           ,btn
                           'treemacs-suffix-annotation
                           (current-buffer)
                           (line-end-position))
                          btn-end))
           (delete-region (point) (line-end-position))
           (when suffix-value (insert suffix-value))))))))

(defun treemacs-apply-single-annotation (path)
  "Apply annotations for a single node at given PATH in all treemacs buffers."
  (treemacs-run-in-all-derived-buffers
   (-when-let (btn (treemacs-find-node path))
     (treemacs-with-writable-buffer
      (save-excursion
        (treemacs--do-apply-annotation
         btn
         (-when-let (git-cache
                     (->> path
                          (treemacs--parent-dir)
                          (ht-get treemacs--git-cache)))
           (ht-get git-cache path))))))))

(defun treemacs-apply-annotations-in-buffer (buffer)
  "Apply annotations for all nodes in the given BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (treemacs-with-writable-buffer
       (save-excursion
         (goto-char (point-min))
         (let* ((btn (point)))
           (while (setf btn (next-button btn))
             (-let [path (treemacs-button-get btn :key)]
               (treemacs--do-apply-annotation
                btn
                (-when-let (git-cache
                            (->> path
                                 (treemacs--parent-dir)
                                 (ht-get treemacs--git-cache)))
                  (ht-get git-cache path)))))))))))

(defun treemacs-apply-annotations (path)
  "Apply annotations for all nodes under the given PATH.

PATH: Node Path"
  (treemacs-run-in-all-derived-buffers
   (treemacs-with-writable-buffer
    (save-excursion
      (goto-char (treemacs-find-node path))
      (let ((git-info (ht-get treemacs--git-cache path treemacs--empty-table))
            (btn (point)))
        (treemacs--do-apply-annotation
         btn
         (ht-get git-info (treemacs-button-get btn :key)))
        (while (and (setf btn (next-button btn))
                    (/= 0 (treemacs-button-get btn :depth)))
          (-let [parent-path (treemacs-button-get
                              (treemacs-button-get btn :parent)
                              :key)]
            (treemacs--do-apply-annotation
             btn
             (ht-get
              (ht-get treemacs--git-cache parent-path git-info)
              (treemacs-button-get btn :key))))))))))

(provide 'treemacs-annotations)

;;; treemacs-annotations.el ends here
