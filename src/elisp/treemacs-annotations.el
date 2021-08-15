;;; treemacs.el --- A tree style file viewer package -*- lexical-binding: t -*-

;; Copyright (C) 2021 Alexander Miller

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
(require 'treemacs-core-utils)
(require 'treemacs-workspaces)

(eval-when-compile
  (require 'treemacs-macros)
  (require 'inline)
  (require 'cl-lib))

(eval-when-compile
  (cl-declaim (optimize (speed 3) (safety 0))))

(defconst treemacs--annotation-store (make-hash-table :size 200 :test 'equal))
(defconst treemacs--annotation-cache (make-hash-table :size 200 :test 'equal))

(cl-defstruct (treemacs-annotation
               (:conc-name treemacs-annotation->)
               (:constructor treemacs-annotation->create!)
               (:copier nil))
  suffix)

(define-inline treemacs-get-annotation (path)
  "Get annotation data for the given PATH.
Will return nil if no annotations exist.

PATH: Node Path"
  (declare (side-effect-free t))
  (inline-letevals (path)
    (inline-quote
     (ht-get treemacs--annotation-store ,path))))

(define-inline treemacs--create-new-annotation (path)
  "Create a new empty annotation for PATH and return it."
  (declare (side-effect-free t))
  (inline-letevals (path)
    (inline-quote
     (let ((ann (treemacs-annotation->create!)))
       (ht-set! treemacs--annotation-store ,path ann)
       ann))))

(define-inline treemacs--remove-annotation-if-empty (ann path)
  "Remove annotation ANN for PATH from the store if it is empty."
  (inline-letevals (ann path)
    (inline-quote
     (when (null (treemacs-annotation->suffix ,ann))
       (ht-remove! treemacs--annotation-store ,path)))))

(define-inline treemacs-set-annotation-suffix (path suffix source)
  "Add the given SUFFIX to PATH's annotation.
Will save the SUFFIX as coming from SOURCE so it can be combined with
other suffixes.

Source must be a *string* so that multiple suffix annotations on the same node
can be sorted to always appear in the same order, regardless of when they were
added.

Treemacs does not prescribe using a specific face for suffix annotations, users
of this api can propertiue suffixes as they see fit.

PATH: Node Path
SUFFIX: String
SOURCE: String"
  (inline-letevals (source path suffix)
    (inline-quote
     (let* ((ann (or (treemacs-get-annotation ,path)
                     (treemacs--create-new-annotation ,path)))
            (suffix-list (treemacs-annotation->suffix ann))
            (old-suffix (--first (string= ,source (car it)) suffix-list)))
       (put-text-property 0 (length ,suffix) 'treemacs-suffix-annotation t ,suffix)
       (if old-suffix
           (setcdr old-suffix ,suffix)
         (setf (treemacs-annotation->suffix ann)
               (--sort (string< (car it) (car other))
                       (cons (cons ,source ,suffix) suffix-list))))))))

(define-inline treemacs-remove-annotation-suffix (path source)
  "Remove PATH's suffix annotation for the given SOURCE.
PATH: Node Path
SOURCE: String"
  (inline-letevals (path source)
    (inline-quote
     (-when-let (ann (treemacs-get-annotation ,path))
       (let ((suffix-list (treemacs-annotation->suffix ann)))
         (setf (treemacs-annotation->suffix ann)
               (--reject-first (string= ,source (car it)) suffix-list)))
       (treemacs--remove-annotation-if-empty ann ,path)))))

(defun treemacs-clear-annotation-suffixes (source)
  "Remove all suffix annotations of the given SOURCE."
    (treemacs--maphash treemacs--annotation-store (path ann)
      (let ((suffix-list (treemacs-annotation->suffix ann)))
        (setf (treemacs-annotation->suffix ann)
              (--reject-first (string= source (car it)) suffix-list))
        (treemacs--remove-annotation-if-empty ann path))))

;; TODO(2021/08/18): change annotation application such that only a single
;; timer is needed even if multiple levels have been expanded
(defun treemacs--apply-annotations-deferred (btn)
  "Deferred application for annotations for BTN.

Runs on a timer after BTN was expanded and will apply annotations for all of
BTN's *immediate* children."
  (-let [buffer (marker-buffer btn)alue]
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (save-excursion
          (treemacs-with-writable-buffer
           (let* ((btn (treemacs-find-node (treemacs-button-get btn :path)))
                  (depth (1+ (treemacs-button-get btn :depth))))
             ;; the depth check ensures that we only iterate over the nodes that
             ;; are below parent-btn and stop when we've moved on to nodes that
             ;; are above or belong to the next project
             (while (and (setq btn (next-button btn))
                         (>= (treemacs-button-get btn :depth) depth))
               (when (= depth (treemacs-button-get btn :depth))
                 (treemacs--do-apply-annotation btn))))))))))

(define-inline treemacs--do-apply-annotation (btn)
  "Apply a single BTN's annotations."
  (inline-letevals (btn)
    (inline-quote
     (-when-let (ann (treemacs-get-annotation (treemacs-button-get ,btn :path)))
       (let ((suffix (treemacs--get-annotation-suffix-string ann)))
         (goto-char ,btn)
         (goto-char (or (next-single-property-change
                         ,btn
                         'treemacs-suffix-annotation
                         (current-buffer)
                         (point-at-eol))
                        (treemacs-button-end ,btn)))
         (delete-region (point) (point-at-eol))
         (when suffix (insert suffix)))))))

(define-inline treemacs--get-annotation-suffix-string (ann)
  "Get the complete, concatenated suffix string from ANN."
  (declare (side-effect-free t))
  (inline-letevals (ann)
    (inline-quote
     (-when-let (suffix-list (treemacs-annotation->suffix ,ann))
       (mapconcat #'cdr suffix-list "")))))

(defun treemacs-apply-annotations-in-buffer (buffer)
  "Apply annotations for all nodes in the given BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (treemacs-with-writable-buffer
       (save-excursion
         (goto-char (point-min))
         (let* ((btn (point)))
           (while (setf btn (next-button btn))
             (treemacs--do-apply-annotation btn))))))))

(provide 'treemacs-annotations)

;;; treemacs-annotations.el ends here
