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
;;; Handling of visuals in general and icons in particular.

;;; Code:

(require 'image)
(require 'pulse)
(require 'hl-line)
(require 'treemacs-core-utils)
(require 'treemacs-scope)
(require 'treemacs-themes)
(require 'treemacs-icons)
(require 'treemacs-customization)
(require 'treemacs-fringe-indicator)
(eval-and-compile
  (require 'inline)
  (require 'treemacs-macros))

(treemacs-import-functions-from "treemacs-icons"
  treemacs-get-icon-value)

(defvar-local treemacs--last-highlight nil
  "The last button treemacs has highlighted.")

(defvar-local treemacs--indentation-string-cache-key nil
  "Cache key for `treemacs--indentation-string-cache.")
(defvar-local treemacs--indentation-string-cache (vector)
  "Cached propertized indentation.")

(define-inline treemacs--forget-last-highlight ()
  "Set `treemacs--last-highlight' to nil."
  (inline-quote (setq treemacs--last-highlight nil)))

(defun treemacs--setup-icon-highlight ()
  "Make sure treemacs icons background aligns with hi-line's."
  (advice-add #'hl-line-highlight :after #'treemacs--update-icon-selection)
  (advice-add #'enable-theme      :after #'treemacs--setup-icon-background-colors)
  (advice-add #'disable-theme     :after #'treemacs--setup-icon-background-colors))

(defun treemacs--tear-down-icon-highlight ()
  "Tear down highlighting advice when no treemacs buffer exists anymore."
  (treemacs--forget-last-highlight)
  (unless treemacs--buffer-storage
    (advice-remove #'hl-line-highlight #'treemacs--update-icon-selection)
    (advice-remove #'enable-theme      #'treemacs--setup-icon-background-colors)
    (advice-remove #'disable-theme     #'treemacs--setup-icon-background-colors)))

(defun treemacs--update-icon-selection ()
  "Highlight current icon, unhighlight `treemacs--last-highlight'."
  (when (eq major-mode 'treemacs-mode)
    (condition-case e
        (progn
          (when treemacs-fringe-indicator-mode
            (treemacs--move-fringe-indicator-to-point))
          (-when-let (btn (treemacs-current-button))
            (let* ((pos (max (point-at-bol) (- (treemacs-button-start btn) 2)))
                   (img-selected (get-text-property pos 'img-selected)))
              (treemacs-with-writable-buffer
               (when (and treemacs--last-highlight
                          (> (point-max) treemacs--last-highlight))
                 (let* ((last-pos (- (treemacs-button-start treemacs--last-highlight) 2))
                        (img-unselected (get-text-property last-pos 'img-unselected)))
                   (put-text-property last-pos (1+ last-pos) 'display img-unselected)))
               (when img-selected
                 (put-text-property pos (1+ pos) 'display img-selected)
                 (setq treemacs--last-highlight btn))))))
      (error
       (treemacs-log "Error on highlight, this shouldn't happen: %s" e)))))

(defun treemacs--pulse-png-advice (&rest _)
  "Make sure icons' background are pusled alongside the entire line."
  (when (eq 'treemacs-mode major-mode)
    (treemacs-with-writable-buffer
     (-when-let (btn (treemacs-current-button))
       (let* ((start (max (point-at-bol) (- (treemacs-button-start btn) 2)))
              (end (1+ start))
              (img (get-text-property start 'display))
              (cp (copy-sequence img)))
         ;; Icons may not always be images, as extensions may use text and e.g.
         ;; all-the-icons font icons as the icon.
         (when (eq (car cp) 'image)
           (treemacs--set-img-property cp :background
                                       (face-attribute
                                        (overlay-get pulse-momentary-overlay 'face)
                                        :background nil t))
           (put-text-property start end 'display cp)))))))

(defun treemacs--do-pulse (face)
  "Visually pulse current line using FACE."
  (pulse-momentary-highlight-one-line (point) face)
  (advice-add 'pulse-momentary-unhighlight :after #'hl-line-highlight)
  (advice-add 'pulse-lighten-highlight :after #'treemacs--pulse-png-advice))

(defsubst treemacs-pulse-on-success (&rest log-args)
  "Pulse current line with `treemacs-on-success-pulse-face'.
Optionally issue a log statment with LOG-ARGS."
  (declare (indent 1))
  (when log-args
    (treemacs-log (apply #'format log-args)))
  (when treemacs-pulse-on-success
    (treemacs--do-pulse 'treemacs-on-success-pulse-face)))

(defsubst treemacs-pulse-on-failure (&rest log-args)
  "Pulse current line with `treemacs-on-failure-pulse-face'.
Optionally issue a log statment with LOG-ARGS."
  (declare (indent 1))
  (when log-args
    (treemacs-log (apply #'format log-args)))
  (when treemacs-pulse-on-failure
    (treemacs--do-pulse 'treemacs-on-failure-pulse-face)))

(defun treemacs--build-indentation-cache (depth)
  "Rebuild indentation string cache up to DEPTH levels deep."
  (setq treemacs--indentation-string-cache (make-vector (1+ depth) nil)
        treemacs--indentation-string-cache-key (cons treemacs-indentation treemacs-indentation-string))
  (dotimes (i (1+ depth))
    (aset treemacs--indentation-string-cache i
          (cond ((integerp treemacs-indentation)
                 (s-repeat (* i treemacs-indentation) treemacs-indentation-string))
                ((not window-system)
                 (s-repeat (* i 2) treemacs-indentation-string))
                (t (propertize " "
                               'display
                               `(space . (:width (,(* (car treemacs-indentation)
                                                      i))))))))))

(define-inline treemacs--get-indentation (depth)
  "Gets an indentation string DEPTH levels deep."
  (inline-letevals (depth)
    (inline-quote
     (progn
       (when (or (>= ,depth (length treemacs--indentation-string-cache))
                 (not (eq (car treemacs--indentation-string-cache-key) treemacs-indentation))
                 ;; Eq is faster than string comparison, and accidentally
                 ;; rebuilding the cache in some corner case is not disastrous.
                 (not (eq (cdr treemacs--indentation-string-cache-key) treemacs-indentation-string)))
         (treemacs--build-indentation-cache ,depth))
       (aref treemacs--indentation-string-cache ,depth)))))

(provide 'treemacs-visuals)

;;; treemacs-visuals.el ends here
