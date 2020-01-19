;;; treemacs-diagnostics.el --- A tree style file viewer package -*- lexical-binding: t -*-

;; Copyright (C) 2020 Alexander Miller

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
;;; WIP implementation of diagnostics display.

;;; Code:

(require 'ht)
(require 'dash)
(require 'overlay)
(require 'treemacs-dom)
(require 'treemacs-macros)
(require 'treemacs-scope)
(require 'treemacs-workspaces)
(require 'treemacs-core-utils)

(defconst treemacs--diag-store (make-hash-table :size 50 :test 'equal))

(defvar treemacs--diagnostic-timer nil
  "Debounce guard for the application of diagnostics.")

(defconst treemacs--apply-diagnostics-delay 3
  "Debounce delay for the application of diagnostics.")

(defface treemacs-diagnostic-error
  '((t :underline "red"))
  "TODO"
  :group 'treemacs-faces)

(defface treemacs-diagnostic-warning
  '((t :underline "yellow"))
  "TODO"
  :group 'treemacs-faces)

(defun treemacs--reset-and-save-diagnosics (path diagnostics)
  "TODO PATH DIAGNOSTICS."
  (-let [ht (ht-get treemacs--diag-store path)]
    (if ht
        (ht-clear! ht)
      (setf ht (make-hash-table :size 100 :test 'equal))
      (ht-set! treemacs--diag-store path ht))
    (while diagnostics
      (ht-set! ht (pop diagnostics) (pop diagnostics)))))

(defun treemacs-apply-diagnostics (path &rest diagnostics)
  "Display DIAGNOSTICS for given PATH."
  (declare (indent 1))
  (treemacs-debounce treemacs--diagnostic-timer treemacs--apply-diagnostics-delay
    (treemacs--reset-and-save-diagnosics path diagnostics)
    (treemacs-run-in-every-buffer
     (save-excursion
       (-when-let (dom-node (treemacs-find-in-dom path))
         (let* ((start (treemacs-dom-node->position dom-node))
                (end (treemacs--next-neighbour-of start)))
           (-each (overlays-in start end) #'delete-overlay))
         (while diagnostics
           (let ((path  (pop diagnostics))
                 (state (pop diagnostics)))
             (when  (treemacs-is-path-visible? path)
               (-let [btn (treemacs-find-file-node path)]
                 (-doto (make-overlay (treemacs-button-start btn) (treemacs-button-end btn))
                   (overlay-put 'face state))))))))
     (hl-line-highlight))))

(provide 'treemacs-diagnostics)

;;; treemacs-diagnostics.el ends here
