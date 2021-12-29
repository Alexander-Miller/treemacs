;;; treemacs-diagnostics.el --- A tree style file viewer package -*- lexical-binding: t -*-

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

;; WIP implementation of diagnostics display.

;;; Code:

(require 'ht)
(require 'thunk)
(require 'dash)
(require 'overlay)
(require 'treemacs-dom)
(require 'treemacs-scope)
(require 'treemacs-workspaces)
(require 'treemacs-core-utils)

(eval-when-compile
  (require 'treemacs-macros))

(defconst treemacs--diag-store (make-hash-table :size 50 :test 'equal))

(defvar treemacs--diagnostic-timer nil
  "Debounce guard for the application of diagnostics.")

(defconst treemacs--apply-diagnostics-delay 3
  "Debounce delay for the application of diagnostics.")

(defface treemacs-diagnostic-error-face
  '((t :underline "red"))
  "TODO."
  :group 'treemacs-faces)

(defface treemacs-diagnostic-warning-face
  '((t :underline "yellow"))
  "TODO."
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

(defun treemacs-apply-diagnostics (provider)
  "Display diagnostics based on the given arguments PROVIDER.
PROVIDER should be a `thunk' (see thunk.el) that, when evaluated, returns a flat
list of consecutive path and face items.

File paths should use treemacs' canonical format - they should be absolute,
expanded and *not* have a trailing slash.

The diagnostic faces could be any face symbols or raw face literals.  Treemacs
features several pre-made faces named `treemacs-diagnostic-*-face'.

This method is debounced, it will never run more often than once every 3
seconds.  In addition the use of a lazy thunk ensures that potentially expensive
transformations happen only once and only when required.  Performance is thus
not expected to be a major issue.

A basic example use would look like this:

\(treemacs-apply-diagnostics
    (thunk-delay '(\"/path/to/file/x\" 'treemacs-diagnostic-warning-face
                   \"/path/to/file/y\" 'treemacs-diagnostic-error-face
                   \"/path/to/file/z\" '((:underline \"green\")))))"
  (treemacs-debounce treemacs--diagnostic-timer treemacs--apply-diagnostics-delay
    (treemacs-run-in-every-buffer
     (save-excursion
       (-each (overlays-in (point-min) (point-max)) #'delete-overlay)
       (-let [diagnostics (thunk-force provider)]
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
