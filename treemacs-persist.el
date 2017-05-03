;;; treemacs.el --- A tree style file viewer package

;; Copyright (C) 2017 Alexander Miller

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
;;; Follow mode definition only. Everything else is extracted into its
;;; own file to reduce clutter.

;;; Code:

(require 'f)
(require 's)
(require 'treemacs-customization)

(declare-function treemacs-persist "treemacs")
(declare-function treemacs-restore "treemacs")

(defconst treemacs--persist-file (f-join user-emacs-directory ".cache" "treemacs-persist")
  "File treemacs uses to persist its current state.")

(defun treemacs--read-persist-data ()
  "Read the data stored in `treemacs--persist-file'."
    (when (f-file? treemacs--persist-file)
      (let ((ret   (list))
            (lines (s-lines (f-read treemacs--persist-file))))
        (while lines
          (let ((split (s-split " : " (pop lines))))
            (add-to-list 'ret
                         `(,(cl-first split) . ,(cl-second split)))))
        ret)))

(defun treemacs--maybe-persist ()
  "Hook function to save treemacs state when conditions for it are met.
Persistence takes place when the treemacs buffer is killed or when Emacs shuts
down and `treemacs--never-persist' is not t and a state saving mode like
desktop save mode is on."
  (when (and (not treemacs--never-persist)
             (or desktop-save-mode
                 (and (bound-and-true-p persp-auto-save-opt)
                      (not (eq 0 persp-auto-save-opt)))))
    (treemacs-persist)))

(defun treemacs--get-open-dirs ()
  "Collect the paths of all currently expanded folders."
  ;; not using open dirs cache on account of its arbitrary ordering
  ;; dirs are collected - and reopened - from top to bottom
  (save-excursion
    (goto-char 0)
    (let ((btn  (next-button (point)))
          (dirs (list)))
      (while btn
        (when (eq 'dir-open (button-get btn 'state))
          (add-to-list 'dirs (button-get btn 'abs-path) t))
        (setq btn (next-button (button-end btn))))
      dirs)))

(with-eval-after-load "desktop"

  (defun treemacs--desktop-handler (&rest _)
    "Treemacs mode handler for desktop save mode."
    ;; args are irrelevant since treemacs has but one way to be restored
    (treemacs-restore))

  (add-to-list 'desktop-buffer-mode-handlers
               '(treemacs-mode . treemacs--desktop-handler)))

(provide 'treemacs-persist)

;;; treemacs-persist.el ends here
