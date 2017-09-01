;;; treemacs.el --- A tree style file viewer package -*- lexical-binding: t -*-

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
;;; General persistence and desktop-save-mode-integration.

;;; Code:

(require 'dash)
(require 'f)
(require 's)
(require 'treemacs-customization)
(require 'treemacs-impl)

(defconst treemacs--persist-file (f-join user-emacs-directory ".cache" "treemacs-persist")
  "File treemacs uses to persist its current state.")

;;;###autoload
(defun treemacs-restore ()
  "Restore the treemacs state saved by `treeemacs-persist'."
  (interactive)
  (-if-let (stored-data (treemacs--read-persist-data))
      (let ((root      (cdr (assoc "ROOT"      stored-data)))
            (open-dirs (cdr (assoc "OPEN-DIRS" stored-data)))
            (point-at  (cdr (assoc "POINT-AT"  stored-data))))
        (unless (f-dir? root)      (error "[Treemacs] %s is not a directory, so it cannot be restored" root))
        (unless (f-readable? root) (error "[Treemacs] %s is not readable, so it cannot be restored" root))
        (treemacs--buffer-teardown)
        (treemacs--init root)
        ;; Don't always start searching from the very top
        (let ((start 0)
              (btn   nil))
          (--each
              (--filter (f-readable? it) (s-split "|" open-dirs t))
            (setq btn (treemacs--goto-button-at it start))
            (treemacs--push-button btn)
            (setq start (button-end btn))))
        (when (f-exists? point-at)
          (treemacs--goto-button-at point-at))
        (recenter)
        ;; selected line is not visible otherwise
        (hl-line-mode -1)
        (hl-line-mode t))))

(defun treemacs-persist ()
  "Save current state, allowing it to be restored with `treemacs-restore'."
  (interactive)
  (-if-let (buf (get-buffer treemacs--buffer-name))
      (with-current-buffer buf
        (save-excursion
          (let ((root      (treemacs--current-root))
                (open-dirs (treemacs--get-open-dirs))
                (point-at  (treemacs--prop-at-point 'abs-path))
                (text      ""))
            (treemacs--check-persist-file)
            (setq text (s-concat text (format "ROOT : %s" root)))
            (setq text (s-concat text "\n" (format "OPEN-DIRS : %s" (s-join "|" open-dirs))))
            (setq text (s-concat text "\n" (format "POINT-AT : %s" point-at)))
            (f-write text 'utf-8 treemacs--persist-file))))))

(defun treemacs--check-persist-file ()
  "Make sure treemacs' persist file exists and is set up correctly.
Create files/directories if necessary."
  (let ((cache-dir (f-join user-emacs-directory ".cache")))
    (if (f-exists? cache-dir)
        (progn
          (unless (f-dir? cache-dir)
            (error " user-emacs-directory/.cache is not a directory "))
          (unless (f-readable? cache-dir)
            (error " user-emacs-directory/.cache is not readable "))
          (unless (f-exists? treemacs--persist-file)
            (f-touch treemacs--persist-file)))
      (progn
        (f-mkdir cache-dir)
        (f-touch treemacs--persist-file)))))

(defun treemacs--read-persist-data ()
  "Read the data stored in `treemacs--persist-file'."
    (when (f-file? treemacs--persist-file)
      (let ((ret   (list))
            (lines (s-lines (f-read treemacs--persist-file))))
        (while lines
          (let ((split (s-split " : " (pop lines))))
            (when (= 2 (length split))
              (push `(,(cl-first split) . ,(cl-second split)) ret))))
        ret)))

(defun treemacs--maybe-persist ()
  "Hook function to save treemacs state when conditions for it are met.
Persistence takes place when the treemacs buffer is killed or when Emacs shuts
down and `treemacs-never-persist' is not t and a state saving mode like
desktop save mode is on."
  (when (and (not treemacs-never-persist)
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
        (when (eq 'dir-node-open (button-get btn 'state))
          (push (button-get btn 'abs-path) dirs))
        (setq btn (next-button (button-end btn))))
      (nreverse dirs))))

(with-eval-after-load "desktop"

  (defun treemacs--desktop-handler (&rest _)
    "Treemacs mode handler for desktop save mode."
    ;; args are irrelevant since treemacs has but one way to be restored
    (treemacs-restore))

  (add-hook 'desktop-save-hook #'treemacs--maybe-persist)

  (add-to-list 'desktop-buffer-mode-handlers
               '(treemacs-mode . treemacs--desktop-handler)))

(with-eval-after-load "persp-mode"

  (defun treemacs--persp-save (b)
    (with-current-buffer b
      (when (eq major-mode 'treemacs-mode)
        (treemacs-persist)
        '(def-treemacs))))

  (defun treemacs--persp-load (save-list)
    (when (eq (car save-list) 'def-treemacs)
      (treemacs-restore)
      t))

  (if (and (boundp 'persp-save-buffer-functions)
           (boundp 'persp-load-buffer-functions))
      (progn
        (add-to-list 'persp-save-buffer-functions (with-no-warnings #'treemacs--persp-save))
        (add-to-list 'persp-load-buffer-functions (with-no-warnings #'treemacs--persp-load)))
    (treemacs--log "Persp's save and load buffer function vars don't seem to be defined. Failed to set persist hooks.")))

(provide 'treemacs-persist)

;;; treemacs-persist.el ends here
