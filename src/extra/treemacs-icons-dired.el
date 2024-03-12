;;; treemacs-icons-dired.el --- Treemacs icons for dired -*- lexical-binding: t -*-

;; Copyright (C) 2024 Alexander Miller

;; Author: Alexander Miller <alexanderm@web.de>
;; Package-Requires: ((treemacs "0.0") (emacs "26.1"))
;; Version: 0
;; Homepage: https://github.com/Alexander-Miller/treemacs

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
;;; Treemacs icons for Dired.  Code is based on all-the-icons-dired.el

;;; Code:

(require 'treemacs)
(require 'hl-line)
(require 'dired)
(require 'pcase)

(eval-when-compile
  (require 'treemacs-macros))

(defvar treemacs-icons-dired--ranger-adjust nil)
(with-eval-after-load 'ranger (setf treemacs-icons-dired--ranger-adjust t))

(defvar-local treemacs-icons-dired-displayed nil
  "Flags whether icons have been added.")

(defvar-local treemacs-icons-dired--covered-subdirs nil
  "List of subdirs icons were already added for.")

(defun treemacs-icons-dired--display ()
  "Display the icons of files in a Dired buffer."
  (when (and (display-graphic-p)
             (not treemacs-icons-dired-displayed)
             dired-subdir-alist)
    (setq-local treemacs-icons-dired-displayed t)
    (setq-local treemacs-icons (treemacs-theme->gui-icons treemacs--current-theme))
    (pcase-dolist (`(,path . ,pos) dired-subdir-alist)
      (treemacs-icons-dired--display-icons-for-subdir path pos))))

(defun treemacs-icons-dired--display-icons-for-subdir (path pos)
  "Display icons for subdir PATH at given POS."
  (unless (member path treemacs-icons-dired--covered-subdirs)
    (add-to-list 'treemacs-icons-dired--covered-subdirs path)
    (treemacs-with-writable-buffer
     (save-excursion
       (goto-char pos)
       (dired-goto-next-file)
       (treemacs-block
        (while (not (eobp))
          (if (dired-move-to-filename nil)
              (let* ((file (dired-get-filename nil t))
                     (icon (if (file-directory-p file)
                               (treemacs-icon-for-dir file 'closed)
                             (treemacs-icon-for-file file))))
                (insert icon))
            (treemacs-return nil))
          (forward-line 1) ))))))

(defun treemacs-icons-dired--insert-subdir-advice (&rest args)
  "Advice to Dired & Dired+ insert-subdir commands.
Will add icons for the subdir in the `car' of ARGS."
  (let* ((path (file-name-as-directory (car args)))
         (pos (cdr (assoc path dired-subdir-alist))))
    (when pos
      (treemacs-icons-dired--display-icons-for-subdir path pos))))

(advice-add #'dired-insert-subdir :after #'treemacs-icons-dired--insert-subdir-advice)
(with-eval-after-load 'dired+
  (when (fboundp 'diredp-insert-subdirs)
    (advice-add #'diredp-insert-subdirs :after #'treemacs-icons-dired--insert-subdir-advice)))

(defun treemacs-icons-dired--kill-subdir-advice (&rest _args)
  "Advice to Dired kill-subdir commands.
Will remove the killed subdir from `treemacs-icons-dired--covered-subdirs'."
  (setf treemacs-icons-dired--covered-subdirs (delete (dired-current-directory) treemacs-icons-dired--covered-subdirs)))

(advice-add #'dired-kill-subdir :before #'treemacs-icons-dired--kill-subdir-advice)

(defun treemacs-icons-dired--reset (&rest _args)
  "Reset metadata on revert."
  (setq-local treemacs-icons-dired--covered-subdirs nil)
  (setq-local treemacs-icons-dired-displayed nil))

(defun treemacs-icons-dired--add-icon-for-new-entry (file &rest _)
  "Add an icon for a new single FILE added by Dired."
  (let (buffer-read-only)
    (insert (if (file-directory-p file)
                (treemacs-icon-for-dir file 'closed)
              (treemacs-icon-for-file file)))))

(defun treemacs-icons-dired--set-tab-width ()
  "Set the local `tab-width' to 1.
Necessary for the all-the-icons based themes."
  (setq-local tab-width 1))

(defun treemacs-icons-dired--setup ()
  "Setup for the minor-mode."
  (add-hook 'dired-after-readin-hook #'treemacs-icons-dired--display)
  (add-hook 'dired-mode-hook #'treemacs--select-icon-set)
  (add-hook 'dired-mode-hook #'treemacs-icons-dired--set-tab-width)
  (advice-add 'dired-revert :before #'treemacs-icons-dired--reset)
  (advice-add 'ranger-setup :before #'treemacs--select-icon-set)
  (advice-add 'dired-add-entry :after #'treemacs-icons-dired--add-icon-for-new-entry)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'dired-mode)
        (treemacs-icons-dired--set-tab-width)
        (treemacs--select-icon-set)
        (treemacs-icons-dired--display)))))

(defun treemacs-icons-dired--teardown ()
  "Tear-down for the minor-mode."
  (remove-hook 'dired-after-readin-hook #'treemacs-icons-dired--display)
  (remove-hook 'dired-mode-hook #'treemacs--select-icon-set)
  (remove-hook 'dired-mode-hook #'treemacs-icons-dired--set-tab-width)
  (advice-remove 'dired-revert #'treemacs-icons-dired--reset)
  (advice-remove 'ranger-setup #'treemacs--select-icon-set)
  (advice-remove 'dired-add-entry #'treemacs-icons-dired--add-icon-for-new-entry)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'dired-mode)
        (dired-revert)))))

;;;###autoload
(define-minor-mode treemacs-icons-dired-mode
  "Display treemacs icons for each file in a Dired buffer."
  :require    'treemacs-icons-dired
  :init-value nil
  :global     t
  :group      'treemacs
  (if treemacs-icons-dired-mode
      (treemacs-icons-dired--setup)
    (treemacs-icons-dired--teardown)))

;;;###autoload
(defun treemacs-icons-dired-enable-once ()
  "Enable `treemacs-icons-dired-mode' and remove self from `dired-mode-hook'.

This function is meant to be used as a single-use toggle added to
`dired-mode-hook' to enable icons for Dired only once, without having to use
\"with-eval-after-load \\='dired\", since Dired tends to be loaded early."
  (treemacs-icons-dired-mode)
  (remove-hook 'dired-mode-hook #'treemacs-icons-dired-enable-once))


(provide 'treemacs-icons-dired)

;;; treemacs-icons-dired.el ends here
