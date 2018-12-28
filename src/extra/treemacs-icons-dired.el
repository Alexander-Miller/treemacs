;;; treemacs-icons-dired.el --- Treemacs icons for dired -*- lexical-binding: t -*-

;; Copyright (C) 2018 Alexander Miller

;; Author: Alexander Miller <alexanderm@web.de>
;; Package-Requires: ((treemacs "0.0") (emacs "25.2"))
;; Package-Version: 0
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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;; Treemacs icons for dired. Code is based on all-the-icons-dired.el

;;; Code:

(require 'treemacs)
(require 'hl-line)
(require 'dired)

(defvar-local treemacs-icons-dired-displayed nil
  "Flags whether icons have been added.")

(defun treemacs-icons-dired--display ()
  "Display the icons of files in a dired buffer."
  (when (and (display-graphic-p)
             (not treemacs-icons-dired-displayed)
             dired-subdir-alist)
    (setq-local treemacs-icons-dired-displayed t)
    (treemacs-with-writable-buffer
     (save-excursion
       (goto-char (point-min))
       (forward-line 4)
       (while (not (eobp))
         (when  (dired-move-to-filename nil)
           (let* ((file (dired-get-filename nil t))
                  (icon (if (file-directory-p file)
                            treemacs-icon-closed
                          (treemacs-icon-for-file file))))
             (insert icon)
             (forward-line 1))))))))

(defun treemacs-icons-dired--reset (&rest _args)
  "Reset `treemacs-icons-dired--display' when the buffer is reverted."
  (setq-local treemacs-icons-dired-displayed nil))

(defun treemacs-icons-dired--update-icon-selection ()
  "Highlight current icon, unhighlight `treemacs--last-highlight'.
This will make sure the icons' background colors will align with hl-line mode."
  (when (and hl-line-mode (eq major-mode 'dired-mode))
    (condition-case e
        (progn
          (treemacs--evade-image)
          (let* ((last-pos treemacs--last-highlight)
                 (curr-pos (next-single-char-property-change (point-at-bol) 'img-selected nil (point-at-eol)))
                 (img-selected (get-text-property curr-pos 'img-selected)))
            (treemacs-with-writable-buffer
             (when (and last-pos (< last-pos (point-max)))
               (let ((img-unselected (get-text-property last-pos 'img-unselected)))
                 (put-text-property last-pos (1+ last-pos) 'display img-unselected)))
             (when (and img-selected (< curr-pos (point-max)))
               (put-text-property curr-pos (1+ curr-pos) 'display img-selected)
               (setq treemacs--last-highlight (copy-marker curr-pos))))))
      (error
       (treemacs-log "Error on highlight, this shouldn't happen: %s" e)))))

(defun treemacs-icons-dired--enable-highlight-correction ()
  "Locally add `treemacs-icons-dired--update-icon-selection'."
  (add-hook 'post-command-hook #'treemacs-icons-dired--update-icon-selection nil :local))

(defun treemacs-icons-dired--disable-highlight-correction ()
  "Locally remove `treemacs-icons-dired--update-icon-selection'."
  (remove-hook 'post-command-hook #'treemacs-icons-dired--update-icon-selection :local))

;;;###autoload
(define-minor-mode treemacs-icons-dired-mode
  "Display treemacs icons for each files in a dired buffer."
  :require 'treemacs-icons-dired
  :init-value nil
  :global     t
  (if treemacs-icons-dired-mode
      (progn
        (add-hook 'dired-after-readin-hook #'treemacs-icons-dired--display)
        (add-hook 'dired-mode-hook #'treemacs-icons-dired--enable-highlight-correction)
        (dolist (buffer (buffer-list))
          (with-current-buffer buffer
            (when (derived-mode-p 'dired-mode)
              (treemacs-icons-dired--enable-highlight-correction)
              (treemacs-icons-dired--display)))))
    (remove-hook 'dired-after-readin-hook #'treemacs-icons-dired--display)
    (remove-hook 'dired-mode-hook #'treemacs-icons-dired--enable-highlight-correction)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (derived-mode-p 'dired-mode)
          (treemacs-icons-dired--disable-highlight-correction)
          (dired-revert))))))

(advice-add 'dired-revert :before #'treemacs-icons-dired--reset)

(provide 'treemacs-icons-dired)

;;; treemacs-icons-dired.el ends here
