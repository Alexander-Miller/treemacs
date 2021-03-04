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
;;; Simple bits of code to make treemacs compatible with other packages
;;; that aren't worth the effort of being turned into their own package.

;;; Code:

(require 'dash)
(require 'treemacs-customization)
(require 'treemacs-logging)
(require 'treemacs-scope)
(require 'treemacs-core-utils)
(require 'treemacs-interface)

(eval-when-compile
  (require 'treemacs-macros))

(treemacs-only-during-init
 ;; make sure frame params are not persisted by desktop-save-mode
 (push '(treemacs-id . :never) frameset-filter-alist)
 (push '(treemacs-workspace . :never) frameset-filter-alist))

(with-eval-after-load 'tramp
  (setf treemacs--file-name-handler-alist
        (with-no-warnings
          (list
           (cons tramp-file-name-regexp #'tramp-file-name-handler)))))

(with-eval-after-load 'recentf
  (with-no-warnings
    (add-to-list 'recentf-exclude treemacs-persist-file)
    (add-to-list 'recentf-exclude treemacs-last-error-persist-file)))

(with-eval-after-load 'eyebrowse

  (defun treemacs--follow-after-eyebrowse-switch ()
    (when treemacs-follow-mode
      (--when-let (treemacs-get-local-window)
        (with-selected-window it
          (treemacs--follow-after-buffer-list-update)
          (hl-line-highlight)))))

  (declare-function treemacs--follow-after-eyebrowse-switch "treemacs-compatibility")

  (add-hook 'eyebrowse-post-window-switch-hook #'treemacs--follow-after-eyebrowse-switch))

(with-eval-after-load 'winum
  (when (boundp 'winum-ignored-buffers-regexp)
    (add-to-list 'winum-ignored-buffers-regexp (regexp-quote (format "%sScoped-Buffer-" treemacs--buffer-name-prefix)))))

(with-eval-after-load 'ace-window
  (when (boundp 'aw-ignored-buffers)
    (push 'treemacs-mode aw-ignored-buffers)))

(with-eval-after-load 'golden-ratio
  (when (boundp 'golden-ratio-exclude-modes)
    (add-to-list 'golden-ratio-exclude-modes 'treemacs-mode)))

(with-eval-after-load 'indent-guide
  (when (boundp 'indent-guide-inhibit-modes)
    (push 'treemacs-mode indent-guide-inhibit-modes)))

(with-eval-after-load 'ediff
  (add-hook
   'ediff-before-setup-hook
   (defun treemacs--dont-diff-in-treemacs-window ()
     "Select `next-window' before ediff's window setup.
Treemacs is by default a side-window, meaning it'll throw an error if ediff trys
to split it."
     (when treemacs--in-this-buffer
       (select-window (next-window))))))

(with-eval-after-load 'persp-mode
  (defun treemacs--remove-treemacs-window-in-new-frames (persp-activated-for)
    (when (eq persp-activated-for 'frame)
      (-when-let (w (--first (treemacs-is-treemacs-window? it)
                             (window-list)))
        (unless (assoc (treemacs-scope->current-scope treemacs--current-scope-type) treemacs--scope-storage)
          (delete-window w)))))
  (declare-function treemacs--remove-treemacs-window-in-new-frames "treemacs-compatibility")
  (if (boundp 'persp-activated-functions)
      (add-to-list 'persp-activated-functions #'treemacs--remove-treemacs-window-in-new-frames)
    (treemacs-log-failure "`persp-activated-functions' not defined - couldn't add compatibility.")))

(with-eval-after-load 'perspective
  (defun treemacs--remove-treemacs-window-in-new-frames (&rest _)
    (-when-let (w (--first (treemacs-is-treemacs-window? it)
                           (window-list)))
      (unless (assoc (treemacs-scope->current-scope treemacs--current-scope-type) treemacs--scope-storage)
        (delete-window w))))
  (declare-function treemacs--remove-treemacs-window-in-new-frames "treemacs-compatibility")
  (if (boundp 'persp-activated-hook)
      (add-to-list 'persp-activated-hook #'treemacs--remove-treemacs-window-in-new-frames)
    (treemacs-log-failure "`persp-activated-hook' not defined - couldn't add compatibility.")))

(defun treemacs--split-window-advice (original-split-function &rest args)
  "Advice to make sure window splits are sized correctly with treemacs.
This will treat the treemacs window as a side-window for the duration of the
split, calling the ORIGINAL-SPLIT-FUNCTION with its ARGS.  This prevents the
calculations in `split-window-right' from outputting the wrong result for the
width of the new window when the treemacs window is visible."
  (-let [w (treemacs-get-local-window)]
    (unwind-protect
        (progn
          (when w (set-window-parameter w 'window-side treemacs-position))
          (apply original-split-function args))
      (when (and w (null treemacs-display-in-side-window))
        (set-window-parameter w 'window-side nil)))))
(advice-add 'split-window-right :around #'treemacs--split-window-advice)

(with-eval-after-load 'org
  (defun treemacs-store-org-link ()
    "Store an `org-mode' link for the node at point."
    (when (eq major-mode 'treemacs-mode)
      (-when-let* ((btn (treemacs-current-button))
                   (file (treemacs--nearest-path btn)))
        (-let [link (format "file:%s" (abbreviate-file-name file))]
          (with-no-warnings
            (org-add-link-props
             :link link
             :description (treemacs--filename file)))
          link))))
  (with-no-warnings
    (if (fboundp 'org-link-set-parameters)
        (org-link-set-parameters "treemacs" :store #'treemacs-store-org-link)
      (add-hook 'org-store-link-functions #'treemacs-store-org-link))))

(with-eval-after-load 'evil-escape
  (when (boundp 'evil-escape-excluded-major-modes)
    (add-to-list 'evil-escape-excluded-major-modes 'treemacs-mode)))

(defun treemacs-load-all-the-icons-with-workaround-font (font)
  "Load the `treemacs-all-the-icons' package using a workaround FONT for tabs.
Use this if you experience the issue of icons jumping around when they are
closed or opened which can appear when using specific fonts.

FONT should be a simple string name, for example \"Hermit\".

Finding the right FONT is a matter of trial and error, you can quickly try
different fonts using `set-frame-font'.

The workaround will overwrite the values for `treemacs-indentation' and
`treemacs-indentation-string', using your own values for them is no longer
possible.

Can only work if the `treemacs-all-the-icons' module has not been loaded yet."
  (defvar treemacs-all-the-icons-tab-font font)
  (setf treemacs-indentation 1
        treemacs-indentation-string (propertize "\t" 'face `((:family ,treemacs-all-the-icons-tab-font))))
  (require 'treemacs-all-the-icons)
  (treemacs-load-theme "all-the-icons"))

(provide 'treemacs-compatibility)

;;; treemacs-compatibility.el ends here
