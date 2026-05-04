;;; treemacs-git-status-indicator.el --- Show git status letters in the margin -*- lexical-binding: t -*-

;; Copyright (C) 2024 Alexander Miller

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

;; Minor mode to display git status indicator letters (M, A, ?, !, U, R)
;; in the margin of the treemacs buffer.

;; NOTE: This module is lazy-loaded.

;;; Code:

(require 'ht)
(require 'dash)
(require 'treemacs-core-utils)
(require 'treemacs-annotations)
(require 'treemacs-async)

(eval-when-compile
  (require 'treemacs-macros))

(defvar treemacs--after-annotation-applied)
(defvar treemacs-git-status-indicator-mode)

(defcustom treemacs-git-status-indicator-position 'left
  "Which margin to display git status indicator letters in.
Can be `left' or `right'."
  :type '(choice (const :tag "Left margin"  left)
                 (const :tag "Right margin" right))
  :group 'treemacs-git)

(defconst treemacs--git-indicator-face-letter-alist
  '((treemacs-git-modified-face  . "M")
    (treemacs-git-conflict-face  . "U")
    (treemacs-git-untracked-face . "?")
    (treemacs-git-ignored-face   . "!")
    (treemacs-git-added-face     . "A")
    (treemacs-git-renamed-face   . "R"))
  "Alist mapping git face symbols to single-letter status indicators.")

(defvar-local treemacs--git-indicator-prev-hl-ov nil
  "The indicator overlay that currently has the hl-line face applied.")

(defvar treemacs--git-indicator-bulk-applying nil
  "Non-nil while `treemacs--git-indicator-apply-to-buffer' is running.")

(defun treemacs--git-indicator-margin-spec (str)
  "Build a margin display spec for STR."
  (let ((margin-side (if (eq treemacs-git-status-indicator-position 'right)
                         'right-margin
                       'left-margin)))
    (propertize " " 'display `((margin ,margin-side) ,str))))

(defun treemacs--git-indicator-update (_btn git-face)
  "Update the margin indicator for the current node based on GIT-FACE.
Called as `treemacs--after-annotation-applied' after each node's
annotation is applied.  Point is expected to be on the node's line.

_BTN: Button (unused, point is already positioned)
GIT-FACE: Face"
  (let* ((letter (alist-get git-face treemacs--git-indicator-face-letter-alist))
         (bol (line-beginning-position))
         (eol (line-end-position))
         ;; always display something — a space when no status — so that
         ;; hl-line highlighting extends into the margin on every node
         (str (or letter " ")))
    ;; remove ALL existing indicator overlays on this line
    (dolist (ov (overlays-in bol eol))
      (when (overlay-get ov 'treemacs-git-indicator)
        (delete-overlay ov)))
    ;; place indicator overlay
    ;; the git face goes on the OVERLAY's face property (not the string)
    ;; so it applies to the before-string and can be swapped by hl-line
    (let ((ov (make-overlay bol (1+ bol) nil t))
          (on-hl-line (and (not treemacs--git-indicator-bulk-applying)
                           (boundp 'hl-line-overlay)
                           hl-line-overlay
                           (overlay-buffer hl-line-overlay)
                           (= (overlay-start hl-line-overlay) bol))))
      (overlay-put ov 'before-string (treemacs--git-indicator-margin-spec str))
      (overlay-put ov 'evaporate t)
      (overlay-put ov 'treemacs-git-indicator t)
      (overlay-put ov 'treemacs-git-face git-face)
      ;; if this line is currently highlighted by hl-line, apply the
      ;; composite face immediately — hl-line-highlight won't re-run
      ;; since the cursor didn't move
      (if on-hl-line
          (progn
            (overlay-put ov 'face (if git-face
                                      (list 'hl-line git-face)
                                    'hl-line))
            (setq treemacs--git-indicator-prev-hl-ov ov))
        (overlay-put ov 'face git-face)))))

(defun treemacs--git-indicator-hl-update ()
  "Update indicator faces when hl-line moves.
Restores the previous line's indicator to its normal git face and applies
the hl-line face to the current line's indicator, mirroring how hl-line
treats filename text: hl-line attributes take priority, git face fills in
the rest."
  (when treemacs-git-status-indicator-mode
    ;; restore previous highlighted overlay to its normal git face
    (when (and treemacs--git-indicator-prev-hl-ov
               (overlay-buffer treemacs--git-indicator-prev-hl-ov))
      (overlay-put treemacs--git-indicator-prev-hl-ov
                   'face
                   (overlay-get treemacs--git-indicator-prev-hl-ov 'treemacs-git-face)))
    ;; apply hl-line face to current line's indicator
    ;; use 'hl-line face (buffer-locally remapped to treemacs-hl-line-face)
    ;; with git-face as fallback — same merge as hl-line overlay over text property
    (setq treemacs--git-indicator-prev-hl-ov nil)
    (dolist (ov (overlays-at (line-beginning-position)))
      (when (overlay-get ov 'treemacs-git-indicator)
        (-let [git-face (overlay-get ov 'treemacs-git-face)]
          (overlay-put ov 'face (if git-face
                                    (list 'hl-line git-face)
                                  'hl-line))
          (setq treemacs--git-indicator-prev-hl-ov ov))))))

(defun treemacs--git-indicator-setup-buffer ()
  "Set the margin width in the current treemacs buffer.
Uses `treemacs-git-status-indicator-position' to decide which margin."
  (if (eq treemacs-git-status-indicator-position 'right)
      (progn
        (setq right-margin-width 2 left-margin-width 0)
        ;; default: text | right-margin | right-fringe
        ;; we want: text | right-fringe  | right-margin
        (setq fringes-outside-margins nil))
    (setq left-margin-width 2 right-margin-width 0)
    ;; default: left-margin | left-fringe | text
    ;; we want: left-fringe | left-margin | text
    (setq fringes-outside-margins t))
  (advice-add #'hl-line-highlight :after #'treemacs--git-indicator-hl-update)
  (when (get-buffer-window (current-buffer))
    (set-window-buffer (get-buffer-window (current-buffer))
                       (current-buffer))))

(defun treemacs--git-indicator-apply-to-buffer ()
  "Apply git status indicators to all visible nodes in the current buffer."
  (let ((treemacs--git-indicator-bulk-applying t))
    (treemacs-with-writable-buffer
     (save-excursion
       (goto-char (point-min))
       (let ((btn (point)))
         (while (setf btn (next-button btn))
           (let* ((path (treemacs-button-get btn :path))
                  (git-cache
                   (->> path
                        (treemacs--parent-dir)
                        (ht-get treemacs--git-cache)))
                  (git-face (and git-cache (ht-get git-cache path))))
             (goto-char btn)
             (treemacs--git-indicator-update btn git-face))))))))

(defun treemacs--enable-git-status-indicator-mode ()
  "Setup for `treemacs-git-status-indicator-mode'."
  (setf treemacs--after-annotation-applied #'treemacs--git-indicator-update)
  (add-hook 'treemacs-mode-hook #'treemacs--git-indicator-setup-buffer)
  (treemacs-run-in-all-derived-buffers
   (treemacs--git-indicator-setup-buffer)
   (treemacs--git-indicator-apply-to-buffer)))

(defun treemacs--disable-git-status-indicator-mode ()
  "Tear-down for `treemacs-git-status-indicator-mode'."
  (setf treemacs--after-annotation-applied nil)
  (remove-hook 'treemacs-mode-hook #'treemacs--git-indicator-setup-buffer)
  (treemacs-run-in-all-derived-buffers
   (advice-remove #'hl-line-highlight #'treemacs--git-indicator-hl-update)
   (remove-overlays (point-min) (point-max) 'treemacs-git-indicator t)
   (setq treemacs--git-indicator-prev-hl-ov nil)
   (setq left-margin-width 0 right-margin-width 0 fringes-outside-margins nil)
   (when (get-buffer-window (current-buffer))
     (set-window-buffer (get-buffer-window (current-buffer))
                        (current-buffer)))))

;;;###autoload
(define-minor-mode treemacs-git-status-indicator-mode
  "Minor mode to display git status letters in the margin.

When enabled a single-letter indicator will appear in the margin next to each
file or directory that has a git status:

  M - modified
  A - added
  ? - untracked
  ! - ignored
  U - conflict (unmerged)
  R - renamed

The indicator is colored with the same face as the filename and responds to
hl-line highlighting.

Which margin is used is controlled by `treemacs-git-status-indicator-position'.

Requires `treemacs-git-mode' to be active for git status information."
  :init-value nil
  :global     t
  :lighter    nil
  :group      'treemacs-git
  (if treemacs-git-status-indicator-mode
      (treemacs--enable-git-status-indicator-mode)
    (treemacs--disable-git-status-indicator-mode)))

(provide 'treemacs-git-status-indicator)

;;; treemacs-git-status-indicator.el ends here
