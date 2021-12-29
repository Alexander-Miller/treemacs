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

;; TODO

;; NOTE: This module is lazy-loaded.


;;; Code:

(require 'treemacs-tags)
(require 'treemacs-core-utils)

(eval-when-compile
  (require 'treemacs-macros))

(defvar treemacs--peek-timer nil)

(defvar treemacs--peeked-buffers nil)

(defvar treemacs--pre-peek-state nil
  "List of window, buffer to restore and buffer to kill treemacs used for peeking.")

(defun treemacs--kill-peek-buffers ()
  "Kill buffers opened during peeking that are no longer needed."
  (-each treemacs--peeked-buffers #'kill-buffer)
  (setf treemacs--peeked-buffers nil))

(defun treemacs--setup-peek-buffer (path)
  "Setup the peek buffer and window for PATH."
  (let* ((file-buffer (get-file-buffer path))
         (next-window (next-window (selected-window)))
         (window (if file-buffer
                     (or (get-buffer-window file-buffer)
                         next-window)
                   next-window)))
    (save-selected-window
      (select-window window)
      (unless treemacs--pre-peek-state
        (setf treemacs--pre-peek-state (list window (window-buffer window))))
      (if file-buffer
          (switch-to-buffer file-buffer :norecord)
        (find-file-existing path)
        (add-to-list 'treemacs--peeked-buffers (current-buffer))))))

(defun treemacs--do-peek ()
  "Timer callback to set up the peeked buffer.
Check if the node at point is a file, and if yes take a peek."
  (when (eq t treemacs--in-this-buffer)
    (let* ((btn (treemacs-current-button))
           (path (and btn (treemacs-button-get btn :path))))
      (when (and path
                 (stringp path)
                 (file-exists-p path))
        (treemacs--setup-peek-buffer path)))))

(defun treemacs--finish-peek-on-window-leave (&optional _)
  "Finish peeking when the treemacs window is no longer selected.
Shut down peek-mode while making sure that the current buffer will not be
purged."
  (let ((treemacs-buffer (treemacs-get-local-buffer))
        (current-buffer (current-buffer)))
    (unless (equal treemacs-buffer current-buffer)
      (setf treemacs--peeked-buffers
            (delete current-buffer treemacs--peeked-buffers))
      (treemacs-peek-mode -1))))

(defun treemacs--setup-peek-mode ()
  "Set up faces, timers, and hooks etc."
  (when treemacs--fringe-indicator-overlay
    (overlay-put treemacs--fringe-indicator-overlay
                 'face 'treemacs-peek-mode-indicator-face))
  (when treemacs--peek-timer (cancel-timer treemacs--peek-timer))
  (setf treemacs--peek-timer
        (run-with-idle-timer 0.5 :repeat #'treemacs--do-peek))
  (add-hook
   'window-selection-change-functions #'treemacs--finish-peek-on-window-leave
   nil :local))

(defun treemacs--tear-down-peek-mode (&optional restore-window)
  "Tear down faces, timers.
Restore the initial window buffer when RESTORE-WINDOW is non-nil.  Will only
happen when `treemacs-peek-mode' has been called interactively, when the
tear-down happens on account of the window-leave hook the current buffer is
kept."
  (with-current-buffer (treemacs-get-local-buffer)
    (when treemacs--fringe-indicator-overlay
      (overlay-put treemacs--fringe-indicator-overlay
                   'face 'treemacs-fringe-indicator-face))
    (when treemacs--peek-timer (cancel-timer treemacs--peek-timer))
    (treemacs--kill-peek-buffers)
    (remove-hook
     'window-selection-change-functions
     #'treemacs--finish-peek-on-window-leave
     :local)
    (when (and restore-window treemacs--pre-peek-state)
      (-let [(window buffer) treemacs--pre-peek-state]
        (with-selected-window window
          (switch-to-buffer buffer))))
    (setf treemacs--pre-peek-state nil)))

;;;###autoload
(define-minor-mode treemacs-peek-mode
  "Minor mode that allows you to peek at buffers before deciding to open them.

While the mode is active treemacs will automatically display the file at point,
without leaving the treemacs window.

Peeking will stop when you leave the treemacs window, be it through a command
like `treemacs-RET-action' or some other window selection change.

Files' buffers that have been opened for peeking will be cleaned up if they did
not exist before peeking started.

The peeked window can be scrolled using
`treemacs-next/previous-line-other-window' and
`treemacs-next/previous-page-other-window'"
  :init-value nil
  :global     t
  :lighter    nil
  :group      'treemacs
  (if treemacs-peek-mode
      (progn
        (unless (boundp 'window-selection-change-functions)
          (user-error "%s %s"
                      "Peek-mode is only available in Emacs"
                      "versions that support `window-selection-change-functions'"))
        (treemacs--setup-peek-mode))
    (treemacs--tear-down-peek-mode (called-interactively-p 'interactive))))

(provide 'treemacs-peek-mode)

;;; treemacs-peek-mode.el ends here
