;;; treemacs.el --- A tree style file viewer package -*- lexical-binding: t -*-

;; Copyright (C) 2018 Alexander Miller

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
;;; General persistence and desktop-save-mode integration.

;;; Code:

(require 'hl-line)
(require 'dash)
(require 'f)
(require 's)
(require 'treemacs-customization)
(require 'treemacs-impl)
(require 'treemacs-follow-mode)
(eval-and-compile (require 'treemacs-macros))

(declare-function treemacs-mode "treemacs-mode")
(declare-function treemacs-toggle "treemacs")

(defconst treemacs--persist-file (f-join user-emacs-directory ".cache" "treemacs-persist")
  "File treemacs uses to persist its current state.")

(defconst treemacs--desktop-helper-name "*Desktop Treemacs Helper*")

;;;###autoload
(defun treemacs--restore ()
  "Restore the entire treemacs state saved by `treeemacs--persist'."
  ;; condition is true when we're running in eager restoration and the frameset is not yet restored
  ;; in this case this function will be run again, with restored frame parameters, in `desktop-after-read-hook'
  (unless (or treemacs-never-persist
              (--all? (null (frame-parameter it 'treemacs-id)) (frame-list)))
    ;; Abusing a timer like this (hopefully) guarantees that the restore runs after everything else and
    ;; the restored treemacs buffers remain visible
    (run-with-timer
     1 nil
     (lambda ()
       (-when-let- [b (get-buffer treemacs--desktop-helper-name)]
         (kill-buffer b))
       (dolist (frame (frame-list))
         (-when-let (scope-id (frame-parameter frame 'treemacs-id))
           (push (string-to-number scope-id) treemacs--taken-scopes)))
       (-when-let (persist-data (read (f-read treemacs--persist-file 'utf-8)))
         (dolist (buffer-data persist-data)
           ;; inhibit quit to quiet the error messages that crop up since this is in a timer
           (let ((inhibit-quit nil)
                 (scope-id (cdr (assoc "scope-id" buffer-data)))
                 (root     (cdr (assoc "root"     buffer-data)))
                 (point    (cdr (assoc "point"    buffer-data)))
                 (visible  (cdr (assoc "visible"  buffer-data))))
             (-when-let- [frame (--first (string= scope-id (frame-parameter it 'treemacs-id)) (frame-list))]
               (unless (-> frame (assq treemacs--buffer-access) (cdr) (buffer-live-p))
                 (save-selected-window
                   (with-selected-frame frame
                     (treemacs--init root)
                     (when (and (not (string= point "<root>"))
                                (f-exists? point))
                       (treemacs--do-follow point))
                     (hl-line-highlight)
                     (unless visible
                       (treemacs-toggle)))))))))))))

(defun treemacs--persist ()
  "Save current state, allowing it to be restored with `treemacs--restore'.
If `treemacs-never-persist' is non-nil it will instead delete any already
persisted state so it will not be loaded on the next desktop read."
  (if treemacs-never-persist
      (when (f-exists? treemacs--persist-file)
        (f-delete treemacs--persist-file))
    (-let [persist-dir (f-dirname treemacs--persist-file)]
      (unless (f-exists? persist-dir)
        (f-mkdir persist-dir))
      (let (state)
        (dolist (access-pair treemacs--buffer-access)
          (-let [(frame . buffer) access-pair]
            (when (buffer-live-p buffer)
              (with-selected-frame frame
                (with-current-buffer buffer
                  (push `(,(cons "scope-id" (frame-parameter frame 'treemacs-id))
                          ,(cons "root" (treemacs--current-root))
                          ,(cons "point" (--if-let (treemacs-current-button) (treemacs--nearest-path it) "<root>"))
                          ,(cons "visible" (if (treemacs--is-visible?) t nil)))
                        state))))))
        (f-write (pp-to-string state) 'utf-8 treemacs--persist-file)))))

(with-eval-after-load "desktop"

  ;; Desktop mode cannot take the usual approach with treemacs since buffers
  ;; are restored before frame parameters. Using just a hook won't work either since
  ;; buffers can be restored lazily.
  ;; The solution is then to bypass the usual process: treemacs buffers are not
  ;; saved at all, just a single treemacs-mode buffer called `treemacs--desktop-helper-name',
  ;; which only exists so desktop mode will load treemacs.
  ;; If desktop mode tries to restore that buffer during the eager phase `treemacs--restore'
  ;; is a noop, and will run again, properly, as part of `desktop-after-read-hook', when
  ;; frame parameters have been restored.
  ;; If this buffer is restored lazily instead frame params will have been restored,
  ;; so `treemacs--restore' will work immediately.

  (defun treemacs--create-persist-helper-buffer ()
    "Create the desktop helper on shutdown for desktop mode to load treemacs."
    (when (and (not treemacs-never-persist)
               (bound-and-true-p desktop-save-mode))
      (with-current-buffer (get-buffer-create treemacs--desktop-helper-name)
        (treemacs-mode)
        (read-only-mode -1)
        (insert "This buffer only exists so that desktop mode will load treemacs.\n")
        (insert "It will be deleted by `treemacs--restore'."))))

  (defun treemacs--desktop-handler (&rest _)
    "Fake-ish treemacs mode handler for desktop save mode.
Works if run during the lazy restoration phase, otherwise
`desktop-after-read-hook' will take care of treemacs."
    (treemacs--restore)
    ;; we need to give desktop mode a live buffer and the helper will be killed once the real restore is
    ;; run in a timer
    ;; just returning scratch does not work as whatever desktop-mode does may leave it broken as in #101
    (get-buffer-create treemacs--desktop-helper-name))

  (defun treemacs--desktop-persist-advice (&rest _)
    "Persists treemacs alongside `desktop-save'."
    (treemacs--persist))

  (declare-function treemacs--desktop-persist-advice "treemacs-persist.el")
  (declare-function treemacs--create-persist-helper-buffer "treemacs-persist.el")

  (advice-add 'desktop-save :before #'treemacs--desktop-persist-advice)
  (add-hook 'kill-emacs-hook #'treemacs--create-persist-helper-buffer)
  (add-hook 'desktop-after-read-hook #'treemacs--restore)

  (add-to-list 'desktop-buffer-mode-handlers
               '(treemacs-mode . treemacs--desktop-handler)))

(provide 'treemacs-persist)

;;; treemacs-persist.el ends here
