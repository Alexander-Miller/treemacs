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
;;; General persistence and desktop-save-mode integration.

;;; Code:

(require 'hl-line)
(require 'dash)
(require 'f)
(require 's)
(require 'treemacs-customization)
(require 'treemacs-impl)
(require 'treemacs-follow-mode)

(declare-function treemacs-mode "treemacs-mode")

(defconst treemacs--persist-file (f-join user-emacs-directory ".cache" "treemacs-persist")
  "File treemacs uses to persist its current state.")

(defconst treemacs--desktop-helper-name "*Desktop Treemacs Helper*")

;;;###autoload
(defun treemacs--restore ()
  "Restore the entire treemacs state saved by `treeemacs--persist'."
  ;; condition is true when we're running in eager restoration and the frameset is not yet restored
  ;; in this case this function will be run again, with restored frame parameters, in `desktop-after-read-hook'
  (unless (--all? (null (frame-parameter it 'treemacs-id)) (frame-list))
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
                 (root     (cdr (assoc "root" buffer-data)))
                 (point    (cdr (assoc "point" buffer-data))))
             (-when-let (frame (--first (string= scope-id (frame-parameter it 'treemacs-id)) (frame-list)))
               (unless (-> frame (assq treemacs--buffer-access) (cdr) (buffer-live-p))
                 (save-selected-window
                   (with-selected-frame frame
                     (push (cons frame (current-buffer)) treemacs--buffer-access)
                     (treemacs--init root)
                     (bury-buffer (current-buffer))
                     (when (and (not (string= point "<root>"))
                                (f-exists? point))
                       (treemacs--do-follow point))
                     (hl-line-highlight))))))))))))

(defun treemacs--persist ()
  "Save current state, allowing it to be restored with `treemacs--restore'."
  (let ((persist-dir (f-dirname treemacs--persist-file)))
    (unless (f-exists? persist-dir)
      (f-mkdir persist-dir)))
  (let (state)
    (dolist (access-pair treemacs--buffer-access)
      (-let [(frame . buffer) access-pair]
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (push `(,(cons "scope-id" (frame-parameter frame 'treemacs-id))
                    ,(cons "root" (treemacs--current-root))
                    ,(cons "point" (-if-let (b (treemacs--current-button)) (treemacs--nearest-path b) "<root>")))
                  state)))))
    (f-write (pp-to-string state) 'utf-8 treemacs--persist-file)
    (with-current-buffer (get-buffer-create treemacs--desktop-helper-name)
      (treemacs-mode)
      (read-only-mode -1)
      (insert "This buffer only exists so that desktop mode will load treemacs.\n")
      (insert "It will be deleted by `treemacs--restore'."))))

(defun treemacs--maybe-persist ()
  "Hook function to save treemacs state when conditions for it are met.
Persistence takes place when the treemacs buffer is killed or when Emacs shuts
down and `treemacs-never-persist' is not t and a state saving mode like
desktop save mode is on."
  (when (and (not treemacs-never-persist)
             (or desktop-save-mode
                 (and (bound-and-true-p persp-auto-save-opt)
                      (not (eq 0 persp-auto-save-opt)))))
    (treemacs--persist)))

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

  ;; Desktop mode cannot take the usual approach with treemacs since when buffers
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

  (defun treemacs--desktop-handler (&rest _)
    "Fake-ish treemacs mode handler for desktop save mode.
Works if run during the lazy restoration phase, otherwise
`desktop-after-read-hook' will take care of treemacs
Will always return the scratch buffer to make `desktop-mode` think all is well."
    (treemacs--restore)
    (get-buffer-create "*scratch*"))

  (defun treemacs--desktop-persist-advice (&rest _)
    "Persists treemacs alongside `desktop-save'."
    (treemacs--persist))

  (advice-add 'desktop-save :before (with-no-warnings #'treemacs--desktop-persist-advice))
  (add-hook 'desktop-after-read-hook #'treemacs--restore)

  (when (boundp 'desktop-buffers-not-to-save)
    (unless (s-contains? treemacs--buffer-name-prefix desktop-buffers-not-to-save)
      (if desktop-buffers-not-to-save
          (setq desktop-buffers-not-to-save
                (concat "\\(" desktop-buffers-not-to-save "\\|" (rx bol (eval treemacs--buffer-name-prefix) (0+ any)) "\\)"))
        (setq desktop-buffers-not-to-save (rx bol (eval treemacs--buffer-name-prefix) (0+ any))))))

  (add-to-list 'desktop-buffer-mode-handlers
               '(treemacs-mode . treemacs--desktop-handler)))

(provide 'treemacs-persist)

;;; treemacs-persist.el ends here
