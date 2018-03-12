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
;;; Follow mode definition.

;;; Code:

(require 'hl-line)
(require 'dash)
(require 's)
(require 'f)
(require 'treemacs-branch-creation)
(require 'treemacs-structure)
(require 'treemacs-async)
(require 'treemacs-impl)
(eval-and-compile
  (require 'cl-lib)
  (require 'treemacs-macros))

(defvar treemacs--ready-to-follow nil
  "Signals to `treemacs-follow-mode' if a follow action may be run.
Must be set to nil when no following should be triggered, e.g. when the
treemacs buffer is being rebuilt or during treemacs' own window selection
functions.")

(cl-defun treemacs--do-follow (followed-file project)
  "In the treemacs buffer move point to FOLLOWED-FILE under PROJECT.

FOLLOWED-FILE: Filepath
PROJECT: `cl-struct-treemacs-project'"
  (goto-char (treemacs-project->position project))
  (-let*- [;; go back here if the search fails
           (start (point))
           ;; the parts of the path that we can try to go to until we arrive at the project root
           (dir-parts (->> project (treemacs-project->path) (length) (substring followed-file) (f-split) (cdr) (nreverse)))
           ;; the path we try to quickly move to because it's already open and thus in the shadow-index
           (goto-path (treemacs--parent followed-file))
           ;; if we try mode than this many times to grab a path location for the shadow index it means
           ;; the file we want to move to is under a *closed* project node
           (counter (length dir-parts))
           ;; manual as in to be expanded manually after we moved to the next closest node we can find
           ;; in the shadow index
           (manual-parts nil)
           ;; (root (treemacs-project->path project))
           (shadow-node nil)]
    ;; try to move as close as possible to the followed file, starting with its immediate parent
    ;; keep moving upwards in the path we move to until reaching the root of the project (counter = 0)
    ;; all the while collecting the parts of the path that beed manual expanding
    (while (and (> counter 0)
                (or (null shadow-node)
                    ;; shadow node might exist, but one of its parents might be null
                    (null (treemacs-shadow-node->position shadow-node))))
      (setq shadow-node (treemacs-get-from-shadow-index goto-path)
            goto-path (treemacs--parent goto-path)
            counter (1- counter))
      (push (pop dir-parts) manual-parts))
    (-let*- [(btn (if (= 0 counter)
                      (treemacs-project->position project)
                    (treemacs-shadow-node->position shadow-node)))
             ;; do the rest manually - at least the actual file to move to is still left in manual-parts
             (search-result (treemacs--follow-each-dir btn manual-parts))]
      (if (eq 'follow-failed search-result)
          (goto-char start)
        (treemacs--evade-image)
        (hl-line-highlight)
        (set-window-point (get-buffer-window) (point))
        (when treemacs-recenter-after-file-follow
          (treemacs-without-following
           (with-selected-window (get-buffer-window)
             (treemacs--maybe-recenter))))))))

(defun treemacs--follow-each-dir (btn dir-parts)
  "BTN Follow (goto and open) every single dir in DIR-PARTS under ROOT."
  (-let*- [(root (button-get btn :path))
           (git-future (treemacs--git-status-process-function root))
           (last-index (- (length dir-parts) 1))]
    (goto-char btn)
    ;; point is currently on the next closest dir to the followed file we could get
    ;; from the shadow index, so we expand it to keep going
    (-pcase (button-get btn :state)
      [`dir-node-closed (treemacs--expand-dir-node btn :git-future git-future)]
      [`root-node-closed (treemacs--expand-root-node btn)])
    (catch 'follow-failed
      (--each dir-parts
        (setq root (f-join root it))
        ;; for every manual part add it to the current root and find the first button below
        ;; btn whose :path is the root, expand it, keep looping
        (setq btn (first-child-btn-where btn (string= root (button-get child-btn :path))))
        (goto-char btn)
        (unless btn (throw 'follow-failed 'follow-failed))
        ;; don't open dir at the very end of the list since we only want to put
        ;; point in its line
        (when (and (eq 'dir-node-closed (button-get btn :state))
                   (< it-index last-index))
          (treemacs--expand-dir-node btn :git-future git-future))))))

(defun treemacs--follow ()
  "Move point to the current file in the treemacs buffer.
Expand directories if needed. Do nothing if current file does not exist in the
file system or is not below current treemacs root or if the treemacs buffer is
not visible."
  ;; Treemacs selecting files with `ace-window' results in a large amount of
  ;; window selections, so we should be breaking out as soon as possbile
  (when treemacs--ready-to-follow
    (treemacs-without-following
     (let* ((treemacs-window (treemacs--is-visible?))
            (current-buffer  (current-buffer))
            (current-file    (buffer-file-name current-buffer)))
       (when (and treemacs-window
                  current-file
                  (not (s-starts-with? treemacs--buffer-name-prefix (buffer-name current-buffer)))
                  (f-exists? current-file))
         (-when-let- [project-for-file (treemacs--find-project-for-buffer)]
           (with-current-buffer (window-buffer treemacs-window)
             (-let [selected-file (--if-let (treemacs-current-button)
                                      (treemacs--nearest-path it)
                                    (treemacs-project->path project-for-file))]
               (unless (string= selected-file current-file)
                 (treemacs--do-follow current-file project-for-file))))))))))

;; this is only to stop the compiler from complaining about unknown functions
(with-eval-after-load 'which-key
  (declare-function which-key--show-popup "which-key")
  (declare-function which-key--hide-popup "which-key"))

(defun treemacs--select-window-advice (&rest _)
  "Advice function for `treemacs-follow-mode'.
Ignores the original arguments of `select-window' and directly calls
`treemacs--follow'."
  (treemacs--follow))

(defun treemacs--follow-compatibility-advice (original-func &rest args)
  "Make ORIGINAL-FUNC compatible with `treemacs-follow-mode'.
Do so by running it and its ARGS through `treemacs-without-following'."
  (treemacs-without-following
   (apply original-func args)))

(defsubst treemacs--setup-follow-mode ()
  "Setup all the advice needed for `treemacs-follow-mode'."
  (advice-add 'select-window :after #'treemacs--select-window-advice)
  ;; which key compatibility
  (progn
    (when (fboundp 'which-key--show-popup)
      (advice-add #'which-key--show-popup :around #'treemacs--follow-compatibility-advice))
    (when (fboundp 'which-key--hide-popup)
      (advice-add #'which-key--hide-popup :around #'treemacs--follow-compatibility-advice)))
  ;; winum compatibility
  (when (fboundp 'winum--update)
    (advice-add #'winum--update :around #'treemacs--follow-compatibility-advice))
  (treemacs--follow))

(defsubst treemacs--tear-down-follow-mode ()
  "Remove all the advice added by `treemacs--setup-follow-mode'."
  (advice-remove 'select-window 'treemacs--select-window-advice)
  ;; which key compatibility
  (progn
    (when (advice-member-p #'treemacs--follow-compatibility-advice #'which-key--show-popup)
      (advice-remove #'which-key--show-popup #'treemacs--follow-compatibility-advice))
    (when (advice-member-p #'treemacs--follow-compatibility-advice #'which-key--hide-popup)
      (advice-remove #'which-key--hide-popup #'treemacs--follow-compatibility-advice)))
  ;; winum compatibility
  (when (and (fboundp 'winum--update)
             (advice-member-p #'treemacs--follow-compatibility-advice #'winum--update))
    (advice-remove #'winum--update #'treemacs--follow-compatibility-advice)))

(define-minor-mode treemacs-follow-mode
  "Toggle `treemacs-follow-mode'.
When enabled treemacs will keep track of and focus the currently selected
buffer's file. This only applies if the file is within the treemacs root
directory.
This functionality can also be manually invoked with `treemacs-find-file'."
  :init-value nil
  :global     t
  :lighter    nil
  (if treemacs-follow-mode
      (treemacs--setup-follow-mode)
    (treemacs--tear-down-follow-mode)))

(provide 'treemacs-follow-mode)

;;; treemacs-follow-mode.el ends here
