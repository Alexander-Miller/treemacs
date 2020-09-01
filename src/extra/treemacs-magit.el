;;; treemacs-magit.el --- Magit integration for treemacs -*- lexical-binding: t -*-

;; Copyright (C) 2020 Alexander Miller

;; Author: Alexander Miller <alexanderm@web.de>
;; Package-Requires: ((emacs "25.2") (treemacs "0.0") (pfuture "1.3" ) (magit "2.90.0"))
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
;;; Closing the gaps for filewatch- and git-modes in conjunction with magit.
;;; Specifically this package will hook into magit so as to artificially
;;; produce filewatch events for changes that treemacs would otherwise
;;; not catch, nameley the committing and (un)staging of files.

;;; Code:

(require 'treemacs)
(require 'magit)
(require 'pfuture)
(require 'seq)

;; no need for dash for a single when-let
(eval-when-compile
  (when (version< emacs-version "26")
    (defalias 'if-let* #'if-let)
    (defalias 'when-let* #'when-let)))

(defvar treemacs-magit--timers nil
  "Cached list of roots an update is scheduled for.")

(defun treemacs-magit--schedule-update ()
  "Schedule an update to potentially run after 3 seconds of idle time.
In order for the update to fully run several conditions must be met:
 * A timer for an update for the given directory must not already exist
   (see `treemacs-magit--timers')
 * The directory must be part of a treemacs workspace, and
 * The project must not be set for refresh already."
  (when treemacs-git-mode
    (let ((magit-root (treemacs--canonical-path (magit-toplevel))))
      (unless (member magit-root treemacs-magit--timers)
        (push magit-root treemacs-magit--timers)
        (run-with-idle-timer
         3 nil
         (lambda ()
           (unwind-protect
               (pcase treemacs-git-mode
                 ('simple
                  (treemacs-magit--simple-git-mode-update magit-root))
                 ((or 'extended 'deferred)
                  (treemacs-magit--extended-git-mode-update magit-root)))
             (setf treemacs-magit--timers (delete magit-root treemacs-magit--timers)))))))))

(defun treemacs-magit--simple-git-mode-update (magit-root)
  "Update the project at the given MAGIT-ROOT.
Without the parsing ability of extended git-mode this update uses
filewatch-mode's mechanics to update the entire project."
  (treemacs-run-in-every-buffer
   (when-let* ((project (treemacs--find-project-for-path magit-root)))
     (let* ((project-root (treemacs-project->path project))
            (dom-node (treemacs-find-in-dom project-root)))
       (when (and dom-node
                  (null (treemacs-dom-node->refresh-flag dom-node)))
         (treemacs--set-refresh-flags project-root 'magit-refresh project-root))))))

(defun treemacs-magit--extended-git-mode-update (magit-root)
  "Update the project at the given MAGIT-ROOT.
This runs due to a commit or stash action, so we know that no files have
actually been added or deleted.  This allows us to forego rebuilding the entire
project structure just to be sure we caught everything.  Instead we grab the
current git status and just go through the lines as they are right now."
  ;; we run a single git process to update every buffer, so we need to gather
  ;; the visible dirs in every buffer
  ;; this collection may contain duplicates, but they are removed in python
  (-let [visible-dirs nil]
    (treemacs-run-in-every-buffer
     (dolist (dir (-some->> magit-root
                            (treemacs-find-in-dom)
                            (treemacs-dom-node->children)
                            (-map #'treemacs-dom-node->key)))
       (push dir visible-dirs)))
    (pfuture-callback `(,treemacs-python-executable
                        "-O" "-S"
                        ,treemacs--git-status.py
                        ,magit-root
                        ,(number-to-string treemacs-max-git-entries)
                        ,treemacs-git-command-pipe
                        ,@visible-dirs)
      :directory magit-root
      :on-success
      (progn
        (ignore status)
        (treemacs-magit--update-callback magit-root pfuture-buffer)))))

(defun treemacs-magit--update-callback (magit-root pfuture-buffer)
  "Run the update as a pfuture callback.
Will update nodes under MAGIT-ROOT with output in PFUTURE-BUFFER."
  (let ((ht (read (pfuture-output-from-buffer pfuture-buffer))))
    (treemacs-run-in-every-buffer
     (let ((dom-node (or (treemacs-find-in-dom magit-root)
                         (when-let* ((project
                                      (seq-find
                                       (lambda (pr) (treemacs-is-path (treemacs-project->path pr) :in magit-root))
                                       (treemacs-workspace->projects (treemacs-current-workspace)))))
                           (treemacs-find-in-dom (treemacs-project->path project))))))
       (when (and dom-node
                  (treemacs-dom-node->position dom-node)
                  (treemacs-is-node-expanded? (treemacs-dom-node->position dom-node))
                  (null (treemacs-dom-node->refresh-flag dom-node)))
         (save-excursion
           (goto-char (treemacs-dom-node->position dom-node))
           (forward-line 1)
           (let* ((node (treemacs-node-at-point))
                  (start-depth (-some-> node (treemacs-button-get :depth)))
                  (curr-depth start-depth)
                  (path (-some-> node (treemacs-button-get :key))))
             (treemacs-with-writable-buffer
              (while (and node
                          (file-exists-p path)
                          (>= curr-depth start-depth))
                (put-text-property (treemacs-button-start node) (treemacs-button-end node) 'face
                                   (treemacs--get-node-face
                                    path ht
                                    (if (memq (treemacs-button-get node :state)
                                              '(file-node-open file-node-closed))
                                        'treemacs-git-unmodified-face
                                      'treemacs-directory-face)))
                (forward-line 1)
                (if (eobp)
                    (setf node nil)
                  (setf node (treemacs-node-at-point)
                        path (-some-> node (treemacs-button-get :path))
                        curr-depth (-some-> node (treemacs-button-get :depth)))))))))))))

(unless (featurep 'treemacs-magit)
  (add-hook 'magit-post-commit-hook      #'treemacs-magit--schedule-update)
  (add-hook 'git-commit-post-finish-hook #'treemacs-magit--schedule-update)
  (add-hook 'magit-post-stage-hook       #'treemacs-magit--schedule-update)
  (add-hook 'magit-post-unstage-hook     #'treemacs-magit--schedule-update))

(provide 'treemacs-magit)

;;; treemacs-magit.el ends here
