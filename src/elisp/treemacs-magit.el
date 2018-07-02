;;; treemacs-magit.el --- Magit integration for treemacs -*- lexical-binding: t -*-

;; Copyright (C) 2018 Alexander Miller

;; Author: Alexander Miller <alexanderm@web.de>
;; Package-Requires: ((treemacs "2.1") (magit "2.13.0") (with-editor "2.7.3"))
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
;;; Closing the gaps for filewatch- and git-modes in conjunction with magit.
;;; Specifically this package will advice and hook into magit so as to
;;; artificially produce filewatch events for changes that treemacs would
;;; otherwise not catch, nameley the committing and (un)staging of files.

;;; Code:

(require 'treemacs)
(require 'magit)
(require 'with-editor)

(defvar treemacs-magit--commit-directory nil
  "In case of a commit through `with-editor' the git root is saved here.")

(defsubst treemacs-magit--change-path (path)
  "Create a fake updated path based on PATH.
The calls to `magit-toplevel' that we use to determine the updated path after a
commit return the repository root - most likely also the root of the treemacs
project -  which is then marked as changed. This cannot quite work since
treemacs expects to update the parent of the file that was changed, so we
pretend that a fake file was changed."
  (concat (treemacs--canonical-path path) "/TREEMACS-MAGIT"))

(defun treemacs-magit--pre-finish-hook ()
  "Hook to save the git root when a commit is about to run.
The root is saved in `treemacs-magit--commit-directory' and picked up by
`treemacs-magit--post-finish-hook' after the commit has actually run to set an
appropriate refresh flag."
  (when (string= (buffer-name) "COMMIT_EDITMSG")
    (setq treemacs-magit--commit-directory (treemacs-magit--change-path (magit-toplevel)))))

(defun treemacs-magit--post-finish-hook ()
  "Ensures a project is refreshed after a commit through `with-editor'.
The project's root must previously have been set by
`treemacs-magit--pre-finish-hook'."
  (when treemacs-magit--commit-directory
    (treemacs--filewatch-callback `(:treemacs-magit-commit changed ,treemacs-magit--commit-directory))
    (setq treemacs-magit--commit-directory nil)))

(defun treemacs-magit--post-commit-advice (&rest _)
  "Advice for commit cases not covered by `with-editor' hooks.
In practive this means functions like `magit-commit-extend' which do not call
`with-editor' to edit a commit message, but run the commit immediately."
  (treemacs--filewatch-callback `(treemacs-magit-commit-event changed ,(treemacs-magit--change-path (magit-toplevel)))))

(defun treemacs-magit--process-stage-event (root files)
  "Push filewatch events under ROOT for FILES."
  (dolist (file files)
    (treemacs--filewatch-callback `(treemacs-magit-stage-event changed ,(concat root file)))))

(defun treemacs-magit--post-stage-advice (&rest _)
  "Advice run after every stage command of magit."
  (run-with-idle-timer 2 nil #'treemacs-magit--process-stage-event
                       (magit-toplevel) (magit-staged-files)))

(defun treemacs-magit--post-unstage-advice (&rest _)
  "Advice run after every unstage command of magit."
  (run-with-idle-timer 2 nil #'treemacs-magit--process-stage-event
                       (magit-toplevel) (magit-unstaged-files)))

(defun treemacs-magit--setup-or-teardown ()
  "Set up or tear down the bridge between treemacs and magit.
Which branch runs depends on whether `treemacs-filewatch' and
`treemacs-git-mode'are both enabled or not."
  (if (and treemacs-git-mode treemacs-filewatch-mode)
      (treemacs-magit--setup)
    (treemacs-magit--teardown)))

(defun treemacs-magit--setup ()
  "Set up the bridge between treemacs and magit."
  (add-hook 'with-editor-pre-finish-hook #'treemacs-magit--pre-finish-hook)
  (add-hook 'with-editor-post-finish-hook #'treemacs-magit--post-finish-hook)

  (advice-add #'magit-commit-extend        :after #'treemacs-magit--post-commit-advice)
  (advice-add #'magit-commit-fixup         :after #'treemacs-magit--post-commit-advice)
  (advice-add #'magit-commit-instant-fixup :after #'treemacs-magit--post-commit-advice)

  (advice-add #'magit-stage          :after #'treemacs-magit--post-stage-advice)
  (advice-add #'magit-stage-file     :after #'treemacs-magit--post-stage-advice)
  (advice-add #'magit-stage-modified :after #'treemacs-magit--post-stage-advice)

  (advice-add #'magit-unstage      :after #'treemacs-magit--post-unstage-advice)
  (advice-add #'magit-unstage-file :after #'treemacs-magit--post-unstage-advice)
  (advice-add #'magit-unstage-all  :after #'treemacs-magit--post-unstage-advice))

(defun treemacs-magit--teardown ()
  "Tear down the bridge between treemacs and magit."
  (remove-hook 'with-editor-pre-finish-hook  #'treemacs-magit--pre-finish-hook)
  (remove-hook 'with-editor-post-finish-hook #'treemacs-magit--post-finish-hook)

  (advice-remove #'magit-commit-extend        #'treemacs-magit--post-commit-advice)
  (advice-remove #'magit-commit-fixup         #'treemacs-magit--post-commit-advice)
  (advice-remove #'magit-commit-instant-fixup #'treemacs-magit--post-commit-advice)

  (advice-remove #'magit-stage          #'treemacs-magit--post-stage-advice)
  (advice-remove #'magit-stage-file     #'treemacs-magit--post-stage-advice)
  (advice-remove #'magit-stage-modified #'treemacs-magit--post-stage-advice)

  (advice-remove #'magit-unstage      #'treemacs-magit--post-unstage-advice)
  (advice-remove #'magit-unstage-file #'treemacs-magit--post-unstage-advice)
  (advice-remove #'magit-unstage-all  #'treemacs-magit--post-unstage-advice))

(unless (featurep 'treemacs-magit)
  (treemacs-magit--setup-or-teardown))

(provide 'treemacs-magit)

;;; treemacs-magit.el ends here
