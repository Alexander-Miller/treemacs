;;; treemacs-git-commit-diff-mode.el --- A tree style file viewer package -*- lexical-binding: t -*-

;; Copyright (C) 2022 Alexander Miller

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

;; Minor mode to annotate project with the number of commits a repo is ahead
;; and/or behind its remote.

;; NOTE: This module is lazy-loaded.

;;; Code:

(require 'vc-git)
(require 'dash)
(require 'pfuture)
(require 'treemacs-customization)
(require 'treemacs-workspaces)
(require 'treemacs-annotations)

(eval-when-compile
  (require 'treemacs-macros))

(defconst treemacs--git-commit-diff.py
  (if (member "treemacs-git-commit-diff.py" (directory-files treemacs-dir))
      (treemacs-join-path treemacs-dir "treemacs-git-commit-diff.py")
    (treemacs-join-path treemacs-dir "src/scripts/treemacs-git-commit-diff.py")))

(defconst treemacs--commit-diff-ann-source "treemacs-commit-diff"
  "Annotation source name for commit diffs.")

(defun treemacs--update-git-commit-diff (project &optional buffer)
  "Update the commit diff for a single PROJECT.
Look for the PROJECT either in BUFFER or the local treemacs buffer."
  (let ((path (treemacs-project->path project))
        (buffer (or buffer (treemacs-get-local-buffer))))
    (pfuture-callback `(,treemacs-python-executable "-O" ,treemacs--git-commit-diff.py ,path)
      :directory path
      :on-success
      (when (buffer-live-p buffer)
        (-let [out (-> (pfuture-callback-output)
                       (string-trim-right)
                       (read))]
          (with-current-buffer buffer
            (if out
                (treemacs-set-annotation-suffix path out treemacs--commit-diff-ann-source)
              (treemacs-remove-annotation-suffix path treemacs--commit-diff-ann-source))
            (treemacs-apply-single-annotation path)))))))

(defun treemacs--update-commit-diff-in-every-project ()
  "Update diffs for every project in the current scope.
To be run when commt-diff-mode is activated or a treemacs buffer is created."
  (dolist (project (treemacs-workspace->projects (treemacs-current-workspace)))
    (when (vc-git-responsible-p (treemacs-project->path project))
      (treemacs--update-git-commit-diff project))))

(defun treemacs--enable-git-commit-diff-mode ()
  "Setup for `treemacs-comit-diff-mode'."
  (add-hook 'treemacs-post-project-refresh-functions #'treemacs--update-git-commit-diff)
  (add-hook 'treemacs-post-buffer-init-hook #'treemacs--update-commit-diff-in-every-project)
  (treemacs-run-in-every-buffer
   (treemacs--update-commit-diff-in-every-project)))

(defun treemacs--disable-git-commit-diff-mode ()
  "Tear-down for `treemacs-comit-diff-mode'."
  (remove-hook 'treemacs-post-project-refresh-functions #'treemacs--update-git-commit-diff)
  (remove-hook 'treemacs-post-buffer-init-hook #'treemacs--update-commit-diff-in-every-project)
  (treemacs-run-in-every-buffer
   (dolist (project (treemacs-workspace->projects (treemacs-current-workspace)))
     (-let [path (treemacs-project->path project)]
       (treemacs-remove-annotation-suffix path treemacs--commit-diff-ann-source)
       (treemacs-apply-single-annotation path)))))

;;;###autoload
(define-minor-mode treemacs-git-commit-diff-mode
  "Minor mode to display commit differences for your git-tracked projects.

When enabled treemacs will add an annotation next to every git project showing
how many commits ahead or behind your current branch is compared to its remote
counterpart.

The difference will be shown using the format `↑x ↓y', where `x' and `y' are the
numbers of commits a project is ahead or behind.  The numbers are determined
based on the output of `git status -sb'.

By default the annotation is only updated when manually updating a project with
`treemacs-refresh'.  You can install `treemacs-magit' to enable automatic
updates whenever you commit/fetch/rebase etc. in magit.

Does not require `treemacs-git-mode' to be active."
  :init-value nil
  :global     t
  :lighter    nil
  :group      'treemacs
  (if treemacs-git-commit-diff-mode
      (treemacs--enable-git-commit-diff-mode)
    (treemacs--disable-git-commit-diff-mode)))

(provide 'treemacs-git-commit-diff-mode)

;;; treemacs-git-commit-diff-mode.el ends here
