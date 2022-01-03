;;; treemacs.el --- A tree style file explorer package -*- lexical-binding: t -*-

;; Copyright (C) 2021 Alexander Miller

;; Author: Alexander Miller <alexanderm@web.de>
;; Package-Requires: ((emacs "26.1") (cl-lib "0.5") (dash "2.11.0") (s "1.12.0") (ace-window "0.9.0") (pfuture "1.7") (hydra "0.13.2") (ht "2.2") (cfrs "1.3.2"))
;; Homepage: https://github.com/Alexander-Miller/treemacs
;; Version: 2.9.5

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

;; A powerful and flexible file tree project explorer.

;;; Code:

(require 'dash)
(require 'treemacs-macros)
(require 'treemacs-customization)
(require 'treemacs-logging)
(require 'treemacs-themes)
(require 'treemacs-icons)
(require 'treemacs-faces)
(require 'treemacs-visuals)
(require 'treemacs-rendering)
(require 'treemacs-core-utils)
(require 'treemacs-scope)
(require 'treemacs-follow-mode)
(require 'treemacs-filewatch-mode)
(require 'treemacs-mode)
(require 'treemacs-interface)
(require 'treemacs-persistence)
(require 'treemacs-async)
(require 'treemacs-compatibility)
(require 'treemacs-workspaces)
(require 'treemacs-fringe-indicator)
(require 'treemacs-header-line)
(require 'treemacs-extensions)

(defconst treemacs-version
  (eval-when-compile
    (format "v2.9.5 (installed %s) @ Emacs %s"
            (format-time-string "%Y.%m.%d" (current-time))
            emacs-version)))

(treemacs-import-functions-from "treemacs-tag-follow-mode"
  treemacs--flatten&sort-imenu-index
  treemacs--do-follow-tag)

;;;###autoload
(defun treemacs-version ()
  "Return the `treemacs-version'."
  (interactive)
  (when (called-interactively-p 'interactive)
    (treemacs-log "%s" treemacs-version))
  treemacs-version)

;;;###autoload
(defun treemacs (&optional arg)
  "Initialise or toggle treemacs.
- If the treemacs window is visible hide it.
- If a treemacs buffer exists, but is not visible show it.
- If no treemacs buffer exists for the current frame create and show it.
- If the workspace is empty additionally ask for the root path of the first
  project to add.
- With a prefix ARG launch treemacs and force it to select a workspace"
  (interactive "P")
  (pcase (treemacs-current-visibility)
    ((guard arg)
     (treemacs-do-switch-workspace (treemacs--select-workspace-by-name))
     (treemacs-select-window))
    ('visible (delete-window (treemacs-get-local-window)))
    ('exists  (treemacs-select-window))
    ('none    (treemacs--init))))

;;;###autoload
(defun treemacs-select-directory ()
  "Select a directory to open in treemacs.
This command will open *just* the selected directory in treemacs.  If there are
other projects in the workspace they will be removed.

To *add* a project to the current workspace use
`treemacs-add-project-to-workspace' or
`treemacs-add-and-display-current-project' instead."
  (interactive)
  (treemacs-block
   (let* ((path (-> "Directory: "
                    (read-directory-name)
                    (treemacs-canonical-path)))
          (name (treemacs--filename path))
          (ws (treemacs-current-workspace)))
     (treemacs-return-if
         (and (= 1 (length (treemacs-workspace->projects ws)))
              (string= path (-> ws
                                (treemacs-workspace->projects)
                                (car)
                                (treemacs-project->path))))
       (treemacs-select-window))
     (treemacs--show-single-project path name)
     (treemacs-pulse-on-success "Now showing %s"
       (propertize path 'face 'font-lock-string-face)))))

;;;###autoload
(defun treemacs-find-file (&optional arg)
  "Find and focus the current file in the treemacs window.
If the current buffer has visits no file or with a prefix ARG ask for the
file instead.
Will show/create a treemacs buffers if it is not visible/does not exist.
For the most part only useful when `treemacs-follow-mode' is not active."
  (interactive "P")
  (-let ((path (unless arg (buffer-file-name (current-buffer))))
         (manually-entered nil))
    (unless path
      (setq manually-entered t
            path (->> (--if-let (treemacs-current-button) (treemacs--nearest-path it))
                      (read-file-name "File to find: ")
                      (treemacs-canonical-path))))
    (treemacs-unless-let (project (treemacs--find-project-for-path path))
        (treemacs-pulse-on-failure (format "%s does not fall under any project in the workspace."
                                    (propertize path 'face 'font-lock-string-face)))
      (save-selected-window
        (pcase (treemacs-current-visibility)
          ('visible (treemacs--select-visible-window))
          ('exists  (treemacs--select-not-visible-window))
          ('none    (treemacs--init)))
        (treemacs-goto-file-node path project)
        (when manually-entered (treemacs-pulse-on-success))))))

;;;###autoload
(defun treemacs-find-tag ()
  "Find and move point to the tag at point in the treemacs view.
Most likely to be useful when `treemacs-tag-follow-mode' is not active.

Will ask to change the treemacs root if the file to find is not under the
root.  If no treemacs buffer exists it will be created with the current file's
containing directory as root.  Will do nothing if the current buffer is not
visiting a file or Emacs cannot find any tags for the current file."
  (interactive)
  (treemacs-block
   (let* ((buffer (current-buffer))
          (buffer-file (when buffer (buffer-file-name buffer)))
          (project (treemacs--find-project-for-buffer))
          (index (when buffer-file (treemacs--flatten&sort-imenu-index)))
          (treemacs-window nil))
     (treemacs-error-return-if (null buffer-file)
       "Current buffer is not visiting a file.")
     (treemacs-error-return-if (null index)
       "Current buffer has no tags.")
     (treemacs-error-return-if (eq index 'unsupported)
       "Treemacs does not support following tags in this major mode.")
     (treemacs-error-return-if (null project)
       "%s does not fall under any project in the workspace."
       (propertize buffer-file 'face 'font-lock-string-face))
     (save-selected-window
       (pcase (treemacs-current-visibility)
         ('visible (treemacs--select-visible-window))
         ('exists  (treemacs--select-not-visible-window))
         ('none    (treemacs--init)))
       (setq treemacs-window (selected-window)))
     (treemacs--do-follow-tag index treemacs-window buffer-file project))))

;;;###autoload
(defun treemacs-select-window (&optional arg)
  "Select the treemacs window if it is visible.
Bring it to the foreground if it is not visible.
Initialise a new treemacs buffer as calling `treemacs' would if there is no
treemacs buffer for this frame.

In case treemacs is already selected behaviour will depend on
`treemacs-select-when-already-in-treemacs'.

A non-nil prefix ARG will also force a workspace switch."
  (interactive "P")
  (pcase (treemacs-current-visibility)
    ((guard arg)
     (treemacs-do-switch-workspace (treemacs--select-workspace-by-name))
     (treemacs-select-window))
    ('exists  (treemacs--select-not-visible-window))
    ('none    (treemacs--init))
    ('visible
     (if (not (eq treemacs--in-this-buffer t))
         (treemacs--select-visible-window)
       (pcase-exhaustive treemacs-select-when-already-in-treemacs
           ('stay
            (ignore))
           ('close
            (treemacs-quit))
           ('move-back
            (select-window (get-mru-window (selected-frame) nil :not-selected))))))))

;;;###autoload
(defun treemacs-show-changelog ()
  "Show the changelog of treemacs."
  (interactive)
  (-> "Changelog.org"
      (locate-file (list treemacs-dir))
      (find-file-existing)))

;;;###autoload
(defun treemacs-edit-workspaces ()
  "Edit your treemacs workspaces and projects as an `org-mode' file."
  (interactive)
  (require 'org)
  (require 'outline)
  (treemacs--persist)
  (switch-to-buffer (get-buffer-create treemacs--org-edit-buffer-name))
  (erase-buffer)
  (org-mode)
  (use-local-map (copy-keymap (with-no-warnings org-mode-map)))
  (local-set-key (kbd "C-c C-c") #'treemacs-finish-edit)
  (insert "#+TITLE: Edit Treemacs Workspaces & Projects\n")
  (when treemacs-show-edit-workspace-help
    (insert "# Call ~treemacs-finish-edit~ or press ~C-c C-c~ when done.\n")
    (insert "# [[https://github.com/Alexander-Miller/treemacs#conveniently-editing-your-projects-and-workspaces][Click here for detailed documentation.]]\n")
    (insert "# To cancel you can simply kill this buffer.\n\n"))
  (insert-file-contents treemacs-persist-file)
  (with-no-warnings
    (outline-show-all))
  (goto-char 0))

;;;###autoload
(defun treemacs-display-current-project-exclusively ()
  "Display the current project, and *only* the current project.
Like `treemacs-add-and-display-current-project' this will add the current
project to treemacs based on either projectile, the built-in project.el, or the
current working directory.

However the 'exclusive' part means that it will make the current project the
only project, all other projects *will be removed* from the current workspace."
  (interactive)
  (treemacs-block
   (treemacs-unless-let (root (treemacs--find-current-user-project))
       (treemacs-error-return-if (null root)
         "Not in a project.")
     (let* ((path (treemacs-canonical-path root))
            (name (treemacs--filename path))
            (ws (treemacs-current-workspace)))
       (treemacs-return-if
           (and (= 1 (length (treemacs-workspace->projects ws)))
                (treemacs-is-path path :in-workspace ws))
         (treemacs-select-window))
       (treemacs--show-single-project path name)
       (treemacs-pulse-on-success "Now showing %s"
         (propertize path 'face 'font-lock-string-face))))))

;;;###autoload
(defun treemacs-add-and-display-current-project ()
  "Open treemacs and add the current project root to the workspace.
The project is determined first by projectile (if treemacs-projectile is
installed), then by project.el, then by the current working directory.

If the project is already registered with treemacs just move point to its root.
An error message is displayed if the current buffer is not part of any project."
  (interactive)
  (treemacs-block
   (treemacs-unless-let (root (treemacs--find-current-user-project))
       (treemacs-error-return-if (null root)
         "Not in a project.")
     (let* ((path (treemacs-canonical-path root))
            (name (treemacs--filename path)))
       (unless (treemacs-current-workspace)
         (treemacs--find-workspace))
       (if (treemacs-workspace->is-empty?)
           (progn
             (treemacs-do-add-project-to-workspace path name)
             (treemacs-select-window)
             (treemacs-pulse-on-success))
         (treemacs-select-window)
         (if (treemacs-is-path path :in-workspace)
             (treemacs-goto-file-node path)
           (treemacs-add-project-to-workspace path name)))))))

(provide 'treemacs)

;;; treemacs.el ends here
