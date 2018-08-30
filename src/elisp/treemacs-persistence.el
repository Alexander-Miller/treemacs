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
;;; Persistence of treemacs' workspaces into an org-mode compatible file.

;;; Code:

(require 'f)
(require 's)
(require 'dash)
(require 'rx)
(require 'cl-lib)
(require 'treemacs-workspaces)
(require 'treemacs-customization)
(eval-when-compile
  (require 'treemacs-macros))

;; TODO: inline when the backwards compatible parts in `treemacs--restore' are removed
(defvar treemacs--persist-kv-regex
  (rx bol
      " - "
      (1+ (or (syntax word) (syntax symbol) (syntax punctuation)))
      " :: "
      (1+ (or (syntax word) (syntax symbol) (syntax punctuation) space))
      eol)
  "The regular expression to match org's \"key :: value\" lines.")

;; Should probably be replaced with `generator.el', but we're maintaining Emacs 25
;; compatibility while Emacs 26 is the last stable release.
(treemacs--defstruct treemacs-iter list)

(defsubst treemacs-iter->next! (iter)
  "Get the next element of iterator ITER.

ITER: Treemacs-Iter struct."
  (-let [(head . tail) (treemacs-iter->list iter)]
    (setf (treemacs-iter->list iter) tail)
    head))

(defsubst treemacs-iter->peek (iter)
  "Peek at the first element of ITER.

ITER: Treemacs-Iter struct."
  (or (car (treemacs-iter->list iter))
      ;; we still need something to make the `s-matches?' calls work
      "__EMPTY__"))

(defsubst treemacs--should-not-run-persistence? ()
  "No saving and loading in noninteractive and CI environments."
  (or noninteractive (getenv "CI")))

(defun treemacs--read-workspaces (iter)
  "Read a list of workspaces from the lines in ITER.

ITER: Treemacs-Iter struct."
  (let ((workspaces nil)
        (workspace-regex (rx bol "* " (1+ any) eol)))
    (while (s-matches? workspace-regex (treemacs-iter->peek iter))
      (-let [workspace (make-treemacs-workspace)]
        (setf (treemacs-workspace->name workspace)
              (substring (treemacs-iter->next! iter) 2)
              (treemacs-workspace->projects workspace)
              (treemacs--read-projects iter))
        (push workspace workspaces)))
    (nreverse workspaces)))

(defun treemacs--read-projects (iter)
  "Read a list of projects from ITER until another section is found.

ITER: Treemacs-Iter struct"
  (-let ((projects nil)
         (project-regex (rx bol "** " (1+ any) eol)))
    (while (s-matches? project-regex (treemacs-iter->peek iter))
      (let ((kv-lines nil)
            (project (make-treemacs-project)))
        (setf (treemacs-project->name project)
              (substring (treemacs-iter->next! iter) 3))
        (while (s-matches? treemacs--persist-kv-regex (treemacs-iter->peek iter))
          (push (treemacs-iter->next! iter) kv-lines))
        (if (null kv-lines)
            (treemacs-log "Project %s has no path and will be ignored."
                          (propertize (treemacs-project->name project)
                                      'face 'font-lock-type-face))
          (dolist (kv-line kv-lines)
            (-let [(key val) (s-split " :: " kv-line)]
              (pcase key
                (" - path"
                 (setf (treemacs-project->path project) val))
                (_
                 (treemacs-log "Encountered unknown project key-value in line [%s]" kv-line)))))
          (if (-> project (treemacs-project->path) (file-exists-p) (not))
              (treemacs-log "The location of project %s at %s cannot be read, the project will be ignored."
                            (propertize (treemacs-project->name project) 'face 'font-lock-type-face)
                            (propertize (treemacs-project->path project) 'face 'font-lock-string-face))
            (push project projects)))))
    (nreverse projects)))

(defun treemacs--persist ()
  "Persist treemacs' state in `treemacs-persist-file'."
  (unless (treemacs--should-not-run-persistence?)
    (unless (f-exists? treemacs-persist-file)
      (f-mkdir (f-dirname treemacs-persist-file))
      (f-touch treemacs-persist-file))
    (condition-case e
        (let ((txt nil)
              (buffer nil)
              (no-kill nil))
          (--if-let (get-file-buffer treemacs-persist-file)
              (setq buffer it)
            (setq buffer (find-file-noselect treemacs-persist-file :no-warn :literally)
                  no-kill t))
          (with-current-buffer buffer
            (dolist (ws (treemacs-workspaces))
              (push (format "* %s\n" (treemacs-workspace->name ws)) txt)
              (dolist (pr (treemacs-workspace->projects ws))
                (push (format "** %s\n" (treemacs-project->name pr)) txt)
                (push (format " - path :: %s\n" (treemacs-project->path pr)) txt)))
            (delete-region (point-min) (point-max))
            (insert (apply #'concat (nreverse txt)))
            (save-buffer)
            (unless no-kill (kill-buffer))))
      (error (treemacs-log "Error '%s' when persisting workspace." e)))))

(defun treemacs--restore ()
  "Restore treemacs' state from `treemacs-persist-file'."
  (unless (treemacs--should-not-run-persistence?)
    (condition-case e
        (when (file-exists-p treemacs-persist-file)
          (-let [str-list (--reject (or (string= it "")
                                        (s-starts-with? "#+STARTUP:" it))
                                    (-> treemacs-persist-file (f-read) (s-lines)))]
            (if (and (>= (length str-list) 3)
                     (--any? (s-matches? treemacs--persist-kv-regex it) str-list))
                (setq treemacs--workspaces (treemacs--read-workspaces (make-treemacs-iter :list str-list)))
              ;; read state based on the system used before the current ini-format
              ;; should be removed after enough time has passed
              (-when-let (workspace (-> treemacs-persist-file (f-read 'utf-8) (read) (car)))
                (dolist (project (treemacs-workspace->projects workspace))
                  (unless (-> project (treemacs-project->path) (f-exists?))
                    (treemacs-log (format "Project at %s does not exist and was removed from the workspace."
                                          (propertize (treemacs-project->path project) 'face 'font-lock-string-face)))
                    (setf (treemacs-workspace->projects workspace)
                          (delete project (treemacs-workspace->projects workspace)))))
                (setq treemacs--workspaces (list workspace))))))
      (error (treemacs-log "Error '%s' when loading the persisted workspace." e)))))

(add-hook 'kill-emacs-hook #'treemacs--persist)

(provide 'treemacs-persistence)

;;; treemacs-persistence.el ends here
