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
  (require 'inline)
  (require 'treemacs-macros))

(defconst treemacs--org-edit-buffer-name "*Edit Treemacs Workspaces*"
  "The name of the buffer used to edit treemacs' workspace.")

(defconst treemacs--persist-kv-regex
  (rx bol
      (? " ")
      "- "
      (1+ (or (syntax word) (syntax symbol) (syntax punctuation)))
      " :: "
      (1+ (or (syntax word) (syntax symbol) (syntax punctuation) space))
      eol)
  "The regular expression to match org's \"key :: value\" lines.")

(defconst treemacs--persist-workspace-name-regex
  (rx bol "* " (1+ any) eol)
  "The regular expression to match lines with workspace names.")

(defconst treemacs--persist-project-name-regex
  (rx bol "** " (1+ any) eol)
  "The regular expression to match lines with projects names.")

(treemacs--defstruct treemacs-iter list)

(define-inline treemacs-iter->next! (iter)
  "Get the next element of iterator ITER.

ITER: Treemacs-Iter struct."
  (inline-letevals (iter)
    (inline-quote
     (let ((head (car (treemacs-iter->list ,iter)))
           (tail (cdr (treemacs-iter->list ,iter))))
       (setf (treemacs-iter->list ,iter) tail)
       head))))

(define-inline treemacs-iter->peek (iter)
  "Peek at the first element of ITER.

ITER: Treemacs-Iter struct."
  (declare (side-effect-free t))
  (inline-letevals (iter)
    (inline-quote
     (or (car (treemacs-iter->list ,iter))
         ;; we still need something to make the `s-matches?' calls work
         "__EMPTY__"))))

(define-inline treemacs--should-not-run-persistence? ()
  "No saving and loading in noninteractive and CI environments."
  (inline-quote (or noninteractive (getenv "CI"))))

(defun treemacs--read-workspaces (iter)
  "Read a list of workspaces from the lines in ITER.

ITER: Treemacs-Iter struct."
  (let (workspaces)
    (while (s-matches? treemacs--persist-workspace-name-regex (treemacs-iter->peek iter))
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
  (let (projects)
    (while (s-matches? treemacs--persist-project-name-regex (treemacs-iter->peek iter))
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
              (pcase (s-trim key)
                ("- path"
                 (setf (treemacs-project->path project) (treemacs--canonical-path val)))
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
            (setq buffer (find-file-noselect treemacs-persist-file :no-warn)
                  no-kill t))
          (with-current-buffer buffer
            (dolist (ws (treemacs-workspaces))
              (push (format "* %s\n" (treemacs-workspace->name ws)) txt)
              (dolist (pr (treemacs-workspace->projects ws))
                (push (format "** %s\n" (treemacs-project->name pr)) txt)
                (push (format " - path :: %s\n" (treemacs-project->path pr)) txt)))
            (delete-region (point-min) (point-max))
            (insert (apply #'concat (nreverse txt)))
            (-let [inhibit-message t] (save-buffer))
            (unless no-kill (kill-buffer))))
      (error (treemacs-log "Error '%s' when persisting workspace." e)))))

(defun treemacs--read-persist-lines (&optional txt)
  "Read the relevant lines from given TXT or `treemacs-persist-file'."
  (-when-let (str (-some-> (or txt (when (file-exists-p treemacs-persist-file)
                                     (f-read treemacs-persist-file)))
                           (s-trim)
                           (s-lines)))
    (--filter (or (s-matches? treemacs--persist-kv-regex it)
                  (s-matches? treemacs--persist-project-name-regex it)
                  (s-matches? treemacs--persist-workspace-name-regex it))
              str)))

(cl-defun treemacs--validate-persist-lines (lines &optional (context :start))
  "Recursively verify the make-up of the given LINES, based on their CONTEXT.
Lines must start with a workspace name, followed by a project name, followed by
the project's path property, followed by either the next project or the next
workspace.

LINES: List of Strings
CONTEXT: Keyword"
  (cl-block body
    (treemacs-unless-let (line (car lines))
        (pcase context
          (:property
           (cl-return-from body 'success))
          (:start
           (cl-return-from body
             (list 'error "Input is empty")))
          (_
           (cl-return-from body
             (list 'error "Cannot end with a project or workspace name"))))
      (pcase context
        (:start
         (treemacs-return-if (not (s-matches? treemacs--persist-workspace-name-regex line))
           `(error ,(format "First line '%s' is not a workspace name" line)))
         (treemacs--validate-persist-lines (cdr lines) :workspace))
        (:workspace
         (treemacs-return-if (not (s-matches? treemacs--persist-project-name-regex line))
           `(error ,(format "Line '%s' after workspace name is not a project name" line)))
         (treemacs--validate-persist-lines (cdr lines) :project))
        (:project
         (treemacs-return-if (not (s-matches? treemacs--persist-kv-regex line))
           `(error ,(format "Line '%s' after project name is not a path declaration" line)))
         (-let [path (cadr (s-split " :: " line))]
           (treemacs-return-if (and (string= treemacs--org-edit-buffer-name (buffer-name))
                                    (not (file-exists-p path)))
             `(error ,(format "File '%s' does not exist" path)))
           (treemacs--validate-persist-lines (cdr lines) :property)))
        (:property
         (let ((line-is-workspace-name (s-matches? treemacs--persist-workspace-name-regex line))
               (line-is-project-name   (s-matches? treemacs--persist-project-name-regex line)))
           (cond
            (line-is-workspace-name
             (treemacs--validate-persist-lines (cdr lines) :workspace))
            (line-is-project-name
             (treemacs--validate-persist-lines (cdr lines) :project))
            (t
             (treemacs-return-if (-none? #'identity (list line-is-workspace-name line-is-project-name))
               `(error ,(format "Line '%s' after property must be the name of the next project or workspace" line)))))))))))

(defun treemacs--restore ()
  "Restore treemacs' state from `treemacs-persist-file'."
  (unless (treemacs--should-not-run-persistence?)
    (condition-case e
        (-when-let (lines (treemacs--read-persist-lines))
          (pcase (treemacs--validate-persist-lines lines)
            ('success
             (setf treemacs--workspaces (treemacs--read-workspaces (make-treemacs-iter :list lines))
                   (treemacs-current-workspace) (car treemacs--workspaces)))
            (`(error ,error-msg)
             (treemacs-log "Could not restore saved state, found the following error:\n%s." error-msg))))
      (error (treemacs-log "Error '%s' when loading the persisted workspace." e)))))

(add-hook 'kill-emacs-hook #'treemacs--persist)

(provide 'treemacs-persistence)

;;; treemacs-persistence.el ends here
