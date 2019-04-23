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

(defconst treemacs--last-error-persist-file
  (f-join user-emacs-directory ".cache" "treemacs-persist-at-last-error")
  "File that stores the treemacs state as it was during the last load error.")

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

(define-inline treemacs-iter->next! (self)
  "Get the next element of iterator SELF.

SELF: Treemacs-Iter struct."
  (inline-letevals (self)
    (inline-quote
     (let ((head (car (treemacs-iter->list ,self)))
           (tail (cdr (treemacs-iter->list ,self))))
       (setf (treemacs-iter->list ,self) tail)
       head))))

(define-inline treemacs-iter->peek (self)
  "Peek at the first element of SELF.

SELF: Treemacs-Iter struct."
  (declare (side-effect-free t))
  (inline-letevals (self)
    (inline-quote
     (or (car (treemacs-iter->list ,self))
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
          (let ((action 'retry))
            (while (eq action 'retry)
              (setf (treemacs-project->path-status project)
                    (-> (treemacs-project->path project)
                        (treemacs--get-path-status)))
              (setq action
                    (cond
                     ((not (treemacs-project->is-unreadable? project))
                      'keep)
                     ((eq treemacs-missing-project-action 'ask)
                      (let ((completions
                             '(("Keep the project in the project list" . keep)
                               ("Retry" . retry)
                               ("Remove the project from the project list" . remove))))
                        (cdr (assoc (completing-read
                                     (format "Project %s at %s cannot be read."
                                             (treemacs-project->name project)
                                             (treemacs-project->path project))
                                     completions nil t)
                                    completions))))
                     (treemacs-missing-project-action))))
            (if (eq action 'remove)
                (treemacs-log "The location of project %s at %s cannot be read. Project was removed from the project list."
                              (propertize (treemacs-project->name project) 'face 'font-lock-type-face)
                              (propertize (treemacs-project->path project) 'face 'font-lock-string-face))
              (push project projects))))))
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
              (setq buffer it
                    no-kill t)
            (setq buffer (find-file-noselect treemacs-persist-file :no-warn)
                  desktop-save-buffer nil))
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
  "Read the relevant lines from given TXT or `treemacs-persist-file'.
Will read all lines, except those that start with # or contain only whitespace."
  (-when-let (lines (-some-> (or txt (when (file-exists-p treemacs-persist-file)
                                       (f-read treemacs-persist-file)))
                             (s-trim)
                             (s-lines)))
    (--reject (or (s-blank-str? it)
                  (s-starts-with? "#" it))
              lines)))

(cl-defun treemacs--validate-persist-lines (lines &optional (context :start) (prev nil))
  "Recursively verify the make-up of the given LINES, based on their CONTEXT.
Lines must start with a workspace name, followed by a project name, followed by
the project's path property, followed by either the next project or the next
workspace. The previously looked at line type is given by CONTEXT.

A successful validation returns just the symbol 'success, in case of an error a
list of 3 items is returned: the symbol 'error, the exact line where the error
happened, and the error message. In some circumstances (for example when a
project is missing a path property) it makes sense to display the error not in
the currently looked at line, but the one above, which is why the previously
looked at line PREV is given as well.

LINES: List of Strings
CONTEXT: Keyword"
  (treemacs-block
   (cl-labels ((as-warning (txt) (propertize txt 'face 'warning)))
     (treemacs-unless-let (line (car lines))
         (pcase context
           (:property
            (treemacs-return
             'success))
           (:start
            (treemacs-return
             (list 'error :start (as-warning "Input is empty"))))
           (_
            (treemacs-return
             (list 'error prev (as-warning "Cannot end with a project or workspace name")))))
       (pcase context
         (:start
          (treemacs-return-if (not (s-matches? treemacs--persist-workspace-name-regex line))
            `(error ,line ,(as-warning "First item must be a workspace name")))
          (treemacs--validate-persist-lines (cdr lines) :workspace line))
         (:workspace
          (treemacs-return-if (not (s-matches? treemacs--persist-project-name-regex line))
            `(error ,line ,(as-warning "Workspace name must be followed by project name")))
          (treemacs--validate-persist-lines (cdr lines) :project line))
         (:project
          (treemacs-return-if (not (s-matches? treemacs--persist-kv-regex line))
            `(error ,prev ,(as-warning "Project name must be followed by path declaration")))
          (-let [path (cadr (s-split " :: " line))]
            ;; Path not existing is only a hard error when org-editing, when loading on boot
            ;; its significance is determined by the customization setting
            ;; treemacs-missing-project-action. Remote files are skipped to avoid opening
            ;; Tramp connections.
            (treemacs-return-if (and (string= treemacs--org-edit-buffer-name (buffer-name))
                                     (not (file-remote-p path))
                                     (not (file-exists-p path)))
              `(error ,line ,(format (as-warning "File '%s' does not exist") (propertize path 'face 'font-lock-string-face))))
            (treemacs--validate-persist-lines (cdr lines) :property line)))
         (:property
          (let ((line-is-workspace-name (s-matches? treemacs--persist-workspace-name-regex line))
                (line-is-project-name   (s-matches? treemacs--persist-project-name-regex line)))
            (cond
             (line-is-workspace-name
              (treemacs--validate-persist-lines (cdr lines) :workspace line))
             (line-is-project-name
              (treemacs--validate-persist-lines (cdr lines) :project line))
             (t
              (treemacs-return-if (-none? #'identity (list line-is-workspace-name line-is-project-name))
                `(error ,prev ,(as-warning "Path property must be followed by the next workspace or project"))))))))))))

(defun treemacs--restore ()
  "Restore treemacs' state from `treemacs-persist-file'."
  (unless (treemacs--should-not-run-persistence?)
    (-when-let (lines (treemacs--read-persist-lines))
      ;; Don't persist during restore. Otherwise, if the user would quit
      ;; Emacs during restore, for example during the completing read for
      ;; missing project action, the whole persist file would be emptied.
      (let ((kill-emacs-hook (remq #'treemacs--persist kill-emacs-hook)))
        (condition-case e
            (pcase (treemacs--validate-persist-lines lines)
              ('success
               (setf treemacs--workspaces (treemacs--read-workspaces (make-treemacs-iter :list lines))
                     (treemacs-current-workspace) (car treemacs--workspaces)))
              (`(error ,line ,error-msg)
               (treemacs--write-error-persist-state lines (format "'%s' in line '%s'" error-msg line))
               (treemacs-log "Could not restore saved state, %s:\n%s\n%s"
                             (pcase line
                               (:start "found error in the first line")
                               (:end "found error in the last line")
                               (other (format "found error in line '%s'" other)))
                             error-msg
                             (format "Broken state was saved to %s"
                                     (propertize treemacs--last-error-persist-file 'face 'font-lock-string-face)))))
          (error
           (progn
             (treemacs--write-error-persist-state lines e)
             (treemacs-log "Error '%s' when loading the persisted workspace.\n%s"
                           e
                           (format "Broken state was saved to %s"
                                   (propertize treemacs--last-error-persist-file 'face 'font-lock-string-face))))))))))

(defun treemacs--write-error-persist-state (lines error)
  "Write broken state LINES and ERROR to `treemacs--last-error-persist-file'."
  (-let [txt (concat (format "# State when last error occurred on %s\n" (format-time-string "%F %T"))
                     (format "# Error was %s\n\n" error)
                     (apply #'concat (--map (concat it "\n") lines)))]
    (f-write txt 'utf-8 treemacs--last-error-persist-file)))

(add-hook 'kill-emacs-hook #'treemacs--persist)

(provide 'treemacs-persistence)

;;; treemacs-persistence.el ends here
