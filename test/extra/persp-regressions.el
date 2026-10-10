;;; persp-regressions.el --- Perspective activation regressions -*- lexical-binding: t -*-

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

(require 'cl-lib)
(require 'ert)

;; Run in a clean Emacs process, with all state confined to this temporary root.
(defvar treemacs-persp-test-root (make-temp-file "treemacs-persp-test-" t))
(setq user-emacs-directory (file-name-as-directory treemacs-persp-test-root)
      default-directory user-emacs-directory
      persp-save-dir (expand-file-name "persp/" treemacs-persp-test-root)
      persp-auto-save-opt 0
      persp-auto-resume-time -1
      treemacs-persist-file (expand-file-name "tree-state" treemacs-persp-test-root)
      treemacs-last-error-persist-file (expand-file-name "tree-error" treemacs-persp-test-root))
(require 'treemacs-persp)
(persp-mode 1)
(treemacs-set-scope-type 'Perspectives)

(ert-deftest treemacs-persp-existing-workspace-is-selected-immediately ()
  (let* ((name "existing")
         (workspace (treemacs-workspace->create! :name "Perspective existing")))
    (setq treemacs--workspaces (append treemacs--workspaces (list workspace)))
    (persp-switch name)
    (should (eq workspace (treemacs-current-workspace)))))

(ert-deftest treemacs-persp-new-workspace-creation-remains-deferred ()
  (let (scheduled)
    (cl-letf (((symbol-function 'run-with-timer)
               (lambda (delay repeat callback &rest args)
                 (setq scheduled (list delay repeat callback args)))))
      (persp-switch "deferred"))
    (should scheduled)
    (should-not (treemacs--find-workspace-by-name "Perspective deferred"))
    (apply (nth 2 scheduled) (nth 3 scheduled))
    (should (treemacs--find-workspace-by-name "Perspective deferred"))))

(ert-deftest treemacs-persp-saved-load-fresh-sidebar-projects ()
  (skip-unless (display-graphic-p))
  (let* ((wrong (expand-file-name "wrong" treemacs-persp-test-root))
         (right (expand-file-name "right" treemacs-persp-test-root))
         (snapshot (expand-file-name "snapshot" treemacs-persp-test-root)))
    (make-directory wrong t)
    (make-directory right t)
    (setq treemacs--workspaces
          (list (treemacs-workspace->create! :name "Perspective fallback" :projects
                 (list (treemacs-project->create! :name "UNIQUE_WRONG" :path wrong :path-status 'local-readable)))
                (treemacs-workspace->create! :name "Perspective target" :projects
                 (list (treemacs-project->create! :name "UNIQUE_RIGHT" :path right :path-status 'local-readable)))))
    (persp-switch "target")
    (persp-save-to-file-by-names snapshot *persp-hash* '("target") nil nil)
    (persp-switch "fallback")
    (persp-kill "target")
    (persp-load-from-file-by-names snapshot *persp-hash* '("target"))
    (persp-switch "target")
    (treemacs)
    (let ((text (with-current-buffer (treemacs-get-local-buffer) (buffer-string))))
      (should (string-match-p "UNIQUE_RIGHT" text))
      (should-not (string-match-p "UNIQUE_WRONG" text)))
    (sleep-for 0.35)
    (let ((text (with-current-buffer (treemacs-get-local-buffer) (buffer-string))))
      (should (string-match-p "UNIQUE_RIGHT" text))
      (should-not (string-match-p "UNIQUE_WRONG" text)))
    ;; Do not enforce perspective/workspace identity on every sidebar opening:
    ;; explicitly selecting another workspace must remain supported.
    (treemacs-do-switch-workspace (car treemacs--workspaces))
    (treemacs)
    (treemacs)
    (should (string-match-p "UNIQUE_WRONG"
                           (with-current-buffer (treemacs-get-local-buffer) (buffer-string))))))

;;; persp-regressions.el ends here
