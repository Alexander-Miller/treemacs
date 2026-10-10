;;; persp-rename-regressions.el --- Perspective activation regressions -*- lexical-binding: t -*-

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

(defun treemacs-persp-test-rename-fixture (name)
  (cl-letf (((symbol-function 'run-with-timer) (lambda (&rest _arguments))))
    (persp-switch name))
  (get-current-persp))

(defun treemacs-persp-test-add-workspace (name)
  (let ((workspace (treemacs-workspace->create! :name name)))
    (push workspace treemacs--workspaces)
    workspace))

(ert-deftest treemacs-persp-rename-rejects-sidebar-collision ()
  (let* ((perspective (treemacs-persp-test-rename-fixture "collision"))
         (source (treemacs-persp-test-add-workspace "Perspective collision"))
         (target (treemacs-persp-test-add-workspace "Perspective parked")))
    (should-error (persp-rename "parked" perspective) :type 'user-error)
    (should (equal (persp-name perspective) "collision"))
    (should (eq perspective (persp-get-by-name "collision")))
    (should-not (gethash "parked" *persp-hash*))
    (should (equal (treemacs-workspace->name source) "Perspective collision"))
    (should (equal (treemacs-workspace->name target) "Perspective parked"))))

(ert-deftest treemacs-persp-rename-rejects-ambiguous-source ()
  (let* ((perspective (treemacs-persp-test-rename-fixture "ambiguous"))
         (first (treemacs-persp-test-add-workspace "Perspective ambiguous"))
         (second (treemacs-persp-test-add-workspace "Perspective ambiguous")))
    (should-error (persp-rename "unambiguous" perspective) :type 'user-error)
    (should (equal (persp-name perspective) "ambiguous"))
    (should (equal (treemacs-workspace->name first) "Perspective ambiguous"))
    (should (equal (treemacs-workspace->name second) "Perspective ambiguous"))))

(ert-deftest treemacs-persp-rename-keeps-workspace-identity ()
  (let* ((perspective (treemacs-persp-test-rename-fixture "valid"))
         (source (treemacs-persp-test-add-workspace "Perspective valid")))
    (setf (treemacs-current-workspace) source)
    (should (equal (persp-rename "valid-renamed" perspective) "valid"))
    (should (equal (persp-name perspective) "valid-renamed"))
    (should (equal (treemacs-workspace->name source) "Perspective valid-renamed"))
    (should (eq source (treemacs-current-workspace)))))

(ert-deftest treemacs-persp-rename-without-sidebar-is-valid ()
  (let ((perspective (treemacs-persp-test-rename-fixture "unmapped")))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _arguments) (error "Unexpected workspace prompt"))))
      (should (equal (persp-rename "unmapped-renamed" perspective) "unmapped")))
    (should (equal (persp-name perspective) "unmapped-renamed"))
    (should-not (treemacs--find-workspace-by-name "Perspective unmapped-renamed"))))

(ert-deftest treemacs-persp-rename-explicit-nil-is-not-current-perspective ()
  (let ((perspective (treemacs-persp-test-rename-fixture "explicit-nil")))
    (persp-rename "unused" nil)
    (should (equal (persp-name perspective) "explicit-nil"))))

(ert-deftest treemacs-persp-rename-advice-is-scope-local ()
  (should (advice-member-p #'treemacs-persp--validate-rename 'persp-rename))
  (unwind-protect
      (progn
        (treemacs-scope->cleanup 'treemacs-persp-scope)
        (should-not (advice-member-p #'treemacs-persp--validate-rename 'persp-rename)))
    (treemacs-scope->setup 'treemacs-persp-scope)))

;;; persp-rename-regressions.el ends here
