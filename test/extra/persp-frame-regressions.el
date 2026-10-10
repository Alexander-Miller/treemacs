;;; persp-frame-regressions.el --- Perspective activation regressions -*- lexical-binding: t -*-

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

(defun treemacs-persp-test-capture-callback ()
  (let (callback)
    (cl-letf (((symbol-function 'run-with-timer)
               (lambda (_delay _repeat function &rest arguments)
                 (setq callback (cons function arguments)))))
      (treemacs-persp--on-perspective-switch))
    callback))

(ert-deftest treemacs-persp-callback-keeps-origin-frame ()
  (skip-unless (display-graphic-p))
  (let ((origin (selected-frame)) (other (make-frame)) callback other-workspace)
    (unwind-protect
        (cl-letf (((symbol-function 'run-with-timer) (lambda (&rest _arguments))))
          (with-selected-frame origin
            (persp-switch "origin")
            (setq callback (treemacs-persp-test-capture-callback)))
          (with-selected-frame other
            (persp-switch "other")
            (treemacs-persp--ensure-workspace-exists)
            (setq other-workspace (treemacs-current-workspace))
            (apply (car callback) (cdr callback))
            (should (eq (selected-frame) other))
            (should (eq other-workspace (treemacs-current-workspace))))
          (with-selected-frame origin
            (should (equal "Perspective origin"
                           (treemacs-workspace->name (treemacs-current-workspace))))))
      (when (frame-live-p other) (delete-frame other t)))))

(ert-deftest treemacs-persp-callback-ignores-stale-perspective ()
  (let (callback (calls 0))
    (cl-letf (((symbol-function 'run-with-timer) (lambda (&rest _arguments))))
      (persp-switch "stale")
      (setq callback (treemacs-persp-test-capture-callback))
      (persp-switch "replacement"))
    (cl-letf (((symbol-function 'treemacs-persp--ensure-workspace-exists)
               (lambda () (cl-incf calls)))
              ((symbol-function 'treemacs--change-buffer-on-scope-change)
               (lambda (&rest _arguments) (cl-incf calls))))
      (apply (car callback) (cdr callback)))
    (should (zerop calls))))

(ert-deftest treemacs-persp-callback-ignores-dead-frame ()
  (skip-unless (display-graphic-p))
  (let ((origin (make-frame)) callback (calls 0))
    (unwind-protect
        (progn
          (with-selected-frame origin
            (cl-letf (((symbol-function 'run-with-timer) (lambda (&rest _arguments))))
              (persp-switch "dead")
              (setq callback (treemacs-persp-test-capture-callback))))
          (delete-frame origin t)
          (cl-letf (((symbol-function 'treemacs-persp--ensure-workspace-exists)
                     (lambda () (cl-incf calls)))
                    ((symbol-function 'treemacs--change-buffer-on-scope-change)
                     (lambda (&rest _arguments) (cl-incf calls))))
            (apply (car callback) (cdr callback)))
          (should (zerop calls)))
      (when (frame-live-p origin) (delete-frame origin t)))))

(ert-deftest treemacs-persp-callback-ignores-scope-mode-change ()
  (let ((callback (treemacs-persp-test-capture-callback)) (calls 0)
        (treemacs--current-scope-type 'treemacs-frame-scope))
    (cl-letf (((symbol-function 'treemacs-persp--ensure-workspace-exists)
               (lambda () (cl-incf calls)))
              ((symbol-function 'treemacs--change-buffer-on-scope-change)
               (lambda () (cl-incf calls))))
      (apply (car callback) (cdr callback)))
    (should (zerop calls))))

(ert-deftest treemacs-persp-only-latest-callback-runs ()
  (let ((first (treemacs-persp-test-capture-callback))
        (latest (treemacs-persp-test-capture-callback)) (calls 0))
    (cl-letf (((symbol-function 'treemacs-persp--ensure-workspace-exists)
               (lambda () (cl-incf calls)))
              ((symbol-function 'treemacs--change-buffer-on-scope-change)
               (lambda () (cl-incf calls))))
      (apply (car first) (cdr first))
      (should (zerop calls))
      (apply (car latest) (cdr latest))
      (should (= 2 calls))
      (apply (car latest) (cdr latest))
      (should (= 2 calls)))))

;;; persp-frame-regressions.el ends here
