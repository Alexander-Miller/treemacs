;;; treemacs-test.el --- Tests for treemacs

;; Copyright (C) 2020 Alexander Miller

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

;;; Code:

(require 'treemacs)
(require 'ert)
(require 'el-mock)
(require 'org)

(defmacro treemacs--with-workspace (ws &rest body)
  "Set WS as the current workspace and then run BODY."
  (declare (indent 1))
  `(-let [--original-- (treemacs-current-workspace)]
     (unwind-protect
         (progn
           (set-frame-parameter (selected-frame) 'treemacs-workspace ,ws)
           ,@body)
       (set-frame-parameter (selected-frame) 'treemacs-workspace --original--))))

;; Thorough Sys Test
(ert-deftest treemacs::sys-test ()
  (save-window-excursion
    (save-match-data
      (unless noninteractive
        (unwind-protect
            (let* ((imenu-auto-rescan t)
                   (org-imenu-depth 10)
                   (treemacs-collapse-dirs 3)
                   (project (make-treemacs-project :name "Test Project" :path (concat treemacs-dir "/test") :path-status 'local-readable))
                   (workspace (make-treemacs-workspace :name "Test Workspace" :projects (list project)))
                   (workspaces treemacs--workspaces))
              (treemacs--with-workspace workspace
                (setq treemacs--workspaces (list workspace))
                (delete-other-windows)
                (--when-let (treemacs-get-local-buffer) (kill-buffer it))

                (switch-to-buffer "*scratch*")

                ;; init with a workspace with a single project and make sure the root node looks right
                (call-interactively 'treemacs)
                (should (treemacs-get-local-buffer))
                (should (string= "Test Project" (treemacs--get-label-of (treemacs-current-button))))

                ;; add another project, expand both and jump between them
                (-let [unread-command-events (listify-key-sequence (kbd "RET"))]
                  (treemacs-add-project-to-workspace (concat treemacs-dir "/src")))
                (should (string= "src" (treemacs--get-label-of (treemacs-current-button))))
                (call-interactively #'treemacs-previous-project)
                (should (string= "Test Project" (treemacs--get-label-of (treemacs-current-button))))
                (call-interactively #'treemacs-TAB-action)
                (call-interactively #'treemacs-next-project)
                (call-interactively #'treemacs-TAB-action)
                (should (string= "src" (treemacs--get-label-of (treemacs-current-button))))
                (call-interactively #'treemacs-next-line)
                (call-interactively #'treemacs-next-line)
                (call-interactively #'treemacs-previous-project)
                (should (string= "src" (treemacs--get-label-of (treemacs-current-button))))
                (call-interactively #'treemacs-previous-project)
                (should (string= "Test Project" (treemacs--get-label-of (treemacs-current-button))))

                ;; Test Project <- point is here
                ;; ├── testdir1/testdir2
                ;; │   ├── testfile.el
                ;; │   └── testfile.org
                ;; ├── test-helper.el
                ;; ├── treemacs-test.el

                ;; Expand everything
                (call-interactively #'treemacs-next-line)
                (call-interactively #'treemacs-TAB-action)
                (should (string= "testdir1/testdir2" (treemacs--get-label-of (treemacs-current-button))))
                (should (= 1 (treemacs--prop-at-point :collapsed)))

                ;; try goto-parent and -neighbour
                (call-interactively #'treemacs-next-line)
                (call-interactively #'treemacs-next-line)
                (should (string= "testfile.org" (treemacs--get-label-of (treemacs-current-button))))

                (call-interactively #'treemacs-goto-parent-node)
                (should (string= "testdir1/testdir2" (treemacs--get-label-of (treemacs-current-button))))
                (call-interactively #'treemacs-next-neighbour)
                (should (string= "test-helper.el" (treemacs--get-label-of (treemacs-current-button))))
                (call-interactively #'treemacs-next-neighbour)
                (should (string= "treemacs-test.el" (treemacs--get-label-of (treemacs-current-button))))
                (call-interactively #'treemacs-previous-neighbour)
                (call-interactively #'treemacs-previous-neighbour)
                (should (string= "testdir1/testdir2" (treemacs--get-label-of (treemacs-current-button))))

                ;; test tags
                (treemacs-goto-node (concat treemacs-dir "/test/testdir1/testdir2/testfile.el"))
                (should (string= "testfile.el" (treemacs--get-label-of (treemacs-current-button))))

                (call-interactively #'treemacs-TAB-action)
                (should (equal '("Variables" "Functions")
                               (-map #'treemacs--get-label-of
                                     (treemacs-collect-child-nodes (treemacs-current-button)))))

                (call-interactively #'treemacs-next-line)
                (call-interactively #'treemacs-next-line)
                (should (string= "Functions" (treemacs--get-label-of (treemacs-current-button))))
                (call-interactively #'treemacs-TAB-action)
                (should (equal '("fn1" "fn2")
                               (-map #'treemacs--get-label-of
                                     (treemacs-collect-child-nodes (treemacs-current-button)))))

                ;; open a tag
                (call-interactively #'treemacs-next-line)
                (call-interactively #'treemacs-visit-node-no-split)
                (should (string= "testfile.el" (f-filename buffer-file-name)))
                (should (string= "(defun fn1 ())\n" (thing-at-point 'line t)))
                (should (= 2 (length (window-list))))
                (kill-buffer (current-buffer))

                ;; go back
                (call-interactively 'treemacs-select-window)
                (should (eq major-mode 'treemacs-mode))

                ;; now try reopening of tags with org
                (call-interactively #'treemacs-goto-parent-node)
                (call-interactively #'treemacs-goto-parent-node)
                (call-interactively #'treemacs-next-neighbour)
                (should (string= "testfile.org" (treemacs--get-label-of (treemacs-current-button))))

                ;; expand all
                (treemacs-TAB-action t)
                (call-interactively #'treemacs-next-line)
                (call-interactively #'treemacs-next-line)
                (call-interactively #'treemacs-next-line)
                (should (string= "Foo3" (treemacs--get-label-of (treemacs-current-button))))

                ;; go up again and check everything's reopened
                (call-interactively #'treemacs-goto-parent-node)
                (call-interactively #'treemacs-goto-parent-node)
                (call-interactively #'treemacs-goto-parent-node)
                (should (string= "testfile.org" (treemacs--get-label-of (treemacs-current-button))))
                (call-interactively #'treemacs-TAB-action)
                (call-interactively #'treemacs-TAB-action)
                (call-interactively #'treemacs-next-line)
                (call-interactively #'treemacs-next-line)
                (call-interactively #'treemacs-next-line)
                (should (string= "Foo3" (treemacs--get-label-of (treemacs-current-button))))

                ;; try goto with splitting too
                (call-interactively #'treemacs-visit-node-vertical-split)
                (should (eq major-mode 'org-mode))
                (should (= 3 (length (window-list))))
                (org-reveal)
                (should (string= "*** Foo3\n" (thing-at-point 'line)))
                (kill-buffer (current-buffer))
                (call-interactively #'treemacs-select-window)
                (should (string= "Foo3" (treemacs--get-label-of (treemacs-current-button))))

                ;; finally try moving to the current tag
                (call-interactively #'treemacs-previous-project)
                (should (string= "Test Project" (treemacs--get-label-of (treemacs-current-button))))
                (treemacs-TAB-action t)
                (other-window 1)
                (find-file (concat treemacs-dir "/test/testdir1/testdir2/testfile.el"))
                (goto-char (point-min))
                (call-interactively #'treemacs-find-tag)
                (call-interactively #'treemacs-select-window)
                (should (string= "FOO" (treemacs--get-label-of (treemacs-current-button)))))

              (--when-let (treemacs-get-local-buffer) (kill-buffer it))
              (setq treemacs--workspaces workspaces)))))))

(provide 'treemacs-test)

;;; treemacs-test.el ends here
