;;; persp-frame-runner.el --- Run isolated perspective regressions -*- lexical-binding: t -*-

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

(load (expand-file-name "persp-frame-regressions.el" (file-name-directory load-file-name)) nil t)
(let* ((stats (ert-run-tests-batch t))
       (failed (ert-stats-completed-unexpected stats))
       (print-level 3) (print-length 10))
  (with-temp-buffer
    (insert (format "GUI=%S EXPECTED=%s UNEXPECTED=%s\n"
                    (display-graphic-p) (ert-stats-completed-expected stats) failed))
    (mapc (lambda (test)
            (let ((result (ert-test-most-recent-result test)))
              (insert (format "%s %s %S\n" (ert-test-name test) (type-of result)
                              (when (ert-test-result-with-condition-p result)
                                (ert-test-result-with-condition-condition result))))))
          (ert--stats-tests stats))
    (if (getenv "TEST_LOG")
        (write-region (point-min) (point-max) (getenv "TEST_LOG") nil 'silent)
      (princ (buffer-string))))
  (delete-directory treemacs-persp-test-root t)
  (kill-emacs (if (zerop failed) 0 1)))

;;; persp-frame-runner.el ends here
