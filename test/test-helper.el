;;; test-helper.el --- Helpers for treemacs-test.el -*- lexical-binding: t -*-

;; Utilities for running dired-sidebar tests.

;;; Code:
(require 'ert)
(require 'f)

(add-to-list 'load-path
             (-> (f-this-file)
                 (f-parent)
                 (f-parent)
                 (f-join "src/elisp")))

;; changes needed to get output when runner `ert-runner --win'
;; see https://github.com/rejeep/ert-runner.el/pull/37/files#diff-539b7cf40ca1df64d9eb57afd5dab820R198

(defun ert-runner-message (format &rest args)
  "Emit a formatted message.

This bypasses the normal output capturing ert-runner does, and is
primarily intended for reporters."
  (let ((message (apply #'format format args)))
    (if ert-runner-output-file
        (f-append-text message 'utf-8 ert-runner-output-file)
      (princ message t))))

(defun ert-runner/run (&rest tests)
  (ert-runner/use-reporter ert-runner-reporter-name)
  (let ((test-files (ert-runner--test-files tests))
        (test-helper (f-expand "test-helper.el" ert-runner-test-path)))
    (condition-case e
        (progn
          (-each ert-runner-load-files #'ert-runner--load)
          (if (f-exists? test-helper)
              (ert-runner--load test-helper))
          (-each test-files #'ert-runner--load))
      (error
       (ert-runner-message "Error during test setup: %S. No tests were run.\n" e)
       (kill-emacs 1)))
    (if ert-runner-verbose
        (ert-runner/run-tests-batch-and-exit ert-runner-selector)
      (shut-up
        (ert-runner/run-tests-batch-and-exit ert-runner-selector)))))

(require 'treemacs)

;;; test-helper.el ends here
