;;; -*- lexical-binding: t -*-
;;; Based on https://github.com/flycheck/flycheck/blob/master/maint/flycheck-checkdoc.el

(require 'checkdoc)
(require 'dash)
(require 's)

(defconst all-el-files (append (directory-files "./src/elisp" :full ".el")
                               (directory-files "./src/extra" :full ".el")))

(defun checkdoc-buffer (filename)
  ;; output only /src/elisp/filename.el as when compiling
  (message "Checkdoc %s" (substring filename (1+ (s-index-of "/src" filename))))
  (with-temp-buffer
    ;; Visit the file to make sure that the filename is set, as some checkdoc
    ;; lints only apply for buffers with filenames
    (insert-file-contents filename :visit)
    (set-buffer-modified-p nil)
    (delay-mode-hooks (emacs-lisp-mode))
    (setf delay-mode-hooks nil)
    (let ((checkdoc-autofix-flag 'never)
          (checkdoc-force-docstrings-flag t)
          (checkdoc-force-history-flag nil)
          (checkdoc-permit-comma-termination-flag nil)
          (checkdoc-spellcheck-documentation-flag t)
          (checkdoc-arguments-in-order-flag t)
          (checkdoc-verb-check-experimental-flag t))
      (checkdoc-current-buffer :take-notes))
    (get-errors)))

(defun get-errors ()
  (with-current-buffer checkdoc-diagnostic-buffer
    (goto-char (point-min))
    ;; Skip over the checkdoc header
    (re-search-forward (rx line-start "***" (1+ not-newline)
                           ": checkdoc-current-buffer"))
    (forward-line 1)
    (prog1
        (let ((text (buffer-substring-no-properties (point) (point-max))))
          (and (not (s-blank-p text))
               (split-string text "\n")))
      (kill-buffer))))

(let ((errors (-mapcat #'checkdoc-buffer all-el-files)))
  (-each errors #'message)
  (kill-emacs (if errors 1 0)))
