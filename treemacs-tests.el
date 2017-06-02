;;; treemacs.el --- A tree style file viewer package

;; Copyright (C) 2017 Alexander Miller

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

;;; Code:

(require 'treemacs-impl)
(require 'treemacs-customization)
(require 'treemacs-persist)
(require 'ert)
(require 'el-mock)

;; treemacs--maybe-filter-dotfiles
(progn
  (ert-deftest filter-dotfiles::do-nothing-when-dotfiles-are-shown ()
    (with-mock (stub treemacs--current-root => "/home/")
               (let ((treemacs-show-hidden-files t)
                     (input '("/home/.A" "/home/B/C" "/home/.A/B" "/home/.A/.B/.C")))
                 (should (equal input (treemacs--maybe-filter-dotfiles input))))))

  (ert-deftest filter-dotfiles::do-nothing-for-nulls ()
    (with-mock (stub treemacs--current-root => "/home/")
               (let ((treemacs-show-hidden-files nil))
                 (should (null (treemacs--maybe-filter-dotfiles nil))))))

  (ert-deftest filter-dotfiles::do-nothing-for-empty-input ()
    (with-mock (stub treemacs--current-root => "/home/")
               (let ((treemacs-show-hidden-files nil))
                 (should (null (treemacs--maybe-filter-dotfiles '()))))))

  (ert-deftest filter-dotfiles::filter-single-dotfile ()
    (with-mock (stub treemacs--current-root => "/home/")
               (let ((treemacs-show-hidden-files nil)
                     (input '("/home/A/B/C/D/.d")))
                 (should (null (treemacs--maybe-filter-dotfiles input))))))

  (ert-deftest filter-dotfiles::filter-dotfile-based-on-parent ()
    (with-mock (stub treemacs--current-root => "/home/")
               (let ((treemacs-show-hidden-files nil)
                     (input '("/home/A/B/C/.D/d")))
                 (should (null (treemacs--maybe-filter-dotfiles input))))))

  (ert-deftest filter-dotfiles::dont-filter-dotfile-above-root ()
    (with-mock (stub treemacs--current-root => "/home/.A/B")
               (let ((treemacs-show-hidden-files nil)
                     (input '("/home/.A/B/C/d")))
                 (should (equal input (treemacs--maybe-filter-dotfiles input))))))

  (ert-deftest filter-dotfiles::filter-long-input ()
    (with-mock (stub treemacs--current-root => "/home/.A/B")
               (let ((treemacs-show-hidden-files nil)
                     (input '("/home/.A/B/C/d" "/home/.A/B/.C/D/E" "/home/.A/B/C/.d" "/home/.A/B/C/D/E")))
                 (should (equal '("/home/.A/B/C/d" "/home/.A/B/C/D/E") (treemacs--maybe-filter-dotfiles input)))))))

;; treemacs--add-to-cache
(progn
  (ert-deftest add-to-dirs-cache::add-single-item ()
    (let ((parent "/home/A")
          (child  "/home/A/B")
          (treemacs--open-dirs-cache nil))
      (treemacs--add-to-cache parent child)
      (should (equal `((,parent ,child)) treemacs--open-dirs-cache))))

  (ert-deftest add-to-dirs-cache::add-two-same-parent-items ()
    (let ((parent "/home/A")
          (child1 "/home/A/B1")
          (child2 "/home/A/B2")
          (treemacs--open-dirs-cache nil))
      (treemacs--add-to-cache parent child1)
      (treemacs--add-to-cache parent child2)
      (should (equal `((,parent ,child2 ,child1)) treemacs--open-dirs-cache))))

  (ert-deftest add-to-dirs-cache::add-two-different-parent-items ()
    (let ((parent1 "/home/A1")
          (parent2 "/home/A2")
          (child1  "/home/A/B1")
          (child2  "/home/A/B2")
          (treemacs--open-dirs-cache nil))
      (treemacs--add-to-cache parent1 child1)
      (treemacs--add-to-cache parent2 child2)
      (should (equal `((,parent2 ,child2) (,parent1 ,child1)) treemacs--open-dirs-cache))))

  (ert-deftest add-to-dirs-cache::add-new-child-to-cache-with-multiple-items ()
    (let ((parent1 "/home/A1")
          (parent2 "/home/A2")
          (child1  "/home/A/B1")
          (child11 "/home/A/B11")
          (child2  "/home/A/B2")
          (treemacs--open-dirs-cache nil))
      (treemacs--add-to-cache parent1 child1)
      (treemacs--add-to-cache parent2 child2)
      (treemacs--add-to-cache parent1 child11)
      (should (equal `((,parent2 ,child2) (,parent1 ,child11 ,child1)) treemacs--open-dirs-cache)))))

;; treemacs--clear-from-cache
(progn
  (ert-deftest clear-from-dirs-cache::clear-item-from-empty-cache ()
    (let ((treemacs--open-dirs-cache nil))
      (treemacs--clear-from-cache "/home/A")
      (should (null treemacs--open-dirs-cache))))

  (ert-deftest clear-from-dirs-cache::clear-item-not-in-the-cache ()
    (let ((treemacs--open-dirs-cache '(("/home/A" "/home/A/B"))))
      (treemacs--clear-from-cache "/home/X")
      (should (equal '(("/home/A" "/home/A/B")) treemacs--open-dirs-cache))))

  (ert-deftest clear-from-dirs-cache::clear-item-from-empty-cache-with-purge ()
    (let ((treemacs--open-dirs-cache nil))
      (treemacs--clear-from-cache "/home/A" t)
      (should (null treemacs--open-dirs-cache))))

  (ert-deftest clear-from-dirs-cache::clear-item-from-cache-deleting-the-entry ()
    (let ((treemacs--open-dirs-cache '(("/home/A" "/home/A/B"))))
      (treemacs--clear-from-cache "/home/A/B")
      (should (null treemacs--open-dirs-cache))))

  (ert-deftest clear-from-dirs-cache::clear-item-from-cache-deleting-the-entry-with-purge ()
    (let ((treemacs--open-dirs-cache '(("/home/A" "/home/A/B"))))
      (treemacs--clear-from-cache "/home/A/B" t)
      (should (null treemacs--open-dirs-cache))))

  (ert-deftest clear-from-dirs-cache::clear-item-from-cache-leaving-other-entry ()
    (let* ((entry1 '("/home/A" "/home/A/B"))
           (entry2 '("/home/C" "/home/C/D"))
           (treemacs--open-dirs-cache (list entry1 entry2)))
      (treemacs--clear-from-cache "/home/A/B")
      (should (equal treemacs--open-dirs-cache `(,entry2)))))

  (ert-deftest clear-from-dirs-cache::clear-item-from-cache-leaving-other-entry-with-purge ()
    (let* ((entry1 '("/home/A" "/home/A/B"))
           (entry2 '("/home/C" "/home/C/D"))
           (treemacs--open-dirs-cache (list entry1 entry2)))
      (treemacs--clear-from-cache "/home/A/B" t)
      (should (equal treemacs--open-dirs-cache `(,entry2)))))

  (ert-deftest clear-from-dirs-cache::clear-one-of-two-items ()
    (let* ((treemacs--open-dirs-cache '(("/home/A" "/home/A/B" "/home/A/C"))))
      (treemacs--clear-from-cache "/home/A/B" t)
      (should (equal treemacs--open-dirs-cache '(("/home/A" "/home/A/C"))))))

  (ert-deftest clear-from-dirs-cache::clear-one-of-two-items-with-purge ()
    (let* ((treemacs--open-dirs-cache '(("/home/A" "/home/A/B" "/home/A/C"))))
      (treemacs--clear-from-cache "/home/A/B" t)
      (should (equal treemacs--open-dirs-cache '(("/home/A" "/home/A/C"))))))

  (ert-deftest clear-from-dirs-cache::clear-item-and-purge-children ()
    (let* ((treemacs--open-dirs-cache
            '(("/home/A" "/home/A/B")
              ("/home/A/B" "/home/A/B/C1" "/home/A/B/C2")
              ("/home/A/B/C1" "/home/A/B/C1/D1" "/home/A/B/C1/D2" "/home/A/B/C1/D3")
              ("/home/A/B/C2" "/home/A/B/C2/D1")
              ("/home/A/B/C1/D3" "/home/A/B/C1/D3/E1")
              ("/home/A2" "/home/A2/B1" "/home/A2/B2"))))
      (treemacs--clear-from-cache "/home/A/B" t)
      (should (equal treemacs--open-dirs-cache '(("/home/A2" "/home/A2/B1" "/home/A2/B2")))))))

;; treemacs--is-path-in-dir?
(progn
  (ert-deftest path-in-dir::direct-parent ()
    (let ((path "~/A/B/c")
          (parent "~/A/B"))
      (should (treemacs--is-path-in-dir? path parent))))

  (ert-deftest path-in-dir::indirect-parent ()
    (let ((path "~/A/B/C/D/e")
          (parent "~/A"))
      (should (treemacs--is-path-in-dir? path parent))))

  (ert-deftest path-in-dir::not-a-parent ()
    (let ((path "~/A/B/C/D/e")
          (parent "~/B"))
      (should-not (treemacs--is-path-in-dir? path parent))))

  (ert-deftest path-in-dir::not-a-parent-with-simialr-prefix ()
    (let ((path "~/A/prefix")
          (parent "~/A/prefixp"))
      (should-not (treemacs--is-path-in-dir? path parent)))))

;; treemacs--get-face
(progn
  (ert-deftest get-face::dir-face-for-path-without-git-info ()
    (should (eq 'treemacs-directory-face (treemacs--get-face "~/A" t nil))))

  (ert-deftest get-face::dir-face-for-path-with-git-info ()
    (should (eq 'treemacs-directory-face (treemacs--get-face "~/A" t '(("M" . "~/A"))))))

  (ert-deftest get-face::unmodified-face-for-file-without-git-info ()
    (should (eq 'treemacs-git-unmodified-face (treemacs--get-face "~/A" nil nil))))

  (ert-deftest get-face::unmodified-face-for-file-without-fitting-git-info ()
    (let ((git-info '(("M" . "~/B") ("??" . "~A/b"))))
      (should (eq 'treemacs-git-unmodified-face (treemacs--get-face "~/A" nil git-info)))))

  (ert-deftest get-face::unmodified-face-for-file-without-fitting-git-info ()
    (let ((git-info '(("M" . "~/B") ("??" . "~A/b"))))
      (should (eq 'treemacs-git-unmodified-face (treemacs--get-face "~/A" nil git-info)))))

  (ert-deftest get-face::unmodified-face-for-file-without-fitting-git-status ()
    (let ((git-info '(("0" . "~/A"))))
      (should (eq 'treemacs-git-unmodified-face (treemacs--get-face "~/A" nil git-info)))))

  (ert-deftest get-face::modified-face-for-modified-file ()
    (let ((git-info '(("!!" . "~/A/B/c") ("M" . "~/A"))))
      (should (eq 'treemacs-git-modified-face (treemacs--get-face "~/A" nil git-info)))))

  (ert-deftest get-face::untracked-face-for-untracked-file ()
    (let ((git-info '(("!!" . "~/A/B/c") ("??" . "~/A"))))
      (should (eq 'treemacs-git-untracked-face (treemacs--get-face "~/A" nil git-info)))))

  (ert-deftest get-face::ignored-face-for-ignored-file ()
    (let ((git-info '(("!!" . "~/A/B/c") ("!!" . "~/A"))))
      (should (eq 'treemacs-git-ignored-face (treemacs--get-face "~/A" nil git-info)))))

  (ert-deftest get-face::added-face-for-added-file ()
    (let ((git-info '(("!!" . "~/A/B/c") ("A" . "~/A"))))
      (should (eq 'treemacs-git-added-face (treemacs--get-face "~/A" nil git-info))))))

;; treemacs--current-visibility
(progn
  (ert-deftest current-visibility::visible-buffer ()
      (with-mock
        (stub treemacs--is-visible? => t)
        (stub treemacs--buffer-exists? => t)
        (should (eq 'visible (treemacs--current-visibility)))))

  (ert-deftest current-visibility::visible-buffer-even-when-exists?-is-nil ()
    (with-mock
      (stub treemacs--is-visible? => t)
      (stub treemacs--buffer-exists? => nil)
      (should (eq 'visible (treemacs--current-visibility)))))

  (ert-deftest current-visibility::existing-buffer ()
    (with-mock
      (stub treemacs--is-visible? => nil)
      (stub treemacs--buffer-exists? => t)
      (should (eq 'exists (treemacs--current-visibility)))))

  (ert-deftest current-visibility::no-buffer ()
    (with-mock
      (stub treemacs--is-visible? => nil)
      (stub treemacs--buffer-exists? => nil)
      (should (eq 'none (treemacs--current-visibility))))))

;; treemacs--unquote
(progn
  (ert-deftest unquote::does-not-fail-on-nil-string ()
    (should (null (treemacs--unqote nil))))

  (ert-deftest unquote::does-not-fail-on-empty-string ()
    (let ((input ""))
      (should (eq input (treemacs--unqote input)))))

  (ert-deftest unquote::returns-input-unchanged-when-unquoted ()
    (let ((input "input"))
      (should (eq input (treemacs--unqote input)))))

  (ert-deftest unquote::returns-input-unchanged-when-input-ends-with-quote ()
    (let ((input "input\""))
      (should (eq input (treemacs--unqote input)))))

  (ert-deftest unquote::unquotes-when-input-is-quoted ()
    (let ((input "\"input\""))
      (should (equal "input" (treemacs--unqote input)))))

  (ert-deftest unquote::unquotes-when-input-starts-with-quote ()
    (let ((input "\"input"))
      (should (equal "input" (treemacs--unqote input))))))

;; treemacs--reject-ignored-files
(progn
  (let ((treemacs-ignored-file-predicates (default-value 'treemacs-ignored-file-predicates)))

    (ert-deftest reject-ignored::fails-on-nil-file ()
      (should-error (treemacs--reject-ignored-files nil)))

    (ert-deftest reject-ignored::rejects-emacs-lock-file ()
      (should-not (treemacs--reject-ignored-files '("~/A/B/C/.#foo.el"))))

    (ert-deftest reject-ignored::rejects-emacs-backup-file ()
      (should-not (treemacs--reject-ignored-files '("~/A/B/C/foo.el~"))))

    (ert-deftest reject-ignored::rejects-emacs-autosave-file ()
      (should-not (treemacs--reject-ignored-files '("~/A/B/C/#foo.el#"))))

    (ert-deftest reject-ignored::rejects-flycheck-temp-file ()
      (should-not (treemacs--reject-ignored-files '("~/A/B/C/flycheck_foo.el"))))

    (ert-deftest reject-ignored::rejects-dot ()
      (should-not (treemacs--reject-ignored-files '("."))))

    (ert-deftest reject-ignored::rejects-dot-dot ()
      (should-not (treemacs--reject-ignored-files '(".."))))

    (ert-deftest reject-ignored::accepts-dotfile ()
      (should (treemacs--reject-ignored-files '("~/A/B/C/.foo.el"))))

    (ert-deftest reject-ignored::accepts-std-file ()
      (should (treemacs--reject-ignored-files '("~/A/B/C/foo.el"))))

    (ert-deftest reject-ignored::accepts-empty-file ()
      (should (treemacs--reject-ignored-files '(""))))

    (ert-deftest reject-ignored::accepts-dir ()
      (should (treemacs--reject-ignored-files '("~/A/B/C/"))))

    (ert-deftest reject-ignored::accepts-abs-file ()
      (should (treemacs--reject-ignored-files '("foo.el"))))))

;; treemacs--reject-ignored-and-dotfiles
(progn
  (let ((treemacs-ignored-file-predicates (default-value 'treemacs-ignored-file-predicates)))

    (ert-deftest reject-ignored-and-dotfiles::fails-on-nil-file ()
      (should-error (treemacs--reject-ignored-and-dotfiles nil)))

    (ert-deftest reject-ignored-and-dotfiles::rejects-emacs-lock-file ()
      (should-not (treemacs--reject-ignored-and-dotfiles '("~/A/B/C/.#foo.el"))))

    (ert-deftest reject-ignored-and-dotfiles::rejects-emacs-backup-file ()
      (should-not (treemacs--reject-ignored-and-dotfiles '("~/A/B/C/foo.el~"))))

    (ert-deftest reject-ignored-and-dotfiles::rejects-emacs-autosave-file ()
      (should-not (treemacs--reject-ignored-and-dotfiles '("~/A/B/C/#foo.el#"))))

    (ert-deftest reject-ignored-and-dotfiles::rejects-flycheck-temp-file ()
      (should-not (treemacs--reject-ignored-and-dotfiles '("~/A/B/C/flycheck_foo.el"))))

    (ert-deftest reject-ignored-and-dotfiles::rejects-dotfile ()
      (should-not (treemacs--reject-ignored-and-dotfiles '("~/A/B/C/.foo.el"))))

    (ert-deftest reject-ignored-and-dotfiles::rejects-dot ()
      (should-not (treemacs--reject-ignored-and-dotfiles '("."))))

    (ert-deftest reject-ignored-and-dotfiles::rejects-dot-dot ()
      (should-not (treemacs--reject-ignored-and-dotfiles '(".."))))

    (ert-deftest reject-ignored-and-dotfiles::accepts-std-file ()
      (should (treemacs--reject-ignored-and-dotfiles '("~/A/B/C/foo.el"))))

    (ert-deftest reject-ignored-and-dotfiles::accepts-std-file ()
      (should (treemacs--reject-ignored-and-dotfiles '("~/A/B/C/foo.el"))))

    (ert-deftest reject-ignored-and-dotfiles::accepts-std-file ()
      (should (treemacs--reject-ignored-and-dotfiles '("~/A/B/C/foo.el"))))

    (ert-deftest reject-ignored-and-dotfiles::accepts-empty-file ()
      (should (treemacs--reject-ignored-and-dotfiles '(""))))

    (ert-deftest reject-ignored-and-dotfiles::accepts-dir ()
      (should (treemacs--reject-ignored-and-dotfiles '("~/A/B/C/"))))

    (ert-deftest reject-ignored-and-dotfiles::accepts-abs-file ()
      (should (treemacs--reject-ignored-and-dotfiles '("foo.el"))))))

;; str-assq-delete-all
(progn
  (ert-deftest str-assq-delete::does-nothing-for-nil-key-and-list ()
    (should-not (str-assq-delete-all nil nil)))

  (ert-deftest str-assq-delete::does-nothing-for-nil-key ()
    (let ((input '(("A" . 1))))
      (should (equal input (str-assq-delete-all nil input)))))

  (ert-deftest str-assq-delete::does-nothing-for-nil-list ()
    (should-not (str-assq-delete-all "A" nil)))

  (ert-deftest str-assq-delete::returns-same-list-when-nothing-to-delete ()
    (let ((input '(("A" . 1) ("B" . 2))))
      (should (eq input (str-assq-delete-all "X" input)))))

  (ert-deftest str-assq-delete::deletes-every-fitting-item ()
    (let ((input '(("X" . 1) ("A" . 2) ("X" . 3) ("B" . 4) ("X" . 5))))
      (should (equal '(("A" . 2) ("B" . 4))
                     (str-assq-delete-all "X" input))))))

;; treemacs--parent
(progn
  (ert-deftest parent::fails-on-nil-path ()
    (should-error (treemacs--parent nil)))

  (ert-deftest parent::does-not-fail-on-empty-path ()
    (should (treemacs--parent "")))

  (ert-deftest parent::returns-parent ()
    (should (equal "/home/A" (treemacs--parent "/home/A/B"))))

  (ert-deftest parent::returns-root ()
    (should (equal "/" (treemacs--parent "/")))))

;; treemacs--read-persist-data
(progn
  (ert-deftest read-persist::reads-nil-from-empty-file ()
    (with-mock
      (stub f-file? => t)
      (stub f-read => "")
      (should-not (treemacs--read-persist-data))))

  (ert-deftest read-persist::reads-all-data-correctly ()
    (with-mock
      (stub f-file? => t)
      (stub f-read => "ROOT : ~/A\nOPEN-DIRS : ~/A/X|~/A/Y\nPOINT-AT : ~/A/p")
      (should (let ((result (treemacs--read-persist-data)))
                (and (= 3 (length result))
                     (--all? (-contains? result it)
                             '(("ROOT" . "~/A")
                               ("OPEN-DIRS" . "~/A/X|~/A/Y")
                               ("POINT-AT" . "~/A/p"))))))))

  (ert-deftest read-persist::reads-empty-open-dirs-correctly ()
    (with-mock
      (stub f-file? => t)
      (stub f-read => "ROOT : ~/A\nOPEN-DIRS :\nPOINT-AT : ~/A/p")
      (should (let ((result (treemacs--read-persist-data)))
                (and (= 2 (length result))
                     (--all? (-contains? result it)
                             '(("ROOT" . "~/A")
                               ("POINT-AT" . "~/A/p")))))))))

;; treemacs--check-window-system
(progn
  (ert-deftest check-window-system::returns-nil-when-no-change-from-x ()
    (with-mock
     (stub window-system => 'x)
     (let ((treemacs--in-gui 'x))
       (should-not (treemacs--check-window-system)))))

  (ert-deftest check-window-system::returns-nil-when-no-change-from-term ()
    (with-mock
      (stub window-system => nil)
      (let ((treemacs--in-gui))
        (should-not (treemacs--check-window-system)))))

  (ert-deftest check-window-system::returns-t-when-no-change-from-x-to-term ()
    (with-mock
      (stub window-system => 'x)
      (let ((treemacs--in-gui))
        (should (treemacs--check-window-system)))))

  (ert-deftest check-window-system::returns-t-when-no-change-from-term-to-x ()
    (with-mock
      (stub window-system => nil)
      (let ((treemacs--in-gui 'x))
        (should (treemacs--check-window-system))))))

(provide 'treemacs-tests)

;;; treemacs-tests.el ends here
