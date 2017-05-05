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

;; treemacs--should-show?
(progn

  (let ((treemacs-dotfiles-regex (default-value 'treemacs-dotfiles-regex)))

    (ert-deftest should-show::fails-on-nil-file ()
      (should-error (treemacs--should-show? nil)))

    (ert-deftest should-show::shows-empty-name ()
      (should (treemacs--should-show? "")))

    (ert-deftest should-show::shows-file-name ()
      (should (treemacs--should-show? "~/A/B/C/d.el")))

    (ert-deftest should-show::shows-dir-name ()
      (should (treemacs--should-show? "~/A/B/C/d/")))

    (ert-deftest should-show::shows-abs-name ()
      (should (treemacs--should-show? "foo.el")))

    (ert-deftest should-show::shows-single-dot ()
      (should (treemacs--should-show? ".")))

    (ert-deftest should-show::wont-show-dot-dot ()
      (should-not (treemacs--should-show? "..")))

    (ert-deftest should-show::wont-show-abs-dotfile ()
      (should-not (treemacs--should-show? ".foo")))

    (ert-deftest should-show::wont-show-relative-dotfile ()
      (should-not (treemacs--should-show? "~/A/B/C/.foo")))))

(provide 'treemacs-tests)

;;; treemacs-tests.el ends here
