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

(require 'treemacs)
(require 'ert)
(require 'el-mock)
(require 'subr-x)
(require 'f)

;; `treemacs--maybe-filter-dotfiles'
(progn
  (ert-deftest filter-dotfiles::do-nothing-when-dotfiles-are-shown ()
    (let ((treemacs-show-hidden-files t)
          (default-directory "/home/")
          (input '("/home/.A" "/home/B/C" "/home/.A/B" "/home/.A/.B/.C")))
      (should (equal input (treemacs--maybe-filter-dotfiles input)))))

  (ert-deftest filter-dotfiles::do-nothing-for-nulls ()
    (let ((treemacs-show-hidden-files nil)
          (default-directory "/home/"))
      (should (null (treemacs--maybe-filter-dotfiles nil)))))

  (ert-deftest filter-dotfiles::do-nothing-for-empty-input ()
    (let ((treemacs-show-hidden-files nil)
          (default-directory "/home/"))
      (should (null (treemacs--maybe-filter-dotfiles '())))))

  (ert-deftest filter-dotfiles::filter-single-dotfile ()
    (let ((treemacs-show-hidden-files nil)
          (default-directory "/home/")
          (input '("/home/A/B/C/D/.d")))
      (should (null (treemacs--maybe-filter-dotfiles input)))))

  (ert-deftest filter-dotfiles::filter-dotfile-based-on-parent ()
    (let ((treemacs-show-hidden-files nil)
          (default-directory "/home/")
          (input '("/home/A/B/C/.D/d")))
      (should (null (treemacs--maybe-filter-dotfiles input)))))

  (ert-deftest filter-dotfiles::dont-filter-dotfile-above-root ()
    (let ((treemacs-show-hidden-files nil)
          (default-directory "/home/.A/B")
          (input '("/home/.A/B/C/d")))
      (should (equal input (treemacs--maybe-filter-dotfiles input)))))

  (ert-deftest filter-dotfiles::filter-long-input ()
    (let ((treemacs-show-hidden-files nil)
          (default-directory "/home/.A/B")
          (input '("/home/.A/B/C/d" "/home/.A/B/.C/D/E" "/home/.A/B/C/.d" "/home/.A/B/C/D/E")))
      (should (equal '("/home/.A/B/C/d" "/home/.A/B/C/D/E") (treemacs--maybe-filter-dotfiles input))))))

;; `treemacs--add-to-cache'
(progn
  (ert-deftest add-to-dirs-cache::add-single-item ()
    (with-temp-buffer
      (let ((b (insert-text-button "b"))
            (p (insert-text-button "p"))
            (child  "/home/A/B")
            (treemacs--open-dirs-cache nil))
        (button-put p 'abs-path "/home/A")
        (button-put b 'abs-path "/home/A/B")
        (button-put b 'parent p)
        (treemacs--add-to-cache b)
        (should (equal '(("/home/A" "/home/A/B")) treemacs--open-dirs-cache)))))

  (ert-deftest add-to-dirs-cache::add-single-collapsed-item ()
    (with-temp-buffer
      (let ((b (insert-text-button "b"))
            (p (insert-text-button "p"))
            (child  "/home/A/B")
            (treemacs--open-dirs-cache nil))
        (button-put p 'abs-path "/home/A")
        (button-put b 'abs-path "/home/A/B")
        (button-put b 'parent p)
        (button-put b 'parent-path "/home/P")
        (treemacs--add-to-cache b)
        (should (equal '(("/home/P" "/home/A/B")) treemacs--open-dirs-cache)))))

  (ert-deftest add-to-dirs-cache::add-two-same-parent-items ()
    (with-temp-buffer
      (let ((p  (insert-text-button "p"))
            (b1 (insert-text-button "b1"))
            (b2 (insert-text-button "b2"))
            (treemacs--open-dirs-cache nil))
        (button-put p 'abs-path "/home/A")
        (button-put b1 'abs-path "/home/A/B1")
        (button-put b2 'abs-path "/home/A/B2")
        (button-put b1 'parent p)
        (button-put b2 'parent p)
        (treemacs--add-to-cache b1)
        (treemacs--add-to-cache b2)
        (should (equal '(("/home/A" "/home/A/B2" "/home/A/B1")) treemacs--open-dirs-cache)))))

  (ert-deftest add-to-dirs-cache::add-two-different-parent-items ()
    (with-temp-buffer
      (let ((p1 (insert-text-button "p1"))
            (p2 (insert-text-button "p2"))
            (b1 (insert-text-button "b1"))
            (b2 (insert-text-button "b2"))
            (treemacs--open-dirs-cache nil))
        (button-put p1 'abs-path "/A1")
        (button-put p2 'abs-path "/A2")
        (button-put b1 'abs-path "/A1/B1")
        (button-put b2 'abs-path "/A2/B2")
        (button-put b1 'parent p1)
        (button-put b2 'parent p2)
        (treemacs--add-to-cache b1)
        (treemacs--add-to-cache b2)
        (should (equal `(("/A2" "/A2/B2") ("/A1" "/A1/B1")) treemacs--open-dirs-cache)))))

  (ert-deftest add-to-dirs-cache::add-new-child-to-cache-with-multiple-items ()
    (with-temp-buffer
      (let ((p1 (insert-text-button "p1"))
            (p2 (insert-text-button "p2"))
            (b1 (insert-text-button "b1"))
            (b2 (insert-text-button "b2"))
            (b3 (insert-text-button "b3"))
            (treemacs--open-dirs-cache nil))
        (button-put p1 'abs-path "/A1")
        (button-put p2 'abs-path "/A2")
        (button-put b1 'abs-path "/A1/B1")
        (button-put b2 'abs-path "/A2/B2")
        (button-put b3 'abs-path "/A1/B3")
        (button-put b1 'parent p1)
        (button-put b2 'parent p2)
        (button-put b3 'parent p1)
        (treemacs--add-to-cache b1)
        (treemacs--add-to-cache b2)
        (treemacs--add-to-cache b3)
        (should (equal `(("/A2" "/A2/B2") ("/A1" "/A1/B3" "/A1/B1")) treemacs--open-dirs-cache))))))

;; `treemacs--clear-from-cache'
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

;; `treemacs--is-path-in-dir?'
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

;; `treemacs--get-face'
(progn
  (let ((treemacs-git-integration t))
    (ert-deftest get-face::unmodified-face-for-file-without-git-info ()
      (should (eq 'treemacs-git-unmodified-face (treemacs--get-face "~/A" nil))))

    (ert-deftest get-face::unmodified-face-for-file-without-fitting-git-info ()
      (let ((git-info '(("M" . "~/B") ("??" . "~A/b"))))
        (should (eq 'treemacs-git-unmodified-face (treemacs--get-face "~/A" git-info)))))

    (ert-deftest get-face::unmodified-face-for-file-without-fitting-git-info ()
      (let ((git-info '(("M" . "~/B") ("??" . "~A/b"))))
        (should (eq 'treemacs-git-unmodified-face (treemacs--get-face "~/A" git-info)))))

    (ert-deftest get-face::unmodified-face-for-file-without-fitting-git-status ()
      (let ((git-info '(("0" . "~/A"))))
        (should (eq 'treemacs-git-unmodified-face (treemacs--get-face "~/A" git-info)))))

    (ert-deftest get-face::modified-face-for-modified-file ()
      (let ((git-info '(("!!" . "~/A/B/c") ("M" . "~/A"))))
        (should (eq 'treemacs-git-modified-face (treemacs--get-face "~/A" git-info)))))

    (ert-deftest get-face::untracked-face-for-untracked-file ()
      (let ((git-info '(("!!" . "~/A/B/c") ("??" . "~/A"))))
        (should (eq 'treemacs-git-untracked-face (treemacs--get-face "~/A" git-info)))))

    (ert-deftest get-face::ignored-face-for-ignored-file ()
      (let ((git-info '(("!!" . "~/A/B/c") ("!!" . "~/A"))))
        (should (eq 'treemacs-git-ignored-face (treemacs--get-face "~/A" git-info)))))

    (ert-deftest get-face::added-face-for-added-file ()
      (let ((git-info '(("!!" . "~/A/B/c") ("A" . "~/A"))))
        (should (eq 'treemacs-git-added-face (treemacs--get-face "~/A" git-info)))))))

;; `treemacs--current-visibility'
(progn
  (ert-deftest current-visibility::visible-buffer ()
      (with-mock
        (mock (get-buffer-window treemacs--buffer-name) => t)
        (should (eq 'visible (treemacs--current-visibility)))))

  (ert-deftest current-visibility::visible-buffer-even-when-exists?-is-nil ()
    (with-mock
      (mock (get-buffer-window treemacs--buffer-name) => t)
      (stub -contains? => t)
      (should (eq 'visible (treemacs--current-visibility)))))

  (ert-deftest current-visibility::existing-buffer ()
    (with-mock
      (stub get-buffer-window => nil)
      (stub buffer-list => (list (get-buffer-create treemacs--buffer-name)))
      (should (eq 'exists (treemacs--current-visibility)))))

  (ert-deftest current-visibility::no-buffer ()
    (with-mock
      (mock (get-buffer-window treemacs--buffer-name) => nil)
      (stub buffer-list => (list))
      (should (eq 'none (treemacs--current-visibility))))))

;; `treemacs--unquote'
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

;; `treemacs--reject-ignored-files'
(progn
  (let ((treemacs-ignored-file-predicates (default-value 'treemacs-ignored-file-predicates)))

    (ert-deftest reject-ignored::fails-on-nil-file ()
      (should-error (treemacs--reject-ignored-files nil)))

    (ert-deftest reject-ignored::rejects-emacs-lock-file ()
      (should-not (treemacs--reject-ignored-files "~/A/B/C/.#foo.el")))

    (ert-deftest reject-ignored::rejects-emacs-backup-file ()
      (should-not (treemacs--reject-ignored-files "~/A/B/C/foo.el~")))

    (ert-deftest reject-ignored::rejects-emacs-autosave-file ()
      (should-not (treemacs--reject-ignored-files "~/A/B/C/#foo.el#")))

    (ert-deftest reject-ignored::rejects-flycheck-temp-file ()
      (should-not (treemacs--reject-ignored-files "~/A/B/C/flycheck_foo.el")))

    (ert-deftest reject-ignored::rejects-dot ()
      (should-not (treemacs--reject-ignored-files ".")))

    (ert-deftest reject-ignored::rejects-dot-dot ()
      (should-not (treemacs--reject-ignored-files "..")))

    (ert-deftest reject-ignored::accepts-dotfile ()
      (should (treemacs--reject-ignored-files "~/A/B/C/.foo.el")))

    (ert-deftest reject-ignored::accepts-std-file ()
      (should (treemacs--reject-ignored-files "~/A/B/C/foo.el")))

    (ert-deftest reject-ignored::accepts-empty-file ()
      (should (treemacs--reject-ignored-files "")))

    (ert-deftest reject-ignored::accepts-dir ()
      (should (treemacs--reject-ignored-files "~/A/B/C/")))

    (ert-deftest reject-ignored::accepts-abs-file ()
      (should (treemacs--reject-ignored-files "foo.el")))))

;; `treemacs--reject-ignored-and-dotfiles'
(progn
  (let ((treemacs-ignored-file-predicates (default-value 'treemacs-ignored-file-predicates)))

    (ert-deftest reject-ignored-and-dotfiles::fails-on-nil-file ()
      (should-error (treemacs--reject-ignored-and-dotfiles nil)))

    (ert-deftest reject-ignored-and-dotfiles::rejects-emacs-lock-file ()
      (should-not (treemacs--reject-ignored-and-dotfiles "~/A/B/C/.#foo.el")))

    (ert-deftest reject-ignored-and-dotfiles::rejects-emacs-backup-file ()
      (should-not (treemacs--reject-ignored-and-dotfiles "~/A/B/C/foo.el~")))

    (ert-deftest reject-ignored-and-dotfiles::rejects-emacs-autosave-file ()
      (should-not (treemacs--reject-ignored-and-dotfiles "~/A/B/C/#foo.el#")))

    (ert-deftest reject-ignored-and-dotfiles::rejects-flycheck-temp-file ()
      (should-not (treemacs--reject-ignored-and-dotfiles "~/A/B/C/flycheck_foo.el")))

    (ert-deftest reject-ignored-and-dotfiles::rejects-dotfile ()
      (should-not (treemacs--reject-ignored-and-dotfiles "~/A/B/C/.foo.el")))

    (ert-deftest reject-ignored-and-dotfiles::rejects-dot ()
      (should-not (treemacs--reject-ignored-and-dotfiles ".")))

    (ert-deftest reject-ignored-and-dotfiles::rejects-dot-dot ()
      (should-not (treemacs--reject-ignored-and-dotfiles "..")))

    (ert-deftest reject-ignored-and-dotfiles::accepts-std-file ()
      (should (treemacs--reject-ignored-and-dotfiles "~/A/B/C/foo.el")))

    (ert-deftest reject-ignored-and-dotfiles::accepts-std-file ()
      (should (treemacs--reject-ignored-and-dotfiles "~/A/B/C/foo.el")))

    (ert-deftest reject-ignored-and-dotfiles::accepts-std-file ()
      (should (treemacs--reject-ignored-and-dotfiles "~/A/B/C/foo.el")))

    (ert-deftest reject-ignored-and-dotfiles::accepts-empty-file ()
      (should (treemacs--reject-ignored-and-dotfiles "")))

    (ert-deftest reject-ignored-and-dotfiles::accepts-dir ()
      (should (treemacs--reject-ignored-and-dotfiles "~/A/B/C/")))

    (ert-deftest reject-ignored-and-dotfiles::accepts-abs-file ()
      (should (treemacs--reject-ignored-and-dotfiles "foo.el")))))

;; `str-assq-delete-all'
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

;; `treemacs--parent'
(progn
  (ert-deftest parent::fails-on-nil-path ()
    (should-error (treemacs--parent nil)))

  (ert-deftest parent::does-not-fail-on-empty-path ()
    (should (treemacs--parent "")))

  (ert-deftest parent::returns-parent ()
    (should (equal "/home/A" (treemacs--parent "/home/A/B"))))

  (ert-deftest parent::returns-root ()
    (should (equal "/" (treemacs--parent "/")))))

;; `treemacs--read-persist-data'
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

;; `treemacs--check-window-system'
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

;; `treemacs--is-event-relevant?'
(progn
  (let ((treemacs-ignored-file-predicates (default-value 'treemacs-ignored-file-predicates)))
    (ert-deftest file-event-relevance::stop-watching-is-not-relevant ()
          (should-not (treemacs--is-event-relevant? '(nil stopped "~/A/a"))))

    (ert-deftest file-event-relevance::file-change-without-git-is-not-relevant ()
      (let ((treemacs-git-integration))
        (should-not (treemacs--is-event-relevant? '(nil changed "~/A/a")))))

    (ert-deftest file-event-relevance::lockfile-event-is-not-relevant ()
      (should-not (treemacs--is-event-relevant? '(nil created "~/A/.#foo.el"))))

    (ert-deftest file-event-relevance::filecheck-file-event-is-not-relevant ()
      (should-not (treemacs--is-event-relevant? '(nil created "~/A/flycheck_foo.el"))))

    (ert-deftest file-event-relevance::file-change-with-git-is-relevant ()
      (let ((treemacs-git-integration t))
        (should (treemacs--is-event-relevant? '(nil changed "~/A/a")))))

    (ert-deftest file-event-relevance::file-creation-is-relevant ()
      (should (treemacs--is-event-relevant? '(nil created "~/A/a"))))

    (ert-deftest file-event-relevance::file-deletion-is-relevant ()
      (should (treemacs--is-event-relevant? '(nil deleted "~/A/a"))))))

;; `treemacs--file-extension'
(progn
  (ert-deftest file-ext::fails-on-nil-file ()
    (should-error (treemacs--file-extension nil)))

  (ert-deftest file-ext::empty-in-empty-out ()
    (should (equal "" (treemacs--file-extension ""))))

  (ert-deftest file-ext::empty-when-input-is-dot ()
    (should (equal "" (treemacs--file-extension "."))))

  (ert-deftest file-ext::empty-when-input-is-many-dots ()
    (should (equal "" (treemacs--file-extension "....."))))

  (ert-deftest file-ext::return-filename-for-path-without-extension ()
    (should (equal "foo" (treemacs--file-extension "/A/B/C/D/foo"))))

  (ert-deftest file-ext::return-filename-for-file-without-extension ()
    (should (equal "foo" (treemacs--file-extension "foo"))))

  (ert-deftest file-ext::return-extension-for-absolute-path ()
    (should (equal "el" (treemacs--file-extension "~/A/B/C/D/foo.el"))))

  (ert-deftest file-ext::return-extension-for-filename ()
    (should (equal "el" (treemacs--file-extension "foo.el"))))

  (ert-deftest file-ext::return-extension-for-path-with-dots ()
    (should (equal "el" (treemacs--file-extension "~/A/foo.bar/baz.qux/foo.el")))))

;; `treemacs--tags-path-of'
(progn
  (ert-deftest tags-path-of::fails-on-nil-btn ()
    (should-error (treemacs--tags-path-of nil)))

  (ert-deftest tags-path-of::directly-returns-path-when-possible ()
    (with-temp-buffer
      (let ((b (make-button 0 0)))
        (button-put b 'abs-path "/a/b/c")
        (should (equal '("/a/b/c") (treemacs--tags-path-of b))))))

  (ert-deftest tags-path-of::walks-up-to-the-first-file-button ()
    (with-temp-buffer
      (let ((b1 (button-at (insert-text-button "b1")))
            (b2 (button-at (insert-text-button "b2")))
            (b3 (button-at (insert-text-button "b3")))
            (b4 (button-at (insert-text-button "b4"))))
        (button-put b1 'parent b2)
        (button-put b2 'parent b3)
        (button-put b3 'parent b4)
        (button-put b4 'abs-path "A")
        (should (equal '("b1" "b3" "b2") (treemacs--tags-path-of b1)))))))

;; `treemacs--partition-imenu-index'
(progn
  (ert-deftest partition-index::returns-nil-on-nil-input ()
    (should-not (treemacs--partition-imenu-index nil "A")))

  (ert-deftest partition-index::returns-index-unchanged-when-input-has-no-top-level-leaves ()
    (let ((input '(("A" ("a1" "a2")) ("B" ("b1" "b2")))))
      (should (equal input (treemacs--partition-imenu-index '(("A" ("a1" "a2")) ("B" ("b1" "b2"))) "Functions")))))

  (ert-deftest partition-index::partitions-single-top-level-list-into-functions ()
    (should (equal
             '(("Functions" ("x" "y" "z")))
             (treemacs--partition-imenu-index '(("x" "y" "z")) "Functions"))))

  (ert-deftest partition-index::partitions-top-level-tails-into-functions ()
    (let ((input '(("A" ("a1" "a2")) ("B" ("b1" "b2")) ("x" "y" "z"))))
      (should (equal
               '(("A" ("a1" "a2")) ("B" ("b1" "b2")) ("Functions" ("x" "y" "z")))
               (treemacs--partition-imenu-index input "Functions"))))))

;; `treemacs--add-to-tags-cache'
(progn
  (ert-deftest add-to-tags-cache::fails-on-nil-btn ()
    (should-error (treemacs--add-to-tags-cache nil)))

  (ert-deftest add-to-tags-cache::creates-new-table-if-one-doesnt-yet-exist ()
    (with-temp-buffer
      (let ((treemacs--tags-cache (make-hash-table :test #'equal))
            (b (button-at (insert-text-button "b")))
            (p (button-at (insert-text-button "p"))))
        (button-put p 'abs-path "/A/B/C")
        (button-put b 'parent p)
        (treemacs--add-to-tags-cache b)
        (should-not (null (gethash "/A/B/C" treemacs--tags-cache))))))

  (ert-deftest add-to-tags-cache::creates-new-cache-entry-if-one-doesnt-yet-exist ()
    (with-temp-buffer
      (let ((treemacs--tags-cache (make-hash-table :test #'equal))
            (b (button-at (insert-text-button "button-lbl")))
            (p (button-at (insert-text-button "parent-lbl"))))
        (button-put p 'abs-path "/parent/path")
        (button-put b 'parent p)
        (treemacs--add-to-tags-cache b)
        (should (equal '(("button-lbl"))
                       (gethash '("/parent/path") (gethash "/parent/path" treemacs--tags-cache)))))))

  (ert-deftest add-to-tags-cache::adds-to-existing-cache-entry ()
    (with-temp-buffer
      (let ((treemacs--tags-cache (make-hash-table :test #'equal))
            (b1 (button-at (insert-text-button "button-lbl-1")))
            (b2 (button-at (insert-text-button "button-lbl-2")))
            (p (button-at (insert-text-button "parent-lbl"))))
        (button-put p 'abs-path "/parent/path")
        (button-put b1 'parent p)
        (button-put b2 'parent p)
        (treemacs--add-to-tags-cache b1)
        (treemacs--add-to-tags-cache b2)
        (should (equal '(("button-lbl-2") ("button-lbl-1"))
                       (gethash '("/parent/path") (gethash "/parent/path" treemacs--tags-cache)))))))

  (ert-deftest add-to-tags-cache::adds-to-existing-cache-entry-in-long-tag-path ()
    (with-temp-buffer
      (let ((treemacs--tags-cache (make-hash-table :test #'equal))
            (b1 (button-at (insert-text-button "button-lbl-1")))
            (b2 (button-at (insert-text-button "button-lbl-2")))
            (p1 (button-at (insert-text-button "parent-lbl-1")))
            (p2 (button-at (insert-text-button "parent-lbl-2")))
            (p3 (button-at (insert-text-button "parent-lbl-3")))
            (p4 (button-at (insert-text-button "parent-lbl-4"))))
        (button-put b1 'parent p1)
        (button-put b2 'parent p1)
        (button-put p1 'parent p2)
        (button-put p2 'parent p3)
        (button-put p3 'parent p4)
        (button-put p4 'abs-path "/parent/path")
        (treemacs--add-to-tags-cache b1)
        (treemacs--add-to-tags-cache b2)
        (should
         (equal '(("button-lbl-2" "parent-lbl-3" "parent-lbl-2" "parent-lbl-1")
                  ("button-lbl-1" "parent-lbl-3" "parent-lbl-2" "parent-lbl-1"))
                (gethash '("parent-lbl-1" "parent-lbl-3" "parent-lbl-2")
                         (gethash "/parent/path" treemacs--tags-cache))))))))

;; `treemacs--remove-from-tags-cache'
(progn
  (ert-deftest remove-from-tags-cache::fails-on-nil-btn ()
    (should-error (treemacs--remove-from-tags-cache nil)))

  (ert-deftest remove-from-tags-cache::fully-remove-cache-list ()
    (with-temp-buffer
      (let ((treemacs--tags-cache (make-hash-table :test #'equal))
            (b (button-at (insert-text-button "b")))
            (p (button-at (insert-text-button "p"))))
        (button-put p 'abs-path "/A/B/C")
        (button-put b 'parent p)
        (treemacs--add-to-tags-cache b)
        (treemacs--remove-from-tags-cache b)
        (should (hash-table-empty-p (gethash "/A/B/C" treemacs--tags-cache))))))

  (ert-deftest remove-from-tags-cache::remove-single-entry-from-cache-list ()
    (with-temp-buffer
      (let ((treemacs--tags-cache (make-hash-table :test #'equal))
            (b1 (button-at (insert-text-button "b1")))
            (b2 (button-at (insert-text-button "b2")))
            (p  (button-at (insert-text-button "p"))))
        (button-put p 'abs-path "/A/B/C")
        (button-put b1 'parent p)
        (button-put b2 'parent p)
        (treemacs--add-to-tags-cache b1)
        (treemacs--add-to-tags-cache b2)
        (treemacs--remove-from-tags-cache b2)
        (should (equal '(("b1")) (gethash '("/A/B/C") (gethash "/A/B/C" treemacs--tags-cache)))))))

  (ert-deftest remove-from-tags-cache::correctly-remove-entry-under-section ()
    (with-temp-buffer
      (let ((treemacs--tags-cache (make-hash-table :test #'equal))
            (b1 (button-at (insert-text-button "b1")))
            (b2 (button-at (insert-text-button "b2")))
            (b3 (button-at (insert-text-button "b3")))
            (p  (button-at (insert-text-button "p"))))
        (button-put p 'abs-path "/A/B/C")
        (button-put b1 'parent p)
        (button-put b2 'parent b1)
        (button-put b3 'parent b2)
        (treemacs--add-to-tags-cache b1)
        (treemacs--add-to-tags-cache b2)
        (treemacs--remove-from-tags-cache b1)
        (should-not (gethash '((b1)) (gethash "/A/B/C" treemacs--tags-cache)))))))

;; `treemacs--tags-path-of'
(progn
  (ert-deftest tags-path::fails-on-nil-btn ()
    (should-error (treemacs--tags-path-of nil)))

  (ert-deftest tags-path::returns-abs-path-for-non-tag-buttons ()
    (with-temp-buffer
      (let ((b (insert-text-button "b")))
        (button-put b 'abs-path "/A/B/C/")
        (should (equal '("/A/B/C/") (treemacs--tags-path-of b))))))

  (ert-deftest tags-path::returns-label-for-depth-1-button ()
    (with-temp-buffer
      (let ((p (insert-text-button "p"))
            (b (insert-text-button "label")))
        (button-put p 'abs-path "/A/B/C/")
        (button-put b 'parent p)
        (should (equal '("label") (treemacs--tags-path-of b))))))

  (ert-deftest tags-path::returns-full-path-for-deeply-nested-button ()
    (with-temp-buffer
      (let ((b1 (insert-text-button "b1"))
            (b2 (insert-text-button "b2"))
            (b3 (insert-text-button "b3"))
            (b4 (insert-text-button "b4"))
            (b5 (insert-text-button "b5")))
        (button-put b5 'parent b4)
        (button-put b4 'parent b3)
        (button-put b3 'parent b2)
        (button-put b2 'parent b1)
        (button-put b2 'parent b1)
        (button-put b1 'abs-path "/A/B/C")
        (should (equal '("b5" "b2" "b3" "b4") (treemacs--tags-path-of b5)))))))

;; `treemacs--next-non-child-node'
(progn
  (ert-deftest next-non-child::returns-nil-for-nil-input ()
    (should-not (treemacs--next-non-child-node nil)))

  (ert-deftest next-non-child::returns-nil-for-single-button ()
    (with-temp-buffer
      (let ((b (insert-text-button "b")))
        (should-not (treemacs--next-non-child-node b)))))

  (ert-deftest next-non-child::directly-retuns-next-btn-property ()
    (with-temp-buffer
      (let ((b1 (insert-text-button "b1"))
            (b2 (insert-text-button "b2")))
        (button-put b1 'next-node b2)
        (should (equal b2 (treemacs--next-non-child-node b1))))))

  (ert-deftest next-non-child::searches-through-parent-hierarchy ()
    (with-temp-buffer
      (let ((b1 (insert-text-button "b1"))
            (b2 (insert-text-button "b2"))
            (b3 (insert-text-button "b3"))
            (b4 (insert-text-button "b4"))
            (b5 (insert-text-button "b5"))
            (b6 (insert-text-button "b6")))
        (button-put b1 'parent b2)
        (button-put b2 'parent b3)
        (button-put b3 'parent b4)
        (button-put b4 'parent b5)
        (button-put b5 'next-node b6)
        (should (equal b6 (treemacs--next-non-child-node b1)))))))

;; `treemacs--update-caches-after-rename'
(progn
  (ert-deftest update-caches-after-rename::update-runs-correctly ()
    (let ((treemacs--open-dirs-cache)
          (treemacs--tags-cache (make-hash-table :size 100 :test #'equal)))
      (push '("/old/name/A" "/old/name/A/B1" "/old/name/A/B2") treemacs--open-dirs-cache)
      (push '("/other/A" "/other/A/B1" "/other/A/B2") treemacs--open-dirs-cache)
      (let ((hash (make-hash-table :test #'equal)))
        (puthash (list "/old/name/A") '(("/old/name/A/B" "/old/name/A/B/C") ("/X" "/X/Y")) hash)
        (puthash (list "/other/A") '(("/other/A/B" "/other/A/B/C")) hash)
        (puthash "/X" hash treemacs--tags-cache))
      (let ((hash (make-hash-table :test #'equal)))
        (puthash (list "/other/A") '(("/other/A/B" "/other/A/B/C")) hash)
        (puthash "/old/name" hash treemacs--tags-cache))
      (treemacs--update-caches-after-rename "/old/name" "/new/name")
      (should (equal treemacs--open-dirs-cache
                     '(("/other/A" "/other/A/B1" "/other/A/B2")
                       ("/new/name/A" "/new/name/A/B1" "/new/name/A/B2"))))
      (should-not (gethash "/old/name" treemacs--tags-cache))
      (should (equal (gethash (list "/other/A") (gethash "/new/name" treemacs--tags-cache))
                     '(("/other/A/B" "/other/A/B/C"))))
      (should (equal (gethash (list "/other/A") (gethash "/X" treemacs--tags-cache))
                     '(("/other/A/B" "/other/A/B/C"))))
      (should (equal (gethash (list "/new/name/A") (gethash "/X" treemacs--tags-cache))
                     '(("/old/name/A/B" "/old/name/A/B/C") ("/X" "/X/Y"))))
      (should-not (gethash (list "/old/name/A") (gethash "/X" treemacs--tags-cache))))))

;; `treemacs--flatten-imenu-index'
(progn
  (ert-deftest flatten-imenu::does-nothing-on-nil-input ()
    (should-not (treemacs--flatten-imenu-index nil)))

  (ert-deftest flatten-imenu::does-nothing-on-empty-input ()
    (should-not (treemacs--flatten-imenu-index (list))))

  (ert-deftest flatten-imenu::correctly-parses-single-item ()
    (should (equal '(("Functions")) (treemacs--flatten-imenu-index '("Functions")))))

  (ert-deftest flatten-imenu::correctly-parses-full-list ()
    (let* ((input '(("Functions" ("f1") ("f2"))
                    ("Types"     (("t1") ("t2")))
                    ("Classes"   (("c1" ("Members" ("m1") ("m2")))))))
      (should (equal '((("f2") "Functions")
                       (("f1") "Functions")
                       (("t1") "Types")
                       (("t2") "Types")
                       (("m1") "Members" "Classes")
                       (("m2") "Members" "Classes"))
                     (with-no-warnings (treemacs--flatten-imenu-index input))))))))

;; `treemacs--flatten-org-mode-imenu-index'
(progn
  (ert-deftest flatten-org-imenu::does-nothing-on-nil-input ()
    (should-not (treemacs--flatten-org-mode-imenu-index nil)))

  (ert-deftest flatten-org-imenu::does-nothing-on-empty-input ()
    (should-not (treemacs--flatten-org-mode-imenu-index (list))))

  (ert-deftest flatten-org-imenu::correctly-parses-single-item ()
    (should (equal '(("Functions")) (treemacs--flatten-org-mode-imenu-index '("Functions")))))

  (ert-deftest flatten-org-imenu::correctly-parses-full-list ()
    (let* ((input '(("Functions" ("f1") ("f2"))
                    ("Types"     (("t1") ("t2")))
                    ("Classes"   (("c1" ("Members" ("m1") ("m2")))))))
           (should (equal '(("Functions")
                            (("f2") "Functions")
                            (("f1") "Functions")
                            ("Types")
                            (("t1") "Types")
                            (("t2") "Types")
                            ("c1" "Classes")
                            ("Members" "Classes" "c1")
                            (("m1") "Members" "Classes")
                            (("m2") "Members" "Classes"))
                          (with-no-warnings (treemacs--flatten-imenu-index input))))))))

;; `treemacs--find-index-pos'
(progn
  (ert-deftest find-index::error-in-nil-point ()
    (should-error (treemacs--find-index-pos nil '((("A" . (make-marker)))))))

  (ert-deftest find-index::nil-on-empty-list ()
    (should-not (treemacs--find-index-pos 1 nil)))

  (ert-deftest find-index::find-index-before-first-marker ()
    (let ((input `((("A" . ,(move-marker (make-marker) 10)))
                   (("B" . ,(move-marker (make-marker) 20)))
                   (("C" . ,(move-marker (make-marker) 30))))))
      (should (equal (car input) (treemacs--find-index-pos 1 input)))))

  (ert-deftest find-index::find-index-after-last-marker ()
    (let ((input `((("A" . ,(move-marker (make-marker) 10)))
                   (("B" . ,(move-marker (make-marker) 20)))
                   (("C" . ,(move-marker (make-marker) 30))))))
      (should (equal (nth 2 input) (treemacs--find-index-pos 100 input)))))

  (ert-deftest find-index::find-index-with-binary-search ()
    (with-temp-buffer
      ;; make those markers viable
      (dotimes (_ 10) (insert "            \n"))
      (let ((input `((("A" . ,(move-marker (make-marker) 10)))
                     (("B" . ,(move-marker (make-marker) 20)))
                     (("C" . ,(move-marker (make-marker) 30)))
                     (("D" . ,(move-marker (make-marker) 40)))
                     (("E" . ,(move-marker (make-marker) 50)))
                     (("F" . ,(move-marker (make-marker) 60)))
                     (("G" . ,(move-marker (make-marker) 70)))
                     (("H" . ,(move-marker (make-marker) 80)))
                     (("I" . ,(move-marker (make-marker) 90))))))
        (should (equal (nth 6 input) (treemacs--find-index-pos 72 input)))))))

;; `treemacs--flatten&sort-imenu-index'
(progn
  (ert-deftest flatten&sort::correctly-transform-org-mode-index ()
    (let ((org-imenu-depth 10)
          (temp-file (make-temp-file "Treemacs Test")))
      (unwind-protect
          (progn
            (find-file-noselect temp-file)
            (with-current-buffer (get-file-buffer temp-file)
              (insert "* H1\n")
              (insert "** H1.2\n")
              (insert "*** H1.2.3\n")
              (insert "* H2\n")
              (org-mode)
              (save-buffer)
              (should (equal (treemacs--flatten&sort-imenu-index)
                             `((("H1" . ,(move-marker (make-marker) 1)))
                               (("H1.2" . ,(move-marker (make-marker) 6)) "H1")
                               (("H1.2.3" . ,(move-marker (make-marker) 14)) "H1" "H1.2")
                               (("H2" . ,(move-marker (make-marker) 25))))))))
        (progn
          (kill-buffer (get-file-buffer temp-file))
          (delete-file temp-file))))))

;;; Thorough Sys Test
(ert-deftest treemacs::sys-test ()
  (save-window-excursion
    (save-match-data
      (let ((test-buffer (get-buffer-create "*Treemacs Sys Test*"))
            (imenu-auto-rescan t)
            (treemacs-collapse-dirs 3))
        (if noninteractive
            (message "Sys Test not run in batch mode.")
          (unwind-protect
              (progn
                (switch-to-buffer test-buffer)
                (delete-other-windows)
                (-when-let (b (get-buffer treemacs--buffer-name)) (kill-buffer b))
                (call-interactively 'treemacs)

                (let ((b (get-buffer treemacs--buffer-name)))
                  (should b)
                  (switch-to-buffer b))

                (treemacs--goto-button-at (f-join default-directory "test/testfolder1/testfolder2"))
                (should (equal "test/testfolder1/testfolder2"
                               (treemacs--get-label-of
                                (treemacs--current-button))))

                (call-interactively 'treemacs-change-root)
                (should (equal "testfile.el"
                               (treemacs--get-label-of (treemacs--current-button))))

                (call-interactively 'treemacs-push-button)

                (should (equal '("Variables" "Functions")
                               (-map #'treemacs--get-label-of
                                     (treemacs--get-children-of (treemacs--current-button)))))

                (call-interactively 'treemacs-next-line)
                (call-interactively 'treemacs-push-button)
                (call-interactively 'treemacs-next-neighbour)
                (call-interactively 'treemacs-push-button)

                (should (equal '("fn1" "fn2")
                               (-map #'treemacs--get-label-of
                                     (treemacs--get-children-of (treemacs--current-button)))))

                (call-interactively 'treemacs-next-line)
                (call-interactively 'treemacs-visit-node-no-split)

                (should (equal "testfile.el" (f-filename buffer-file-name)))
                (should (equal "(defun fn1 ())\n" (thing-at-point 'line t)))
                (should (equal 2 (length (window-list))))

                (call-interactively 'treemacs-select-window)
                (call-interactively 'treemacs-goto-parent-node)
                (call-interactively 'treemacs-goto-parent-node)
                (call-interactively 'treemacs-next-neighbour)
                (call-interactively 'treemacs-push-button)

                (should (equal '("Foo" "Bar")
                               (-map #'treemacs--get-label-of
                                     (treemacs--get-children-of (treemacs--current-button)))))

                (call-interactively 'treemacs-next-line)
                (call-interactively 'treemacs-push-button)
                (call-interactively 'treemacs-next-line)
                (call-interactively 'treemacs-push-button)
                (call-interactively 'treemacs-next-line)

                (should (equal "Foo3" (treemacs--get-label-of (treemacs--current-button))))

                (funcall #'treemacs-visit-node-vertical-split t)
                (should (and (equal major-mode 'treemacs-mode)
                             (equal 3 (length (window-list)))
                             (equal 1 (length (--filter (with-current-buffer (window-buffer it)
                                                          (equal major-mode 'org-mode))
                                                        (window-list))))))

                (dotimes (_ 3) (call-interactively 'treemacs-goto-parent-node))
                (dotimes (_ 2) (call-interactively 'treemacs-push-button))
                (dotimes (_ 3) (call-interactively 'treemacs-next-line))
                (should (equal "Foo3" (treemacs--get-label-of (treemacs--current-button)))))

            (kill-buffer test-buffer)))))))

(provide 'treemacs-tests)

;;; treemacs-tests.el ends here
