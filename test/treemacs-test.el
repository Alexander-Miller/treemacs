;;; treemacs-test.el --- Tests for treemacs

;; Copyright (C) 2018 Alexander Miller

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
(require 'pfuture)
(require 'dash)
(require 'org)
(require 'filenotify)
(require 'ert)
(require 'el-mock)
(require 'subr-x)
(require 'f)
(require 'ht)

;; `treemacs--maybe-filter-dotfiles'
(progn
  (ert-deftest filter-dotfiles::do-nothing-when-dotfiles-are-shown ()
    (with-mock
      (stub treemacs--find-project-for-path => "/home/")
      (let ((treemacs-show-hidden-files t)
            (input '("/home/.A" "/home/B/C" "/home/.A/B" "/home/.A/.B/.C")))
        (should (equal input (treemacs--maybe-filter-dotfiles input))))))

  (ert-deftest filter-dotfiles::do-nothing-for-nulls ()
    (with-mock
      (stub treemacs--find-project-for-path => "/home/")
      (-let [treemacs-show-hidden-files nil]
        (should (null (treemacs--maybe-filter-dotfiles nil))))))

  (ert-deftest filter-dotfiles::do-nothing-for-empty-input ()
    (with-mock
      (stub treemacs--find-project-for-path => "/home/")
      (-let [treemacs-show-hidden-files nil]
        (should (null (treemacs--maybe-filter-dotfiles '()))))))

  (ert-deftest filter-dotfiles::filter-single-dotfile ()
    (with-mock
      (stub treemacs--find-project-for-path => "/home/")
      (let ((treemacs-show-hidden-files nil)
            (input '("/home/A/B/C/D/.d")))
        (should (null (treemacs--maybe-filter-dotfiles input))))))

  (ert-deftest filter-dotfiles::filter-dotfile-based-on-parent ()
    (with-mock
      (stub treemacs--find-project-for-path => "/home/")
      (let ((treemacs-show-hidden-files nil)
            (input '("/home/A/B/C/.D/d")))
        (should (null (treemacs--maybe-filter-dotfiles input))))))

  (ert-deftest filter-dotfiles::dont-filter-dotfile-above-root ()
    (with-mock
      (stub treemacs--find-project-for-path => "/home/.A/B")
      (let ((treemacs-show-hidden-files nil)
            (input '("/home/.A/B/C/d")))
        (should (equal input (treemacs--maybe-filter-dotfiles input))))))

  (ert-deftest filter-dotfiles::filter-long-input ()
    (with-mock
      (stub treemacs--find-project-for-path => "/home/.A/B")
      (let ((treemacs-show-hidden-files nil)
            (input '("/home/.A/B/C/d" "/home/.A/B/.C/D/E" "/home/.A/B/C/.d" "/home/.A/B/C/D/E")))
        (should (equal '("/home/.A/B/C/d" "/home/.A/B/C/D/E") (treemacs--maybe-filter-dotfiles input)))))))

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

;; `treemacs-current-visibility'
(progn
  (ert-deftest current-visibility::visible-buffer ()
      (with-mock
        (stub s-starts-with? => t)
        (should (eq 'visible (treemacs-current-visibility)))))

  (ert-deftest current-visibility::visible-buffer-even-when-exists?-is-nil ()
    (with-mock
      (stub s-starts-with? => t)
      (stub assq => '(t . t))
      (should (eq 'visible (treemacs-current-visibility)))))

  (ert-deftest current-visibility::existing-buffer ()
    (with-mock
      (stub s-starts-with? => nil)
      (stub assq => `(t . ,(current-buffer)))
      (should (eq 'exists (treemacs-current-visibility)))))

  (ert-deftest current-visibility::no-buffer ()
    (with-mock
      (stub s-starts-with? => nil)
      (stub assq => '(nil . nil))
      (should (eq 'none (treemacs-current-visibility))))))

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

;; `treemacs--parent'
(progn
  (ert-deftest parent::fails-on-nil-path ()
    (should-error (treemacs--parent nil)))

  (ert-deftest parent::fails-on-empty-path ()
    (should-not (treemacs--parent "")))

  (ert-deftest parent::fails-on-invalid-path ()
    (should-not (treemacs--parent "ABC")))

  (ert-deftest parent::returns-parent ()
    (should (equal "/home/A" (treemacs--parent "/home/A/B"))))

  (ert-deftest parent::returns-root ()
    (should (equal "/" (treemacs--parent "/")))))

;; `treemacs--is-event-relevant?'
(progn
  (let ((treemacs-ignored-file-predicates (default-value 'treemacs-ignored-file-predicates)))
    (ert-deftest file-event-relevance::stop-watching-is-not-relevant ()
          (should-not (treemacs--is-event-relevant? '(nil stopped "~/A/a"))))

    (ert-deftest file-event-relevance::file-change-without-git-is-not-relevant ()
      (let ((treemacs-git-mode nil))
        (should-not (treemacs--is-event-relevant? '(nil changed "~/A/a")))))

    (ert-deftest file-event-relevance::lockfile-event-is-not-relevant ()
      (should-not (treemacs--is-event-relevant? '(nil created "~/A/.#foo.el"))))

    (ert-deftest file-event-relevance::filecheck-file-event-is-not-relevant ()
      (should-not (treemacs--is-event-relevant? '(nil created "~/A/flycheck_foo.el"))))

    (ert-deftest file-event-relevance::file-change-with-git-is-relevant ()
      (let ((treemacs-git-mode t))
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

;; `treemacs--tags-path-of'
(progn
  (ert-deftest tags-path::fails-on-nil-btn ()
    (should-error (treemacs--tags-path-of nil)))

  (ert-deftest tags-path::returns-abs-path-for-non-tag-buttons ()
    (with-temp-buffer
      (let ((b (insert-text-button "b")))
        (button-put b :path "/A/B/C")
        (should (equal "/A/B/C" (treemacs--tags-path-of b))))))

  (ert-deftest tags-path::returns-path-and-label-for-depth-1-button ()
    (with-temp-buffer
      (let ((p (insert-text-button "p"))
            (b (insert-text-button "label")))
        (button-put p :path "/A/B/C")
        (button-put b :parent p)
        (should (equal '("label" "/A/B/C") (treemacs--tags-path-of b))))))

  (ert-deftest tags-path::returns-full-path-for-deeply-nested-button ()
    (with-temp-buffer
      (let ((b1 (insert-text-button "b1"))
            (b2 (insert-text-button "b2"))
            (b3 (insert-text-button "b3"))
            (b4 (insert-text-button "b4"))
            (b5 (insert-text-button "b5")))
        (button-put b5 :parent b4)
        (button-put b4 :parent b3)
        (button-put b3 :parent b2)
        (button-put b2 :parent b1)
        (button-put b2 :parent b1)
        (button-put b1 :path "/A/B/C")
        (should (equal '("b5" "/A/B/C" "b2" "b3" "b4") (treemacs--tags-path-of b5)))))))

;; `treemacs--next-non-child-button'
(progn
  (ert-deftest next-non-child::returns-nil-for-nil-input ()
    (should-not (treemacs--next-non-child-button nil)))

  (ert-deftest next-non-child::returns-nil-for-single-button ()
    (with-temp-buffer
      (let ((b (insert-text-button "b")))
        (should-not (treemacs--next-non-child-button b)))))

  (ert-deftest next-non-child::directly-retuns-next-button ()
    (with-temp-buffer
      (let ((b1 (insert-text-button "b1"))
            (b2 (insert-text-button "b2")))
        (button-put b1 :depth 1)
        (button-put b2 :depth 1)
        (should (equal b2 (marker-position (treemacs--next-non-child-button b1)))))))

  (ert-deftest next-non-child::searches-through-higher-depth-buttons ()
    (with-temp-buffer
      (let ((b1 (insert-text-button "b1"))
            (b2 (insert-text-button "b2"))
            (b3 (insert-text-button "b3"))
            (b4 (insert-text-button "b4"))
            (b5 (insert-text-button "b5"))
            (b6 (insert-text-button "b6")))
        (button-put b1 :depth 1)
        (button-put b2 :depth 2)
        (button-put b3 :depth 3)
        (button-put b4 :depth 4)
        (button-put b5 :depth 5)
        (button-put b6 :depth 1)
        (should (equal b6 (marker-position (treemacs--next-non-child-button b1)))))))

  (ert-deftest next-non-child::returns-nil-when-there-is-no-next-non-child ()
    (with-temp-buffer
      (let ((b1 (insert-text-button "b1"))
            (b2 (insert-text-button "b2"))
            (b3 (insert-text-button "b3"))
            (b4 (insert-text-button "b4"))
            (b5 (insert-text-button "b5"))
            (b6 (insert-text-button "b6")))
        (button-put b1 :depth 1)
        (button-put b2 :depth 2)
        (button-put b3 :depth 3)
        (button-put b4 :depth 4)
        (button-put b5 :depth 5)
        (button-put b6 :depth 6)
        (should-not (treemacs--next-non-child-button b1))))))

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

;; `treemacs--find-project-for-path'
(progn
  (ert-deftest project-for-path::returns-nil-on-nil-input ()
    (let* ((project (make-treemacs-project :path "/A"))
           (treemacs-current-workspace (make-treemacs-workspace :projects (list project))))
      (should-not (treemacs--find-project-for-path nil))))

  (ert-deftest project-for-path::returns-nil-on-empty-workspace ()
    (-let [treemacs-current-workspace (make-treemacs-workspace :projects nil)]
      (should-not (treemacs--find-project-for-path "/A"))))

  (ert-deftest project-for-path::returns-nil-when-path-does-not-fit ()
    (let* ((project (make-treemacs-project :path "/A/B"))
           (treemacs-current-workspace (make-treemacs-workspace :projects (list project))))
      (should-not (treemacs--find-project-for-path "/A/C"))))

  (ert-deftest project-for-path::returns-project-when-path-fits ()
    (let* ((project (make-treemacs-project :path "/A/B"))
           (treemacs-current-workspace (make-treemacs-workspace :projects (list project))))
      (should (equal project (treemacs--find-project-for-path "/A/B/C"))))))

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

;; `treemacs--start-watching'
(when file-notify--library
  (ert-deftest start-watching::start-watching-unwatched-file ()
    (with-mock
     (stub file-notify-add-watch => 123456)
     (let ((path "/A")
           (treemacs-filewatch-mode t)
           (treemacs--filewatch-index (make-hash-table :test #'equal))
           (treemacs--collapsed-filewatch-index (make-hash-table :test #'equal)))
       (treemacs--start-watching path t)
       (should (equal (gethash path treemacs--filewatch-index)
                      (cons (list (current-buffer)) 123456)))
       (should (gethash path treemacs--collapsed-filewatch-index)))))

  (ert-deftest start-watching::start-watching-watched-file ()
    (with-mock
      (stub file-notify-add-watch => 123456)
      (let ((path "/A")
            (treemacs-filewatch-mode t)
            (treemacs--filewatch-index (make-hash-table :test #'equal))
            (treemacs--collapsed-filewatch-index (make-hash-table :test #'equal)))
        (puthash path (cons '(x y) 123456) treemacs--filewatch-index)
        (treemacs--start-watching path t)
        (should (equal (gethash path treemacs--filewatch-index)
                       (cons (list (current-buffer) 'x 'y) 123456)))
        (should (gethash path treemacs--collapsed-filewatch-index)))))

  (ert-deftest start-watching::add-watching-buffer-only-once ()
    (with-mock
      (stub file-notify-add-watch => 123456)
      (let ((path "/A")
            (treemacs-filewatch-mode t)
            (treemacs--filewatch-index (make-hash-table :test #'equal))
            (treemacs--collapsed-filewatch-index (make-hash-table :test #'equal)))
        (puthash path (cons '(x y) 123456) treemacs--filewatch-index)
        (treemacs--start-watching path t)
        (treemacs--start-watching path t)
        (should (equal (gethash path treemacs--filewatch-index)
                       (cons (list (current-buffer) 'x 'y) 123456)))
        (should (gethash path treemacs--collapsed-filewatch-index))))))

;; `treemacs--stop-watching'
(when file-notify--library

  (ert-deftest stop-watching::do-nothing-when-path-is-not-watched ()
    (let ((treemacs--filewatch-index (make-hash-table :test #'equal))
          (treemacs--collapsed-filewatch-index (make-hash-table :test #'equal)))
      (treemacs--stop-watching "/A")))

  (ert-deftest stop-watching::stop-watch-of-the-only-watching-buffer ()
    (with-mock
      (stub file-notify-rm-watch => t)
      (let ((path "/A")
            (treemacs--filewatch-index (make-hash-table :test #'equal))
            (treemacs--collapsed-filewatch-index (make-hash-table :test #'equal)))
        (puthash path (cons (list (current-buffer)) 123456) treemacs--filewatch-index)
        (puthash path t treemacs--collapsed-filewatch-index)
        (treemacs--stop-watching path)
        (should-not (gethash path treemacs--filewatch-index))
        (should-not (gethash path treemacs--collapsed-filewatch-index)))))

  (ert-deftest stop-watching::stop-watch-of-one-of-several-buffers ()
    (let ((path "/A")
          (treemacs--filewatch-index (make-hash-table :test #'equal))
          (treemacs--collapsed-filewatch-index (make-hash-table :test #'equal)))
      (puthash path (cons (list 'x 'y (current-buffer)) 123456) treemacs--filewatch-index)
      (puthash path t treemacs--collapsed-filewatch-index)
      (treemacs--stop-watching path)
      (should (equal (gethash path treemacs--filewatch-index)
                     (cons '(x y) 123456)))
      (should (gethash path treemacs--collapsed-filewatch-index))))

  (ert-deftest stop-watching::stop-watch-of-path-under-stopped-path ()
    (with-mock
      (stub file-notify-rm-watch => t)
      (let ((path "/A/B")
            (treemacs--filewatch-index (make-hash-table :test #'equal))
            (treemacs--collapsed-filewatch-index (make-hash-table :test #'equal)))
        (puthash path (cons (list (current-buffer)) 123456) treemacs--filewatch-index)
        (puthash path t treemacs--collapsed-filewatch-index)
        (treemacs--stop-watching "/A")
        (should-not (gethash path treemacs--filewatch-index))
        (should-not (gethash path treemacs--collapsed-filewatch-index)))))

  (ert-deftest stop-watching::stop-watch-of-all-buffers ()
    (with-mock
      (stub file-notify-rm-watch => t)
      (let ((path "/A")
            (treemacs--filewatch-index (make-hash-table :test #'equal))
            (treemacs--collapsed-filewatch-index (make-hash-table :test #'equal)))
        (puthash path (cons '(x y z) 123456) treemacs--filewatch-index)
        (puthash path t treemacs--collapsed-filewatch-index)
        (treemacs--stop-watching path t)
        (should-not (gethash path treemacs--filewatch-index))
        (should-not (gethash path treemacs--collapsed-filewatch-index))))))

;; `treemacs-on-expand'
(progn
  (ert-deftest on-expand::does-nothing-on-nil-arguments ()
    (with-temp-buffer
      (setq treemacs-shadow-index (ht))
      (should-not (treemacs-on-expand "A" 1 nil))
      (should-not (treemacs-on-expand "A" nil "B"))
      (should-not (treemacs-on-expand nil 1 "B"))))

  (ert-deftest on-expand::correctly-expands-new-node ()
    (with-temp-buffer
      (let* ((default-directory "/A")
             (treemacs-shadow-index (ht))
             (root (progn
                     (ht-set! treemacs-shadow-index default-directory
                              (make-treemacs-shadow-node :key default-directory))
                     (treemacs-get-from-shadow-index default-directory))))
        (treemacs-on-expand "/A/B" 22 "/A")
        (-let [node (treemacs-get-from-shadow-index "/A/B")]
          (should (= 2 (ht-size treemacs-shadow-index)))
          (should (equal (list node) (treemacs-shadow-node->children root)))
          (should (= 22 (treemacs-shadow-node->position node)))
          (should (equal root (treemacs-shadow-node->parent node)))))))

  (ert-deftest on-expand::correctly-expands-previously-open-node ()
    (with-temp-buffer
      (let* ((default-directory "/A")
             (treemacs-shadow-index (ht))
             (root (progn
                     (ht-set! treemacs-shadow-index default-directory
                              (make-treemacs-shadow-node :key default-directory))
                     (treemacs-get-from-shadow-index default-directory)))
             (node (progn
                     (ht-set! treemacs-shadow-index "/A/B"
                              (make-treemacs-shadow-node :key "/A/B" :closed t))
                     (treemacs-get-from-shadow-index "/A/B"))))
        (setf (treemacs-shadow-node->parent node) root)
        (setf (treemacs-shadow-node->children root) (list node))
        (treemacs-on-expand "/A/B" 22 "/A")
        (-let [node (treemacs-get-from-shadow-index "/A/B")]
          (should (= 2 (ht-size treemacs-shadow-index)))
          (should (equal (list node) (treemacs-shadow-node->children root)))
          (should (= 22 (treemacs-shadow-node->position node)))
          (should (equal root (treemacs-shadow-node->parent node)))
          (should-not (treemacs-shadow-node->closed node)))))))

;; `treemacs-on-collapse'
(progn
  (ert-deftest on-collapse::does-nothing-on-nil-argument ()
    (with-temp-buffer
      (setq treemacs-shadow-index (ht))
      (should-not (treemacs-on-collapse nil))
      (should (= 0 (ht-size treemacs-shadow-index)))))

  (ert-deftest on-collapse::fully-remove-node-without-children ()
    (with-temp-buffer
      (let* ((default-directory "/A")
             (treemacs-shadow-index (ht))
             (root (progn
                     (ht-set! treemacs-shadow-index default-directory
                              (make-treemacs-shadow-node :key default-directory))
                     (treemacs-get-from-shadow-index default-directory)))
             (node (progn
                     (ht-set! treemacs-shadow-index "/A/B"
                              (make-treemacs-shadow-node :key "/A/B"))
                     (treemacs-get-from-shadow-index "/A/B"))))
        (setf (treemacs-shadow-node->parent node) root)
        (setf (treemacs-shadow-node->children root) (list node))
        (treemacs-on-collapse "/A/B")
        (should (= 1 (ht-size treemacs-shadow-index)))
        (should-not (ht-get treemacs-shadow-index "/A/B"))
        (should-not (treemacs-shadow-node->children root)))))

  (ert-deftest on-collapse::mark-node-with-children-as-closed ()
    (with-temp-buffer
      (let* ((default-directory "/A")
             (treemacs-shadow-index (ht))
             (root (progn
                     (ht-set! treemacs-shadow-index default-directory
                              (make-treemacs-shadow-node :key default-directory))
                     (treemacs-get-from-shadow-index default-directory)))
             (node1 (progn
                      (ht-set! treemacs-shadow-index "/A/B"
                               (make-treemacs-shadow-node :key "/A/B"))
                      (treemacs-get-from-shadow-index "/A/B")))
             (node2 (progn
                      (ht-set! treemacs-shadow-index "/A/B/C"
                               (make-treemacs-shadow-node :key "/A/B/C"))
                      (treemacs-get-from-shadow-index "/A/B/C"))))
        (setf (treemacs-shadow-node->parent node1) root
              (treemacs-shadow-node->parent node2) node1
              (treemacs-shadow-node->children root) (list node1)
              (treemacs-shadow-node->children node1) (list node2))
        (treemacs-on-collapse "/A/B")
        (should (= 3 (ht-size treemacs-shadow-index)))
        (should (ht-get treemacs-shadow-index "/A/B"))
        (should (ht-get treemacs-shadow-index "/A/B/C"))
        (should (treemacs-shadow-node->closed node1))
        (should-not (treemacs-shadow-node->closed node2))
        (should (equal (list node1) (treemacs-shadow-node->children root)))
        (should (equal (list node2) (treemacs-shadow-node->children node1)))
        (should (equal root (treemacs-shadow-node->parent node1)))
        (should (equal node1 (treemacs-shadow-node->parent node2))))))

  (ert-deftest on-collapse::reset-subtrees-refresh-and-pos-on-collapse ()
    (with-temp-buffer
      (let* ((default-directory "/A")
             (treemacs-shadow-index (ht))
             (root (progn
                     (ht-set! treemacs-shadow-index default-directory
                              (make-treemacs-shadow-node :key default-directory))
                     (treemacs-get-from-shadow-index default-directory)))
             (node1 (progn
                      (ht-set! treemacs-shadow-index "/A/B"
                               (make-treemacs-shadow-node :key "/A/B" :position 11 :refresh-flag t))
                      (treemacs-get-from-shadow-index "/A/B")))
             (node2 (progn
                      (ht-set! treemacs-shadow-index "/A/B/C"
                               (make-treemacs-shadow-node :key "/A/B/C" :position 12 :refresh-flag t))
                      (treemacs-get-from-shadow-index "/A/B/C"))))
        (setf (treemacs-shadow-node->parent node1) root
              (treemacs-shadow-node->parent node2) node1
              (treemacs-shadow-node->children root) (list node1)
              (treemacs-shadow-node->children node1) (list node2))
        (treemacs-on-collapse "/A/B")
        (should (= 3 (ht-size treemacs-shadow-index)))
        (should (ht-get treemacs-shadow-index "/A/B"))
        (should (ht-get treemacs-shadow-index "/A/B/C"))
        (should-not (treemacs-shadow-node->refresh-flag node1))
        (should-not (treemacs-shadow-node->refresh-flag node2))
        (should-not (treemacs-shadow-node->position node1))
        (should-not (treemacs-shadow-node->position node2)))))

  (ert-deftest on-collapse::remove-subtree-on-purge ()
    (with-temp-buffer
      (let* ((default-directory "/A")
             (treemacs-shadow-index (ht))
             (root (progn
                     (ht-set! treemacs-shadow-index default-directory
                              (make-treemacs-shadow-node :key default-directory))
                     (treemacs-get-from-shadow-index default-directory)))
             (node1 (progn
                      (ht-set! treemacs-shadow-index "/A/B"
                               (make-treemacs-shadow-node :key "/A/B" :position 11))
                      (treemacs-get-from-shadow-index "/A/B")))
             (node2 (progn
                      (ht-set! treemacs-shadow-index "/A/B/C"
                               (make-treemacs-shadow-node :key "/A/B/C" :position 12))
                      (treemacs-get-from-shadow-index "/A/B/C"))))
        (setf (treemacs-shadow-node->parent node1) root
              (treemacs-shadow-node->parent node2) node1
              (treemacs-shadow-node->children root) (list node1)
              (treemacs-shadow-node->children node1) (list node2))
        (treemacs-on-collapse "/A/B" t)
        (should (= 1 (ht-size treemacs-shadow-index)))
        (should-not (ht-get treemacs-shadow-index "/A/B"))
        (should-not (ht-get treemacs-shadow-index "/A/B/C"))
        (should-not (treemacs-shadow-node->children root))))))

;; `treemacs--get-or-parse-git-result'
(progn
  (ert-deftest get-or-parse-git::retuns-empty-table-for-nil ()
    (-let [result (treemacs--get-or-parse-git-result nil)]
      (should result)
      (should (ht-empty? result))))

  (ert-deftest get-or-parse-git::returns-already-parsed-table ()
    (let ((input (pfuture-new "echo"))
          (result (ht)))
      (process-put input 'git-table result)
      (should (eq result (treemacs--get-or-parse-git-result input)))))

  (ert-deftest get-or-parse-git::parses-git-output ()
    (with-mock
      (mock (treemacs--git-status-parse-function *) => (ht ("A" 1) ("B" 2)))
      (let* ((input (pfuture-new "echo"))
             (result (treemacs--get-or-parse-git-result input)))
        (should (ht? result))
        (should (= 2 (ht-size result)))
        (should (= 1 (ht-get result "A")))
        (should (= 2 (ht-get result "B")))))))

;; `treemacs-on-rename'
(progn
  (ert-deftest on-rename::does-nothing-on-empty-index ()
    (with-temp-buffer
      (-let [treemacs-shadow-index (ht)]
        (treemacs--on-rename "OLD" "NEW")
        (should (ht-empty? treemacs-shadow-index)))))

  (ert-deftest on-rename::does-nothing-when-old-key-not-in-index ()
    (with-temp-buffer
      (-let [treemacs-shadow-index (ht ("A" (make-treemacs-shadow-node :key "A")))]
        (treemacs--on-rename "OLD" "NEW")
        (should (= 1 (ht-size treemacs-shadow-index)))
        (should (ht-get treemacs-shadow-index "A")))))

  (ert-deftest on-rename::correctly-renames-full-subtree ()
    (with-temp-buffer
      (let* ((default-directory "/A")
             (root (make-treemacs-shadow-node :key "/A"))
             (node1 (make-treemacs-shadow-node :key "/A/OLD"))
             (node2 (make-treemacs-shadow-node :key "/A/OLD/X"))
             (node3 (make-treemacs-shadow-node :key "/A/OLD/X/Y"))
             (node4 (make-treemacs-shadow-node :key (list "Classes" "/A/OLD/X/Y")))
             (node5 (make-treemacs-shadow-node :key (list "Class Foo" "/A/OLD/X/Y" "Classes")))
             (node6 (make-treemacs-shadow-node :key (list "void bar()" "/A/OLD/X/Y" "Classes" "Class Foo")))
             (nodex (make-treemacs-shadow-node :key "/A/B"))
             (nodey (make-treemacs-shadow-node :key "/A/B/C")))
        (setf (treemacs-shadow-node->parent nodex) root
              (treemacs-shadow-node->parent nodey) root
              (treemacs-shadow-node->parent node1) root
              (treemacs-shadow-node->parent node2) node1
              (treemacs-shadow-node->parent node3) node2
              (treemacs-shadow-node->parent node4) node3
              (treemacs-shadow-node->parent node5) node4
              (treemacs-shadow-node->parent node6) node5
              (treemacs-shadow-node->children root) (list node1 nodex nodey)
              (treemacs-shadow-node->children node1) (list node2)
              (treemacs-shadow-node->children node2) (list node3)
              (treemacs-shadow-node->children node3) (list node4)
              (treemacs-shadow-node->children node4) (list node5)
              (treemacs-shadow-node->children node5) (list node6))
        (setq treemacs-shadow-index
              (ht ((treemacs-shadow-node->key root) root)
                  ((treemacs-shadow-node->key nodex) nodex)
                  ((treemacs-shadow-node->key nodey) nodey)
                  ((treemacs-shadow-node->key node1) node1)
                  ((treemacs-shadow-node->key node2) node2)
                  ((treemacs-shadow-node->key node3) node3)
                  ((treemacs-shadow-node->key node4) node4)
                  ((treemacs-shadow-node->key node5) node5)
                  ((treemacs-shadow-node->key node6) node6)))
        (treemacs--on-rename "/A/OLD" "/A/NEW")
        (print treemacs-shadow-index)
        (dolist (key '("/A/OLD" "/A/OLD/X" "/A/OLD/X/Y" ("Classes" "/A/OLD/X/Y")
                       ("Class Foo" "/A/OLD/X/Y" "Classes") ("void bar()" "/A/OLD/X/Y" "Classes" "Class Foo")))
          (should-not (ht-get treemacs-shadow-index key)))
        (dolist (key '("/A/NEW"   "/A/NEW/X" "/A/NEW/X/Y" ("Classes" "/A/NEW/X/Y")
                       ("Class Foo" "/A/NEW/X/Y" "Classes") ("void bar()" "/A/NEW/X/Y" "Classes" "Class Foo")))
          (should (ht-get treemacs-shadow-index key)))
        (should (= 9 (ht-size treemacs-shadow-index)))))))

;; Thorough Sys Test
(ert-deftest treemacs::sys-test ()
  (save-window-excursion
    (save-match-data
      (let* ((imenu-auto-rescan t)
             (org-imenu-depth 10)
             (treemacs-collapse-dirs 3)
             (project (make-treemacs-project :name "Test Project" :path (concat treemacs-dir "/test")))
             (treemacs-current-workspace (make-treemacs-workspace :name "Test Workspace" :projects (list project))))
        (unless noninteractive
          (unwind-protect
              (progn
                (delete-other-windows)
                (--when-let (treemacs-get-local-buffer) (kill-buffer it))

                (switch-to-buffer "*scratch*")

                ;; init with a workspace with a single project and make sure the root node looks right
                (call-interactively 'treemacs)
                (should (treemacs-get-local-buffer))
                (should (string= "Test Project" (treemacs--get-label-of (treemacs-current-button))))

                ;; add another project, expand both and jump between them
                (-let [unread-command-events (listify-key-sequence (kbd "RET"))]
                  (treemacs-add-project (concat treemacs-dir "/src")))
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
                (treemacs-goto-button (concat treemacs-dir "/test/testdir1/testdir2/testfile.el"))
                (should (string= "testfile.el" (treemacs--get-label-of (treemacs-current-button))))

                (call-interactively #'treemacs-TAB-action)
                (should (equal '("Variables" "Functions")
                               (-map #'treemacs--get-label-of
                                     (treemacs--get-children-of (treemacs-current-button)))))

                (call-interactively #'treemacs-next-line)
                (call-interactively #'treemacs-next-line)
                (should (string= "Functions" (treemacs--get-label-of (treemacs-current-button))))
                (call-interactively #'treemacs-TAB-action)
                (should (equal '("fn1" "fn2")
                               (-map #'treemacs--get-label-of
                                     (treemacs--get-children-of (treemacs-current-button)))))

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
            (--when-let (treemacs-get-local-buffer) (kill-buffer it))))))))

(provide 'treemacs-test)

;;; treemacs-test.el ends here
