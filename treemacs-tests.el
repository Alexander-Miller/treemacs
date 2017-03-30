;;; treemacs.el --- A tree style file viewer package

;; Copyright (C) 2017 Alexander Miller

;; Author: Alexander Miller <alexanderm@web.de>
;; Package-Requires: ((cl-lib "0.5") (dash "2.11.0") (s "1.10.0") (f "0.11.0") (ace-window "0.9.0"))
;; Homepage: https://github.com/Alexander-Miller/treemacs
;; Version: 1.0
;; Keywords: tree, file, explorer

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
;;; Tests extracted into their own file to reduce clutter.

;;; Code:

(require 'treemacs-impl)
(require 'treemacs-customization)
(require 'ert)
(require 'el-mock)

;; treemacs--maybe-filter-dotfiles
(progn
  (ert-deftest filter-dotfiles//do-nothing-when-dotfiles-are-shown ()
    (with-mock (stub treemacs--current-root => "/home/")
               (let ((treemacs-show-hidden-files t)
                     (input '("/home/.A" "/home/B/C" "/home/.A/B" "/home/.A/.B/.C")))
                 (should (equal input (treemacs--maybe-filter-dotfiles input))))))

  (ert-deftest filter-dotfiles//do-nothing-for-nulls ()
    (with-mock (stub treemacs--current-root => "/home/")
               (let ((treemacs-show-hidden-files nil))
                 (should (null (treemacs--maybe-filter-dotfiles nil))))))

  (ert-deftest filter-dotfiles//do-nothing-for-empty-input ()
    (with-mock (stub treemacs--current-root => "/home/")
               (let ((treemacs-show-hidden-files nil))
                 (should (null (treemacs--maybe-filter-dotfiles '()))))))

  (ert-deftest filter-dotfiles//filter-single-dotfile ()
    (with-mock (stub treemacs--current-root => "/home/")
               (let ((treemacs-show-hidden-files nil)
                     (input '("/home/A/B/C/D/.d")))
                 (should (null (treemacs--maybe-filter-dotfiles input))))))

  (ert-deftest filter-dotfiles//filter-dotfile-based-on-parent ()
    (with-mock (stub treemacs--current-root => "/home/")
               (let ((treemacs-show-hidden-files nil)
                     (input '("/home/A/B/C/.D/d")))
                 (should (null (treemacs--maybe-filter-dotfiles input))))))

  (ert-deftest filter-dotfiles//dont-filter-dotfile-above-root ()
    (with-mock (stub treemacs--current-root => "/home/.A/B")
               (let ((treemacs-show-hidden-files nil)
                     (input '("/home/.A/B/C/d")))
                 (should (equal input (treemacs--maybe-filter-dotfiles input))))))

  (ert-deftest filter-dotfiles//filter-long-input ()
    (with-mock (stub treemacs--current-root => "/home/.A/B")
               (let ((treemacs-show-hidden-files nil)
                     (input '("/home/.A/B/C/d" "/home/.A/B/.C/D/E" "/home/.A/B/C/.d" "/home/.A/B/C/D/E")))
                 (should (equal '("/home/.A/B/C/d" "/home/.A/B/C/D/E") (treemacs--maybe-filter-dotfiles input)))))))

;; treemacs--add-to-cache
(progn
  (ert-deftest add-to-dirs-cache//add-single-item ()
    (let ((parent "/home/A")
          (child  "/home/A/B")
          (treemacs--open-dirs-cache nil))
      (treemacs--add-to-cache parent child)
      (should (equal `((,parent ,child)) treemacs--open-dirs-cache))))

  (ert-deftest add-to-dirs-cache//add-two-same-parent-items ()
    (let ((parent "/home/A")
          (child1 "/home/A/B1")
          (child2 "/home/A/B2")
          (treemacs--open-dirs-cache nil))
      (treemacs--add-to-cache parent child1)
      (treemacs--add-to-cache parent child2)
      (should (equal `((,parent ,child2 ,child1)) treemacs--open-dirs-cache))))

  (ert-deftest add-to-dirs-cache//add-two-different-parent-items ()
    (let ((parent1 "/home/A1")
          (parent2 "/home/A2")
          (child1  "/home/A/B1")
          (child2  "/home/A/B2")
          (treemacs--open-dirs-cache nil))
      (treemacs--add-to-cache parent1 child1)
      (treemacs--add-to-cache parent2 child2)
      (should (equal `((,parent2 ,child2) (,parent1 ,child1)) treemacs--open-dirs-cache))))

  (ert-deftest add-to-dirs-cache//add-new-child-to-cache-with-multiple-items ()
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
  (ert-deftest clear-from-dirs-cache//clear-item-from-empty-cache ()
    (let ((treemacs--open-dirs-cache nil))
      (treemacs--clear-from-cache "/home/A")
      (should (null treemacs--open-dirs-cache))))

  (ert-deftest clear-from-dirs-cache//clear-item-not-in-the-cache ()
    (let ((treemacs--open-dirs-cache '(("/home/A" "/home/A/B"))))
      (treemacs--clear-from-cache "/home/X")
      (should (equal '(("/home/A" "/home/A/B")) treemacs--open-dirs-cache))))

  (ert-deftest clear-from-dirs-cache//clear-item-from-empty-cache-with-purge ()
    (let ((treemacs--open-dirs-cache nil))
      (treemacs--clear-from-cache "/home/A" t)
      (should (null treemacs--open-dirs-cache))))

  (ert-deftest clear-from-dirs-cache//clear-item-from-cache-deleting-the-entry ()
    (let ((treemacs--open-dirs-cache '(("/home/A" "/home/A/B"))))
      (treemacs--clear-from-cache "/home/A/B")
      (should (null treemacs--open-dirs-cache))))

  (ert-deftest clear-from-dirs-cache//clear-item-from-cache-deleting-the-entry-with-purge ()
    (let ((treemacs--open-dirs-cache '(("/home/A" "/home/A/B"))))
      (treemacs--clear-from-cache "/home/A/B" t)
      (should (null treemacs--open-dirs-cache))))

  (ert-deftest clear-from-dirs-cache//clear-item-from-cache-leaving-other-entry ()
    (let* ((entry1 '("/home/A" "/home/A/B"))
           (entry2 '("/home/C" "/home/C/D"))
           (treemacs--open-dirs-cache (list entry1 entry2)))
      (treemacs--clear-from-cache "/home/A/B")
      (should (equal treemacs--open-dirs-cache `(,entry2)))))

  (ert-deftest clear-from-dirs-cache//clear-item-from-cache-leaving-other-entry-with-purge ()
    (let* ((entry1 '("/home/A" "/home/A/B"))
           (entry2 '("/home/C" "/home/C/D"))
           (treemacs--open-dirs-cache (list entry1 entry2)))
      (treemacs--clear-from-cache "/home/A/B" t)
      (should (equal treemacs--open-dirs-cache `(,entry2)))))

  (ert-deftest clear-from-dirs-cache//clear-one-of-two-items ()
    (let* ((treemacs--open-dirs-cache '(("/home/A" "/home/A/B" "/home/A/C"))))
      (treemacs--clear-from-cache "/home/A/B" t)
      (should (equal treemacs--open-dirs-cache '(("/home/A" "/home/A/C"))))))

  (ert-deftest clear-from-dirs-cache//clear-one-of-two-items-with-purge ()
    (let* ((treemacs--open-dirs-cache '(("/home/A" "/home/A/B" "/home/A/C"))))
      (treemacs--clear-from-cache "/home/A/B" t)
      (should (equal treemacs--open-dirs-cache '(("/home/A" "/home/A/C"))))))

  (ert-deftest clear-from-dirs-cache//clear-item-and-purge-children ()
    (let* ((treemacs--open-dirs-cache
            '(("/home/A" "/home/A/B")
              ("/home/A/B" "/home/A/B/C1" "/home/A/B/C2")
              ("/home/A/B/C1" "/home/A/B/C1/D1" "/home/A/B/C1/D2" "/home/A/B/C1/D3")
              ("/home/A/B/C2" "/home/A/B/C2/D1")
              ("/home/A/B/C1/D3" "/home/A/B/C1/D3/E1")
              ("/home/A2" "/home/A2/B1" "/home/A2/B2"))))
      (treemacs--clear-from-cache "/home/A/B" t)
      (should (equal treemacs--open-dirs-cache '(("/home/A2" "/home/A2/B1" "/home/A2/B2")))))))

(provide 'treemacs-tests)

;;; treemacs-tests.el ends here
