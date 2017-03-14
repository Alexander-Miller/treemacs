(require 'treemacs)
(require 'ert)
(require 'el-mock)

;; treemacs--maybe-filter-dotfiles ;;
(progn
  (ert-deftest filter-dotfiles//do-nothing-when-dotfiles-are-shown ()
    (with-mock (stub treemacs--current-root => "~/")
               (let ((treemacs-show-hidden-files t)
                     (input '("~/.A" "~/B/C" "~/.A/B" "~/.A/.B/.C")))
                 (should (equal input (treemacs--maybe-filter-dotfiles input))))))

  (ert-deftest filter-dotfiles//do-nothing-for-nulls ()
    (with-mock (stub treemacs--current-root => "~/")
               (let ((treemacs-show-hidden-files nil))
                 (should (null (treemacs--maybe-filter-dotfiles nil))))))

  (ert-deftest filter-dotfiles//do-nothing-for-empty-input ()
    (with-mock (stub treemacs--current-root => "~/")
               (let ((treemacs-show-hidden-files nil))
                 (should (null (treemacs--maybe-filter-dotfiles '()))))))

  (ert-deftest filter-dotfiles//filter-single-dotfile ()
    (with-mock (stub treemacs--current-root => "~/")
               (let ((treemacs-show-hidden-files nil)
                     (input '("~/A/B/C/D/.d")))
                 (should (null (treemacs--maybe-filter-dotfiles input))))))

  (ert-deftest filter-dotfiles//filter-dotfile-based-on-parent ()
    (with-mock (stub treemacs--current-root => "~/")
               (let ((treemacs-show-hidden-files nil)
                     (input '("~/A/B/C/.D/d")))
                 (should (null (treemacs--maybe-filter-dotfiles input))))))

  (ert-deftest filter-dotfiles//dont-filter-dotfile-above-root ()
    (with-mock (stub treemacs--current-root => "~/.A/B")
               (let ((treemacs-show-hidden-files nil)
                     (input '("~/.A/B/C/d")))
                 (should (equal input (treemacs--maybe-filter-dotfiles input))))))

  (ert-deftest filter-dotfiles//filter-long-input ()
    (with-mock (stub treemacs--current-root => "~/.A/B")
               (let ((treemacs-show-hidden-files nil)
                     (input '("~/.A/B/C/d" "~/.A/B/.C/D/E" "~/.A/B/C/.d" "~/.A/B/C/D/E")))
                 (should (equal '("~/.A/B/C/d" "~/.A/B/C/D/E") (treemacs--maybe-filter-dotfiles input)))))))
