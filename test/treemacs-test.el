;;; treemacs-test.el --- Tests for treemacs -*- lexical-binding: t -*-

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

(require 'filenotify)
(require 'dash)
(require 'pfuture)
(require 'treemacs)
(require 'treemacs-bookmarks)
(require 'treemacs-core-utils)
(require 'org)
(require 'buttercup)

(defconst treemacs-should-run-file-notify-tests (not (null file-notify--library)))

(defmacro treemacs--with-project (pr &rest body)
  "Set PR as the only project in current workspace and then run BODY."
  (declare (indent 1))
  `(let ((--original-- (treemacs-current-workspace))
         (ws (make-treemacs-workspace :name "FAKE" :projects ,(when pr `(list ,pr)))))
     (unwind-protect
         (progn
           (setf (treemacs-current-workspace) ws)
           ,@body)
       (progn
         (setf (treemacs-current-workspace) --original--)))))

(defmacro treemacs--save-workspace (&rest body)
  "Execute BODY saving the current workspace."
  `(-let [ws (treemacs-current-workspace)]
     (unwind-protect
         ,@body
       (setf (treemacs-current-workspace) ws))))

(describe "treemacs-is-path"

  (describe ":in matcher"

    (it "identifies direct parent"
      (let ((path "~/A/B/c")
            (parent "~/A/B"))
        (expect (treemacs-is-path path :in parent) :to-be-truthy)))

    (it "identifies indirect parent"
      (let ((path "~/A/B/C/D/e")
            (parent "~/A/B"))
        (expect (treemacs-is-path path :in parent) :to-be-truthy)))

    (it "identifies non-parent"
      (let ((path "~/A/B/C/D/e")
            (parent "~/B"))
        (expect (treemacs-is-path path :in parent) :not :to-be-truthy)))

    (it "identifies non-parent with similar prefix"
      (let ((path "~/A/prefix1")
            (parent "~/A/prefix2"))
        (expect (treemacs-is-path path :in parent) :not :to-be-truthy))))

  (describe ":in-project matcher"

    (it "Identifies that a path is in a project"
      (let ((path "~/P/A/B/C/D/E/F/file")
            (project (make-treemacs-project :name "P" :path "~/P/A/B/C" :path-status 'local-readable)))
        (expect (treemacs-is-path path :in-project project) :to-be-truthy)))

       (it "Identifies that a path is not in a project"
         (let ((path "~/X/abc")
               (project (make-treemacs-project :name "P" :path "~/P" :path-status 'local-readable)))
           (expect (treemacs-is-path path :in-project project) :not :to-be-truthy))))

  (describe ":in-workspace matcher"

    (it "Finds project of path in the workspace"
      (let* ((path "~/C/abc")
             (p1 (make-treemacs-project :name "P1" :path "~/A" :path-status 'local-readable))
             (p2 (make-treemacs-project :name "P2" :path "~/B" :path-status 'local-readable))
             (p3 (make-treemacs-project :name "P3" :path "~/C" :path-status 'local-readable))
             (ws (make-treemacs-workspace :name "WS" :projects (list p1 p2 p3))))
        (expect (treemacs-is-path path :in-workspace ws) :to-be p3)))

    (it "Identifies path not in the workspace" ()
        (let* ((path "~/D/abc")
               (p1 (make-treemacs-project :name "P1" :path "~/A" :path-status 'local-readable))
               (p2 (make-treemacs-project :name "P2" :path "~/B" :path-status 'local-readable))
               (p3 (make-treemacs-project :name "P3" :path "~/C" :path-status 'local-readable))
               (ws (make-treemacs-workspace :name "WS" :projects (list p1 p2 p3))))
          (expect (treemacs-is-path path :in-workspace ws) :to-be nil)))))

(describe "treemacs--reject-ignored-files"

  (let ((treemacs-ignored-file-predicates (default-value 'treemacs-ignored-file-predicates)))

    (describe "Accepting"

      (it "Accepts dot-file"
        (expect (treemacs--reject-ignored-files "~/A/B/C/.foo.el") :to-be t))

      (it "Accepts common absolute path"
        (expect (treemacs--reject-ignored-files "~/A/B/C/foo.el") :to-be t))

      (it "Accepts common filename"
        (expect (treemacs--reject-ignored-files "foo.el") :to-be t))

      (it "Accepts directory"
        (expect (treemacs--reject-ignored-files "~/A/B/C/") :to-be t)))

    (describe "Rejecting"

      (it "Fails on nil input"
        (expect (treemacs--reject-ignored-files nil) :to-throw))

      (it "Fails on empty name"
        (expect (treemacs--reject-ignored-files "") :to-throw))

      (it "Rejects Emacs lock file"
        (expect (treemacs--reject-ignored-files "~/A/B/C/.#foo.el") :to-be nil))

      (it "Rejects Emacs backup file"
        (expect (treemacs--reject-ignored-files "~/A/B/C/foo.el~") :to-be nil))

      (it "Rejects autosave file"
        (expect (treemacs--reject-ignored-files "~/A/B/C/#foo.el#") :to-be nil))

      (it "Rejects flycheck's temp file"
        (expect (treemacs--reject-ignored-files "~/A/B/C/flycheck_foo.el") :to-be nil))

      (it "Rejects .git"
        (expect (treemacs--reject-ignored-files "~/A/B/C/.git") :to-be nil))

      (it "Rejects dot"
        (expect (treemacs--reject-ignored-files ".") :to-be nil))

      (it "Rejects dot-dot"
        (expect (treemacs--reject-ignored-files "..") :to-be nil)))))

(describe "treemacs--reject-ignored-and-dotfiles"

  (let ((treemacs-ignored-file-predicates (default-value 'treemacs-ignored-file-predicates)))

    (describe "Acceptions"

      (it "Accepts common absolute path"
        (expect (treemacs--reject-ignored-and-dotfiles "~/A/B/C/foo.el") :to-be t))

      (it "Accepts common filename"
        (expect (treemacs--reject-ignored-and-dotfiles "foo.el") :to-be t))

      (it "Accepts directory"
        (expect (treemacs--reject-ignored-and-dotfiles "~/A/B/C/") :to-be t)))

    (describe "Rejections"

      (it "Fails on nil input"
        (expect (treemacs--reject-ignored-and-dotfiles nil) :to-throw))

      (it "Fails on empty name"
        (expect (treemacs--reject-ignored-and-dotfiles "") :to-throw))

      (it "Rejects Emacs lock file"
        (expect (treemacs--reject-ignored-and-dotfiles "~/A/B/C/.#foo.el") :to-be nil))

      (it "Rejects Emacs backup file"
        (expect (treemacs--reject-ignored-and-dotfiles "~/A/B/C/foo.el~") :to-be nil))

      (it "Rejects autosave file"
        (expect (treemacs--reject-ignored-and-dotfiles "~/A/B/C/#foo.el#") :to-be nil))

      (it "Rejects flycheck's temp file"
        (expect (treemacs--reject-ignored-and-dotfiles "~/A/B/C/flycheck_foo.el") :to-be nil))

      (it "Rejects dot-file"
        (expect (treemacs--reject-ignored-and-dotfiles "~/A/B/C/.foo.el") :to-be nil))

      (it "Rejects .git"
        (expect (treemacs--reject-ignored-files "~/A/B/C/.git") :to-be nil))

      (it "Rejects dot"
        (expect (treemacs--reject-ignored-and-dotfiles ".") :to-be nil))

      (it "Rejects dot-dot"
          (expect (treemacs--reject-ignored-and-dotfiles "..") :to-be nil)))))

(describe "treemacs--is-event-relevant?"

  (-let [treemacs-ignored-file-predicates (default-value 'treemacs-ignored-file-predicates)]

    (describe "accept"

      (it "accepts change event when git-mode is enabled"
        (let ((treemacs-git-mode t)
              (event '(nil changed "~/A/a")))
          (expect (treemacs--is-event-relevant? event) :to-be-truthy)))

      (it "accepts create events"
        (-let [event '(nil created "~/A/a")]
          (expect (treemacs--is-event-relevant? event) :to-be-truthy)))

      (it "accepts delete events"
        (-let [event '(nil deleted "~/A/a")]
          (expect (treemacs--is-event-relevant? event) :to-be-truthy))))

    (describe "reject"

      (it "rejects stop-watch event"
        (-let [event '(nil stopped "~/A/a")]
          (expect (treemacs--is-event-relevant? event) :not :to-be-truthy)))

      (it "rejects change event when git-mode is disabled"
        (let ((treemacs-git-mode nil)
              (event '(nil changed "~/A/a")))
          (expect (treemacs--is-event-relevant? event) :not :to-be-truthy)))

      (it "rejects lockfile events"
        (-let [event '(nil created "~/A/.#foo.el")]
          (expect (treemacs--is-event-relevant? event) :not :to-be-truthy)))

      (it "rejects flycheck file events"
        (-let [event '(nil created "~/A/flycheck_foo.el")]
          (expect (treemacs--is-event-relevant? event) :not :to-be-truthy))))))

(describe "treemacs--file-extension"

  (it "Fails on nil input"
    (expect (treemacs--file-extension nil) :to-throw))

  (it "Returns empty string when input is empty string"
    (expect (treemacs--file-extension "") :to-equal ""))

  (it "Returns empty string when input is only period"
    (expect (treemacs--file-extension ".") :to-equal ""))

  (it "Returns empty string when input is many periods"
    (expect (treemacs--file-extension ".......") :to-equal ""))

  (it "Returns input for an absolute path without extension"
    (expect (treemacs--file-extension "/A/B/C/D/foo") :to-equal "/A/B/C/D/foo"))

  (it "Returns the filename of a filename without extension"
    (expect (treemacs--file-extension "foo") :to-equal "foo"))

  (it "Returns the extension of an absolute path"
    (expect (treemacs--file-extension "~/A/B/C/D/foo.el") :to-equal "el"))

  (it "Returns the extension of a filename"
    (expect (treemacs--file-extension "foo.el") :to-equal "el"))

  (it "Returns the extension of absolute path with periods"
    (expect (treemacs--file-extension "~/A/foo.bar/baz.qux/foo.el") :to-equal "el")))

(describe "treemacs--partition-imenu-index"

  (it "Returns nil on nil input"
    (expect (treemacs--partition-imenu-index nil "A") :not :to-be-truthy))

  (it "Returns index unchanged when input has no top level leaves"
    (expect '(("A" ("a1" "a2")) ("B" ("b1" "b2")))
            :to-equal
            (treemacs--partition-imenu-index '(("A" ("a1" "a2")) ("B" ("b1" "b2"))) "Functions")))

  (it "Partitions single top-level list into Functions"
    (expect '(("Functions" ("x" "y" "z")))
            :to-equal
            (treemacs--partition-imenu-index '(("x" "y" "z")) "Functions")))

  (it "Partitions top-level lists into Functions"
    (expect '(("A" ("a1" "a2")) ("B" ("b1" "b2")) ("Functions" ("x" "y" "z")))
            :to-equal
            (treemacs--partition-imenu-index '(("A" ("a1" "a2")) ("B" ("b1" "b2")) ("x" "y" "z")) "Functions"))))

(describe "treemacs--should-reenter?"

  (describe "Accepting"

    (it "Accepts nil"
      (expect (treemacs--should-reenter? nil)
              :to-be t))

    (it "Accepts custom nodes"
      (expect (treemacs--should-reenter? '(:custom A B))
              :to-be t))

    (it "Accepts non-string/cons paths"
      (expect (treemacs--should-reenter? 'X)
              :to-be t))

    (it "Accepts non-dotfiles"
      (-let [treemacs-show-hidden-files nil]
        (expect (treemacs--should-reenter? "/foo/bar")
                :to-be t)))

    (it "Accepts extensions under non-dotfiles"
      (-let [treemacs-show-hidden-files nil]
        (expect (treemacs--should-reenter? '("/foo/bar" A B))
                :to-be t)))

    (it "Accepts dotfiles when they are shown"
      (-let [treemacs-show-hidden-files t]
        (expect (treemacs--should-reenter? "/foo/.bar")
                :to-be t)))

    (it "Accepts extensions under dotfiles when they are shown"
      (-let [treemacs-show-hidden-files t]
        (expect (treemacs--should-reenter? '("/foo/.bar" A B))
                :to-be t))))

  (describe "Rejecting"

    (it "Rejects dotfiles when they are hidden"
      (-let [treemacs-show-hidden-files nil]
        (expect (treemacs--should-reenter? "/foo/.bar")
                :to-be nil)))

    (it "Rejects extensions under dotfiles when they are hidden"
      (-let [treemacs-show-hidden-files nil]
        (expect (treemacs--should-reenter? '("/foo/.bar" A B))
                :to-be nil)))))

(describe "treemacs--parent"

  (it "Fails on nil input"
    (expect (treemacs--parent nil) :to-throw))

  (it "Returns nil when input is empty"
    (expect (treemacs--parent "") :to-be nil))

  (it "Returns nil when input is not a valid path"
    (expect (treemacs--parent "ABC") :to-be nil))

  (it "Correctly identifies a parent path"
    (expect (treemacs--parent "/home/A/B") :to-equal "/home/A"))

  (it "Returns the system root when it's the input"
    (expect (treemacs--parent "/") :to-equal "/"))

  (it "Returns parent of root-level extension node."
    (expect (treemacs--parent '(:custom "a" "b")) :to-equal '(:custom "a")))

  (it "Returns directory extension of extension sub-item node."
    (expect (treemacs--parent '("/test1" "a" "b")) :to-equal '("/test1" "a")))

  (it "Returns directory of a directory extension node."
    (expect (treemacs--parent '("/test1" "a")) :to-equal "/test1"))

  (it "Returns project extension of a project sub-item node."
    (let ((p (make-treemacs-project :path "/A" :path-status 'local-readable)))
      (expect (treemacs--parent (list p "a" "b")) :to-equal (list p "a"))))

  (it "Returns project of a project extension node."
    (let ((p (make-treemacs-project :path "/A" :path-status 'local-readable)))
      (expect (treemacs--parent (list p "a")) :to-equal "/A"))))

(describe "treemacs--get-or-parse-git-result"

  (it "Returns an empty table when input is nil"
    (-let [result (treemacs--get-or-parse-git-result nil)]
      (expect result :to-be-truthy)
      (expect (ht-empty? result) :to-be t)))

  (it "Returns an already parsed table"
    (let ((input (pfuture-new "echo"))
          (result (ht)))
      (process-put input 'git-table result)
      (expect (treemacs--get-or-parse-git-result input) :to-be result)))

  (it "Parses a process' git output"

    (spy-on #'treemacs--git-status-parse-function
            :and-return-value (ht ("A" 1) ("B" 2)))

     (let* ((input (pfuture-new "echo"))
            (result (treemacs--get-or-parse-git-result input)))
       (expect (ht? result))
       (expect (= 2 (ht-size result)))
       (expect (= 1 (ht-get result "A")))
       (expect (= 2 (ht-get result "B")))
       (expect #'treemacs--git-status-parse-function :to-have-been-called))))

(describe "treemacs--on-rename"

  (it "Does nothing when the dom is empty"
    (with-temp-buffer
      (-let [treemacs-dom (ht)]
        (treemacs--on-rename "OLD" "NEW" nil)
        (expect (ht-empty? treemacs-dom) :to-be t))))

  (it "Does nothing when the old key is not in the dom"
    (with-temp-buffer
      (-let [treemacs-dom (ht ("A" (make-treemacs-dom-node :key "A")))]
        (treemacs--on-rename "OLD" "NEW" nil)
        (expect (ht-size treemacs-dom) :to-equal 1)
        (expect (ht-get treemacs-dom "A") :to-be-truthy))))

  (it "Correctly renamed a full subtree"
    (with-temp-buffer
      (let* ((default-directory "/A")
             (root (make-treemacs-dom-node :key "/A"))
             (node1 (make-treemacs-dom-node :key "/A/OLD"))
             (node2 (make-treemacs-dom-node :key "/A/OLD/X"))
             (node3 (make-treemacs-dom-node :key "/A/OLD/X/Y"))
             (node4 (make-treemacs-dom-node :key (list "/A/OLD/X/Y" "Classes")))
             (node5 (make-treemacs-dom-node :key (list "/A/OLD/X/Y" "Classes" "Class Foo")))
             (node6 (make-treemacs-dom-node :key (list "/A/OLD/X/Y" "Classes" "Class Foo" "void bar()")))
             (nodex (make-treemacs-dom-node :key "/A/B"))
             (nodey (make-treemacs-dom-node :key "/A/B/C")))
        (setf (treemacs-dom-node->parent nodex) root
              (treemacs-dom-node->parent nodey) root
              (treemacs-dom-node->parent node1) root
              (treemacs-dom-node->parent node2) node1
              (treemacs-dom-node->parent node3) node2
              (treemacs-dom-node->parent node4) node3
              (treemacs-dom-node->parent node5) node4
              (treemacs-dom-node->parent node6) node5
              (treemacs-dom-node->children root) (list node1 nodex nodey)
              (treemacs-dom-node->children node1) (list node2)
              (treemacs-dom-node->children node2) (list node3)
              (treemacs-dom-node->children node3) (list node4)
              (treemacs-dom-node->children node4) (list node5)
              (treemacs-dom-node->children node5) (list node6))
        (setq treemacs-dom
              (ht ((treemacs-dom-node->key root) root)
                  ((treemacs-dom-node->key nodex) nodex)
                  ((treemacs-dom-node->key nodey) nodey)
                  ((treemacs-dom-node->key node1) node1)
                  ((treemacs-dom-node->key node2) node2)
                  ((treemacs-dom-node->key node3) node3)
                  ((treemacs-dom-node->key node4) node4)
                  ((treemacs-dom-node->key node5) node5)
                  ((treemacs-dom-node->key node6) node6)))
        (treemacs--on-rename "/A/OLD" "/A/NEW" nil)
        (dolist (key '("/A/OLD"
                       "/A/OLD/X"
                       "/A/OLD/X/Y"
                       ("/A/OLD/X/Y" "Classes")
                       ("/A/OLD/X/Y" "Classes" "Class Foo")
                       ("/A/OLD/X/Y" "Classes" "Class Foo" "void bar()")))
          (expect (ht-get treemacs-dom key) :to-be nil))
        (dolist (key '("/A/NEW" "/A/NEW/X" "/A/NEW/X/Y"
                       ("/A/NEW/X/Y" "Classes")
                       ("/A/NEW/X/Y" "Classes" "Class Foo")
                       ("/A/NEW/X/Y" "Classes" "Class Foo" "void bar()")))
          (expect (ht-get treemacs-dom key) :to-be-truthy))
        (expect (ht-size treemacs-dom) :to-equal 9))))

 (it "Won't rename initial node when filewatch is enabled"
    (with-temp-buffer
      (let* ((default-directory "/A")
             (root (make-treemacs-dom-node :key "/A"))
             (node1 (make-treemacs-dom-node :key "/A/OLD"))
             (node2 (make-treemacs-dom-node :key "/A/OLD/X"))
             (node3 (make-treemacs-dom-node :key "/A/OLD/X/Y"))
             (nodex (make-treemacs-dom-node :key "/A/B"))
             (nodey (make-treemacs-dom-node :key "/A/B/C")))
        (setf (treemacs-dom-node->parent nodex) root
              (treemacs-dom-node->parent nodey) root
              (treemacs-dom-node->parent node1) root
              (treemacs-dom-node->parent node2) node1
              (treemacs-dom-node->parent node3) node2
              (treemacs-dom-node->children root) (list node1 nodex nodey)
              (treemacs-dom-node->children node1) (list node2)
              (treemacs-dom-node->children node2) (list node3))
        (setf treemacs-dom
              (ht ((treemacs-dom-node->key root) root)
                  ((treemacs-dom-node->key nodex) nodex)
                  ((treemacs-dom-node->key nodey) nodey)
                  ((treemacs-dom-node->key node1) node1)
                  ((treemacs-dom-node->key node2) node2)
                  ((treemacs-dom-node->key node3) node3)))
        (treemacs--on-rename "/A/OLD" "/A/NEW" t)
        (dolist (key '("/A/OLD/X" "/A/OLD/X/Y"))
          (expect (ht-get treemacs-dom key) :to-be nil))
        (dolist (key '("/A/OLD" "/A/NEW/X" "/A/NEW/X/Y"))
          (expect (ht-get treemacs-dom key) :to-be-truthy))
        (expect (ht-size treemacs-dom) :to-equal 6)))))

(describe "treemacs-on-collapse"

  (it "Fails when key is nil"
    (with-temp-buffer
      (-let [treemacs-dom (ht)]
        (expect (treemacs-on-collapse nil) :to-throw))))

  (it "Removes empty nodes from reentry"
    (with-temp-buffer
      (let* ((default-directory "/A")
             (treemacs-dom (ht))
             (root (progn
                     (ht-set! treemacs-dom default-directory
                              (make-treemacs-dom-node :key default-directory))
                     (treemacs-find-in-dom default-directory)))
             (node (progn
                     (ht-set! treemacs-dom "/A/B"
                              (make-treemacs-dom-node :key "/A/B"))
                     (treemacs-find-in-dom "/A/B"))))
        (setf (treemacs-dom-node->parent node) root
              (treemacs-dom-node->children root) (list node)
              (treemacs-dom-node->reentry-nodes root) (list node))
        (treemacs-on-collapse "/A/B")
        (expect (treemacs-dom-node->reentry-nodes root) :to-be nil))))

  (it "Keeps node with children in reentry"
    (with-temp-buffer
      (let* ((default-directory "/A")
             (treemacs-dom (ht))
             (root (progn
                     (ht-set! treemacs-dom default-directory
                              (make-treemacs-dom-node :key default-directory))
                     (treemacs-find-in-dom default-directory)))
             (node1 (progn
                      (ht-set! treemacs-dom "/A/B"
                               (make-treemacs-dom-node :key "/A/B"))
                      (treemacs-find-in-dom "/A/B")))
             (node2 (progn
                      (ht-set! treemacs-dom "/A/B/C"
                               (make-treemacs-dom-node :key "/A/B/C"))
                      (treemacs-find-in-dom "/A/B/C"))))
        (setf (treemacs-dom-node->parent node1) root
              (treemacs-dom-node->parent node2) node1
              (treemacs-dom-node->children root) (list node1)
              (treemacs-dom-node->children node1) (list node2)
              (treemacs-dom-node->reentry-nodes root) (list node1)
              (treemacs-dom-node->reentry-nodes node1) (list node2))
        (treemacs-on-collapse "/A/B")
        (expect (ht-size treemacs-dom)
                :to-equal 2)
        (expect (treemacs-dom-node->children root)
                :to-equal (list node1))
        (expect (treemacs-dom-node->children node1)
                :to-be nil)
        (expect (treemacs-find-in-dom "/A/B/C")
                :to-be nil)
        (expect (treemacs-dom-node->reentry-nodes root)
                :to-be nil)
        (expect (treemacs-dom-node->reentry-nodes node1)
                :to-equal (list node2)))))

  (it "Removes a subtree when purging"
    (with-temp-buffer
      (let* ((default-directory "/A")
             (treemacs-dom (ht))
             (root (progn
                     (ht-set! treemacs-dom default-directory
                              (make-treemacs-dom-node :key default-directory))
                     (treemacs-find-in-dom default-directory)))
             (node1 (progn
                      (ht-set! treemacs-dom "/A/B"
                               (make-treemacs-dom-node :key "/A/B"))
                      (treemacs-find-in-dom "/A/B")))
             (node2 (progn
                      (ht-set! treemacs-dom "/A/B/C" (make-treemacs-dom-node :key "/A/B/C"))
                      (treemacs-find-in-dom "/A/B/C"))))
        (setf (treemacs-dom-node->parent node1) root
              (treemacs-dom-node->parent node2) node1
              (treemacs-dom-node->children root) (list node1)
              (treemacs-dom-node->children node1) (list node2)
              (treemacs-dom-node->reentry-nodes root) (list node1)
              (treemacs-dom-node->reentry-nodes node1) (list node2))
        (treemacs-on-collapse "/A/B" :purge)
        (expect (ht-size treemacs-dom)
                :to-equal 2)
        (expect (ht-get treemacs-dom "/A/B/C")
                :to-be nil)
        (expect (treemacs-dom-node->children node1)
                :to-be nil)
        (expect (treemacs-dom-node->reentry-nodes node1)
                :to-be nil)))))

(describe "treemacs-on-expand"

  (it "Fails when key is nil"
    (with-temp-buffer
      (-let [treemacs-dom (ht)]
        (expect (treemacs-on-expand nil 1) :to-be nil))))

  (it "Correctly expands root node"
    (with-temp-buffer
      (let* ((default-directory "/A")
             (treemacs-dom (ht)))
        (treemacs-on-expand "/A" 1)
        (-let [root (treemacs-find-in-dom default-directory)]
          (expect (ht-size treemacs-dom) :to-equal 1)
          (expect (treemacs-dom-node->position root) :to-equal 1)
          (expect (treemacs-dom-node->parent root) :to-be nil)))))

  (it "Correctly expands child node"
    (with-temp-buffer
      (let* ((default-directory "/A")
             (treemacs-dom (ht))
             (root (progn
                     (ht-set! treemacs-dom default-directory
                              (make-treemacs-dom-node :key default-directory))
                     (treemacs-find-in-dom default-directory)))
             (node (progn
                     (ht-set! treemacs-dom "/A/B"
                              (make-treemacs-dom-node :key "/A/B"))
                     (treemacs-find-in-dom "/A/B"))))
        (setf (treemacs-dom-node->parent node) root
              (treemacs-dom-node->children root) (list node))
        (treemacs-on-expand "/A/B" 2)
        (expect (treemacs-dom-node->position node) :to-equal 2)
        (expect (treemacs-dom-node->reentry-nodes root) :to-equal (list node))))))

(when treemacs-should-run-file-notify-tests

  (describe "treemacs--start-watching"

    (before-each
      (spy-on #'file-notify-add-watch :and-return-value 123456))

    (it "Stars watching an unwatched file"
      (let ((path "/A")
            (treemacs-filewatch-mode t)
            (treemacs--filewatch-index (make-hash-table :test #'equal))
            (treemacs--collapsed-filewatch-index (make-hash-table :test #'equal)))
        (treemacs--start-watching path t)
        (expect (gethash path treemacs--filewatch-index)
                :to-equal
                (cons (list (current-buffer)) 123456))
        (expect (gethash path treemacs--collapsed-filewatch-index) :to-be-truthy)
        (expect #'file-notify-add-watch :to-have-been-called)))

    (it "Keeps watching an already watched file"
      (let ((path "/A")
            (treemacs-filewatch-mode t)
            (treemacs--filewatch-index (make-hash-table :test #'equal))
            (treemacs--collapsed-filewatch-index (make-hash-table :test #'equal)))
        (puthash path (cons '(x y) 123456) treemacs--filewatch-index)
        (treemacs--start-watching path t)
        (expect (gethash path treemacs--filewatch-index)
                :to-equal
                (cons (list (current-buffer) 'x 'y) 123456))
        (expect (gethash path treemacs--collapsed-filewatch-index)
                :to-be-truthy)
        (expect #'file-notify-add-watch :not :to-have-been-called)))

    (it "Adds a watching buffer only once"
      (let ((path "/A")
            (treemacs-filewatch-mode t)
            (treemacs--filewatch-index (make-hash-table :test #'equal))
            (treemacs--collapsed-filewatch-index (make-hash-table :test #'equal)))
        (puthash path (cons '(x y) 123456) treemacs--filewatch-index)
        (treemacs--start-watching path t)
        (treemacs--start-watching path t)
        (expect (gethash path treemacs--filewatch-index)
                :to-equal
                (cons (list (current-buffer) 'x 'y) 123456))
        (expect (gethash path treemacs--collapsed-filewatch-index) :to-be-truthy)
        (expect #'file-notify-add-watch :not :to-have-been-called)))))

(when treemacs-should-run-file-notify-tests

  (describe "treemacs--stop-watching"

    (it "Does nothing when path is not watched"
        (let ((treemacs--filewatch-index (make-hash-table :test #'equal))
              (treemacs--collapsed-filewatch-index (make-hash-table :test #'equal)))
          (expect (treemacs--stop-watching "/A") :to-be nil)))

    (it "Stops the watch of the only watching buffer"
      (spy-on #'file-notify-rm-watch :and-return-value t)
      (let ((path "/A")
            (treemacs--filewatch-index (make-hash-table :test #'equal))
            (treemacs--collapsed-filewatch-index (make-hash-table :test #'equal)))
        (puthash path (cons (list (current-buffer)) 123456) treemacs--filewatch-index)
        (puthash path t treemacs--collapsed-filewatch-index)
        (treemacs--stop-watching path)
        (expect (gethash path treemacs--filewatch-index) :to-be nil)
        (expect (gethash path treemacs--collapsed-filewatch-index) :to-be nil)))

    (it "Stops the watch of one of several buffers"
      (spy-on #'file-notify-rm-watch)
      (let ((path "/A")
            (treemacs--filewatch-index (make-hash-table :test #'equal))
            (treemacs--collapsed-filewatch-index (make-hash-table :test #'equal)))
        (puthash path (cons (list 'x 'y (current-buffer)) 123456) treemacs--filewatch-index)
        (puthash path t treemacs--collapsed-filewatch-index)
        (treemacs--stop-watching path)
        (expect (gethash path treemacs--filewatch-index)
                :to-equal (cons '(x y) 123456))
        (expect (gethash path treemacs--collapsed-filewatch-index) :to-be-truthy)
        (expect #'file-notify-rm-watch :not :to-have-been-called)))

    (it "Stops the watch of path below stopped path"
        (spy-on #'file-notify-rm-watch :and-return-value t)
        (let ((path "/A/B")
              (treemacs--filewatch-index (make-hash-table :test #'equal))
              (treemacs--collapsed-filewatch-index (make-hash-table :test #'equal)))
          (puthash path (cons (list (current-buffer)) 123456) treemacs--filewatch-index)
          (puthash path t treemacs--collapsed-filewatch-index)
          (treemacs--stop-watching "/A")
          (expect (gethash path treemacs--filewatch-index) :to-be nil)
          (expect (gethash path treemacs--collapsed-filewatch-index) :to-be nil)))

    (it "Stops the watch of all watching buffers"
        (spy-on #'file-notify-rm-watch :and-return-value t)
        (let ((path "/A")
              (treemacs--filewatch-index (make-hash-table :test #'equal))
              (treemacs--collapsed-filewatch-index (make-hash-table :test #'equal)))
          (puthash path (cons '(x y z) 123456) treemacs--filewatch-index)
          (puthash path t treemacs--collapsed-filewatch-index)
          (treemacs--stop-watching path t)
          (expect (gethash path treemacs--filewatch-index) :to-be nil)
          (expect (gethash path treemacs--collapsed-filewatch-index) :to-be nil)))))

(describe "treemacs--flatten&sort-imenu-index"

  (it "Correctly transforms an org-mode index"
    (let ((org-imenu-depth 10)
          (temp-file (make-temp-file "Treemacs Test")))
      (ignore org-imenu-depth) ; for the compiler
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
              (expect (treemacs--flatten&sort-imenu-index)
                      :to-equal
                      `((("H1"     . ,(move-marker (make-marker) 1)))
                        (("H1.2"   . ,(move-marker (make-marker) 6)) "H1")
                        (("H1.2.3" . ,(move-marker (make-marker) 14)) "H1" "H1.2")
                        (("H2"     . ,(move-marker (make-marker) 25)))))))
        (kill-buffer (get-file-buffer temp-file))
        (delete-file temp-file)))))

(describe "treemacs--find-index-pos"

  (it "Fails when point is nil"
    (-let [pos nil]
      (expect (treemacs--find-index-pos pos '((("A" . (make-marker))))) :to-throw)))

  (it "Returns nil when index is nil"
    (expect (treemacs--find-index-pos 1 nil) :to-be nil))

  (it "Finds the correct position before the first marker"
    (let ((input `((("A" . ,(move-marker (make-marker) 10)))
                   (("B" . ,(move-marker (make-marker) 20)))
                   (("C" . ,(move-marker (make-marker) 30))))))
      (expect (treemacs--find-index-pos 1 input)
              :to-equal (car input))))

  (it "Finds the correct position after the last marker"
    (let ((input `((("A" . ,(move-marker (make-marker) 10)))
                   (("B" . ,(move-marker (make-marker) 20)))
                   (("C" . ,(move-marker (make-marker) 30))))))
      (expect (treemacs--find-index-pos 100 input)
              :to-equal (-last-item input))))

  (it "Finds an index using binary search"
    (spy-on #'treemacs--binary-index-search :and-call-through)
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
        (expect (treemacs--find-index-pos 72 input)
                :to-equal (nth 6 input))
        (expect #'treemacs--binary-index-search :to-have-been-called)))))

(describe "treemacs--find-project-for-path"

  (it "Returns nil when input is nil"
    (treemacs--with-project (make-treemacs-project :path "/A" :path-status 'local-readable)
      (expect (treemacs--find-project-for-path nil) :to-be nil)))

  (it "Returns nil when the workspace is empty"
    (treemacs--with-project nil
      (expect (treemacs--find-project-for-path "/A") :to-be nil)))

  (it "Returns nil when path does not fit any project"
    (treemacs--with-project (make-treemacs-project :path "/A/B" :path-status 'local-readable)
      (expect (treemacs--find-project-for-path "/A/C") :to-be nil)))

  (it "Returns project when path fits"
    (-let [project (make-treemacs-project :path "/A/B" :path-status 'local-readable)]
      (treemacs--with-project project
        (expect (treemacs--find-project-for-path "/A/B/C")
                :to-equal project)))))

(describe "treemacs--flatten-imenu-index"

  (it "Does nothing when input is nil"
    (expect (treemacs--flatten-imenu-index nil) :to-be nil))

  (it "Does nothing when input is empty"
    (expect (treemacs--flatten-imenu-index (list)) :to-be nil))

  (it "Correctly parses a single item"
    (expect (treemacs--flatten-imenu-index '("Functions")) :to-be nil))

  (it "Correctly parses full index"
    (-let [input `(("Functions" ("f1" . 1) ("f2" . 2))
                   ("Types"     ("t1" . 3) ("t2" . 4))
                   ("Classes"   ("c1" ("Members" ("m1" . 5) ("m2" . 6)))))]
      (expect (treemacs--flatten-imenu-index input)
              :to-equal
              `((("f2" . 2) "Functions")
                (("f1" . 1) "Functions")
                (("t2" . 4) "Types")
                (("t1" . 3) "Types")
                (("m2" . 6) "Classes" "c1" "Members")
                (("m1" . 5) "Classes" "c1" "Members"))))))

(describe "treemacs--flatten-org-mode-imenu-index"

  (it "Does nothing when input is nil"
    (expect (treemacs--flatten-org-mode-imenu-index nil) :to-be nil))

  (it "Does nothing when input is empty"
    (expect (treemacs--flatten-org-mode-imenu-index (list)) :to-be nil))

  (it "Correctly parses a single item"
    (expect (treemacs--flatten-org-mode-imenu-index '("Functions"))
            :to-equal '(("Functions"))))

  (it "Correctly parses full index"
    (-let [input `(("Functions" ("f1" . 1) ("f2" . 2))
                   ("Types"     ("t1" . 3) ("t2" . 4))
                   ("Classes"   ("c1" ("Members" ("m1" . 5) ("m2" . 6)))))]
           (expect (treemacs--flatten-org-mode-imenu-index input)
              :to-equal
              `(("Classes")
                ("Types")
                ("Functions")
                (("f2" . 2) "Functions")
                (("f1" . 1) "Functions")
                (("t2" . 4) "Types")
                (("t1" . 3) "Types")
                ("c1" "Classes")
                ("Members" "Classes" "c1")
                (("m2" . 6) "Classes" "c1" "Members")
                (("m1" . 5) "Classes" "c1" "Members"))))))

(describe "treemacs--next-non-child-button"

  (it "Does nothing when input is nil"
    (expect (treemacs--next-non-child-button nil) :to-be nil))

  (it "Returns nil when there is only a single button"
    (with-temp-buffer
      (-let [b (insert-text-button "b")]
        (expect (treemacs--next-non-child-button b) :to-be nil))))

  (it "Directly returns the next button"
    (with-temp-buffer
      (let ((b1 (insert-text-button "b1"))
            (b2 (insert-text-button "b2")))
        (button-put b1 :depth 1)
        (button-put b2 :depth 1)
        (expect (marker-position (treemacs--next-non-child-button b1))
                :to-equal b2))))

  (it "Searches through higher-depth buttons"
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
        (expect (marker-position (treemacs--next-non-child-button b1))
                :to-equal b6))))

  (it "Returns nil when there is no next non-child button"
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
        (expect (treemacs--next-non-child-button b1) :to-be nil)))))

(describe "treemacs--validate-persist-lines"

  (describe "Successes"

    (it "Succeeds on correctly formed input"
      (-let [lines '("* W1" "** P1" " - path :: a" "** P2" "- path :: b" "* W2" "** P3" " - path :: c")]
        (expect (treemacs--validate-persist-lines lines) :to-be 'success)))

    (it "Succeeds with the same path in multiple workspaces"
      (-let [lines '("* W1" "** P1" " - path :: /A/B" "* W2" "** P2" " - path :: /A/B")]
        (expect (treemacs--validate-persist-lines lines) :to-be 'success)))

    (it "Succeeds with non-connectable remotes"
      (let* ((treemacs--org-edit-buffer-name (buffer-name))
             (lines '("* W1" "** P1" " - path :: /ftp:anonymous@ftp.invalid:/test-path")))
        (expect (treemacs--validate-persist-lines lines) :to-be 'success))))

  (describe "Errors"

    (it "Fails when first line is not a workspace name"
      (-let [lines '("X")]
        (expect (treemacs--validate-persist-lines lines)
                :to-equal '(error "X" "First item must be a workspace name"))))

    (it "Fails when line after workspace name is not a project name"
      (-let [lines '("* X" "Y")]
        (expect (treemacs--validate-persist-lines lines)
                :to-equal '(error  "Y" "Workspace name must be followed by project name"))))

    (it "Fails when line after project name is not a property"
      (-let [lines '("* X" "** Y" "Z")]
        (expect (treemacs--validate-persist-lines lines)
                :to-equal '(error  "** Y" "Project name must be followed by path declaration"))))

    (it "Fails when line after path is not a project or workspace"
      (-let [lines '("* X" "** Y" " - path :: Z" "A")]
        (expect (treemacs--validate-persist-lines lines)
                :to-equal '(error " - path :: Z" "Path property must be followed by the next workspace or project"))))

    (it "Fails when line end at workspace name"
      (-let [lines '("* X")]
        (expect (treemacs--validate-persist-lines lines)
                :to-equal '(error "* X" "Cannot end with a project or workspace name"))))

    (it "Fails when line end at project name"
      (-let [lines '("* X" "** X")]
        (expect (treemacs--validate-persist-lines lines)
                :to-equal '(error "** X" "Cannot end with a project or workspace name"))))

    (it "Fails when input is empty"
      (expect (treemacs--validate-persist-lines nil)
              :to-equal '(error :start "Input is empty")))

    (it "Fails when path appears more than once"
      (-let [lines '("* W1"
                     "** P1"
                     " - path :: /A/B/C"
                     "** P2"
                     "- path :: /A/B/C/D")]
        (expect (treemacs--validate-persist-lines lines)
                :to-equal
                '(error "- path :: /A/B/C/D" "Path '/A/B/C/D' appears in the workspace more than once."))))))

(describe "treemacs--read-persist-lines"

  (it "Ignores commentes"
    (expect (treemacs--read-persist-lines "#\n#\n#") :to-be nil))

  (it "Ignores blanks"
    (expect (treemacs--read-persist-lines "   \n \n \t \t   ") :to-be nil))

  (it "Reads everything else"
    (expect (treemacs--read-persist-lines
             (concat "#Foo: Bar\n" "\n" "* Workspace\n" "\t\n" "** Project\n" "#Comment\n" " - path :: /x\n"))
            :to-equal
            '("* Workspace" "** Project" " - path :: /x"))))

(describe "treemacs--git-status-process"

  (it "Does not call treemacs--git-status-process-function with non-local or unreadable paths"
    (dolist (status '(local-unreadable remote-readable remote-unreadable remote-disconnected extension))
      (spy-on 'treemacs--git-status-process-function)
      (-> treemacs-dir
          (f-join "test")
          (treemacs--git-status-process (make-treemacs-project :name "P" :path treemacs-dir :path-status status)))
      (expect 'treemacs--git-status-process-function
              :not :to-have-been-called)))

  (it "Calls treemacs--git-status-process-function with local readable path"
    (spy-on 'treemacs--git-status-process-function)
    (let ((path (f-join treemacs-dir "test")))
      (treemacs--git-status-process path (make-treemacs-project :name "P" :path treemacs-dir :path-status 'local-readable))
      (expect 'treemacs--git-status-process-function
              :to-have-been-called-with path))))

(describe "treemacs--process-collapsed-dirs"

  (it "Does nothing with non-local or unreadable paths"
    (-let [treemacs-collapse-dirs 3]
      (dolist (status '(local-unreadable remote-readable remote-unreadable remote-disconnected extension))
        (expect (-> treemacs-dir
                    (f-join "test")
                    (treemacs--collapsed-dirs-process (make-treemacs-project :name "P" :path treemacs-dir :path-status status)))
                :to-equal
                nil)))))

(describe "treemacs--parse-collapsed-dirs"

  (it "Finds dirs to flatten in test directory"
    (-let [treemacs-collapse-dirs 3]
      (expect (-> treemacs-dir
                  (f-join "test")
                  (treemacs--collapsed-dirs-process
                   (make-treemacs-project
                    :name "P"
                    :path treemacs-dir
                    :path-status 'local-readable))
                  (treemacs--parse-collapsed-dirs))
              :to-equal
              `(("/testdir2/testdir3"
                 ,(f-join treemacs-dir "test/testdir1")
                 ,(f-join treemacs-dir "test/testdir1/testdir2")
                 ,(f-join treemacs-dir "test/testdir1/testdir2/testdir3"))))))

  (it "Returns nil when there is nothing to flatten"
    (-let [treemacs-collapse-dirs 3]
      (expect (-> treemacs-dir
                  (f-join "test/testdir1/testdir2")
                  (treemacs--collapsed-dirs-process (make-treemacs-project :name "P" :path treemacs-dir :path-status 'local-readable))
                  (treemacs--parse-collapsed-dirs))
              :to-be nil))))

(describe "treemacs--remove-trailing-newline"

  (it "Fails on nil input"
    (expect (treemacs--remove-trailing-newline nil)
            :to-throw))

  (it "Fails on empty string"
    (expect (treemacs--remove-trailing-newline "")
            :to-throw))

  (it "Does nothing when input does not need trimming"
    (-let [input "abc"]
      (expect (treemacs--remove-trailing-newline input) :to-equal input)))

  (it "Removes the last newline"
    (expect (treemacs--remove-trailing-newline "abc\n") :to-equal "abc"))

  (it "Removes only the last newline"
    (expect (treemacs--remove-trailing-newline "abc\n\n\n") :to-equal "abc\n\n")))

(describe "treemacs--add-trailing-slash"

  (it "Fails on nil input"
    (expect (treemacs--add-trailing-slash nil) :to-throw))

  (it "Fails on blank input"
    (expect (treemacs--add-trailing-slash "") :to-throw))

  (it "Does not add slash if one is already present"
    (expect (treemacs--add-trailing-slash "/ABC/") :to-equal "/ABC/"))

  (it "Adds a slash when there isn't one"
    (expect (treemacs--add-trailing-slash "/ABC") :to-equal "/ABC/")))

(describe "treemacs--is-name-invalid?"
  (it "detects nil"
    (expect (treemacs--is-name-invalid? nil) :to-be t))

  (it "detects an empty string"
    (expect (treemacs--is-name-invalid? "") :to-be t))

  (it "detects a blank string"
    (expect (treemacs--is-name-invalid? "      ") :to-be t))

  (it "detects a string with newlines"
    (expect (treemacs--is-name-invalid? "a\nb") :to-be t)))

(describe "treemacs--find-workspace"

  (it "Finds nothing when there are no workspaces"
    (treemacs--save-workspace
     (-let [treemacs--workspaces nil]
       (treemacs--find-workspace)
       (expect (treemacs-current-workspace) :to-be nil)))

    (treemacs--save-workspace
     (-let [treemacs--workspaces nil]
       (treemacs--find-workspace "X")
       (expect (treemacs-current-workspace) :to-be nil))))

  (it "Finds the first workspace when there is no current file"
    (treemacs--save-workspace
     (let* ((ws1 (make-treemacs-workspace :name "A"))
            (ws2 (make-treemacs-workspace :name "B"))
            (treemacs--workspaces (list ws1 ws2)))
       (treemacs--find-workspace)
       (expect (treemacs-current-workspace) :to-be ws1))))

  (it "Finds the first workspace when nothing fits the current file"
    (treemacs--save-workspace
     (let* ((p1  (make-treemacs-project :name "P1" :path "P1"))
            (p2  (make-treemacs-project :name "P2" :path "P2"))
            (ws1 (make-treemacs-workspace :name "A" :projects (list p1)))
            (ws2 (make-treemacs-workspace :name "B" :projects (list p2)))
            (treemacs--workspaces (list ws1 ws2)))
       (treemacs--find-workspace "X")
       (expect (treemacs-current-workspace) :to-be ws1))))

  (it "Finds workspace which contains current file"
    (treemacs--save-workspace
     (let* ((p1  (make-treemacs-project :name "P1" :path "P1"))
            (p2  (make-treemacs-project :name "P2" :path "/A"))
            (ws1 (make-treemacs-workspace :name "A" :projects (list p1)))
            (ws2 (make-treemacs-workspace :name "B" :projects (list p2)))
            (treemacs--workspaces (list ws1 ws2)))
       (treemacs--find-workspace "/A/B/C")
       (expect (treemacs-current-workspace) :to-be ws2)))))

(defmacro test-treemacs--with-sample-buffer (&rest body)
  "Evaluate BODY with some buttons defined.

In BODY, variable PROJECT is defined."
  (declare (indent 0))
  (let ((parent-marker (make-symbol "parent-marker")))
    `(with-temp-buffer
       (let ((project (make-treemacs-project :name "Project" :path "/project"))
             (,parent-marker nil))
         (insert-text-button "Project"
                             :path "/project"
                             :state 'root-node-open
                             :depth 0
                             :project project)
         (setq ,parent-marker (copy-marker (line-beginning-position)))
         (insert "\n")
         (insert-text-button "directory"
                             :path "/project/directory"
                             :key "/project/directory"
                             :state 'dir-node-open
                             :parent ,parent-marker
                             :depth 1)
         (setq ,parent-marker (copy-marker (line-beginning-position)))
         (insert "\n")
         (insert-text-button "file.txt"
                             :path "/project/directory/file.txt"
                             :key "/project/directory/file.txt"
                             :state 'file-node-closed
                             :parent ,parent-marker
                             :depth 2)
         (setq ,parent-marker (copy-marker (line-beginning-position)))
         (insert "\n")
         (goto-char 0)
         ,@body))))

(defun test-treemacs--format-pattern (template expected-1 expected-2 expected-3)
  "Test that `treemacs--format-bookmark-title' expands TEMPLATE correctly.

EXPECTED-1 is the expected expansion of the \"Project\" button.
EXPECTED-2 is the expected expansion of the \"directory\" button.
EXPECTED-3 is the expected expansion of the \"file.txt\" button."
  (test-treemacs--with-sample-buffer
   (let ((treemacs-bookmark-title-template template))
     (expect (treemacs--format-bookmark-title (treemacs-current-button)) :to-equal expected-1)
     (forward-line 1)
     (expect (treemacs--format-bookmark-title (treemacs-current-button)) :to-equal expected-2)
     (forward-line 1)
     (expect (treemacs--format-bookmark-title (treemacs-current-button)) :to-equal expected-3))))

(describe "treemacs-collect-child-nodes"

  (it "Finds nothing for last node"
    (with-temp-buffer
      (insert (propertize "Root" 'button t :depth 1))
      (insert "\n")
      (goto-char 0)
      (let* ((parent-btn (point-marker))
             (result (treemacs-collect-child-nodes parent-btn)))
        (expect result :to-be nil))))

  (it "Finds nothing for node without direct children"
    (with-temp-buffer
      (let* ((root1 (progn
                      (insert (propertize "Root1" 'button t :depth 1))
                      (beginning-of-line)
                      (point-marker)))
             (input (progn
                      (end-of-line)
                      (insert "\n" (propertize " Input" 'button t :depth 2 :parent root1))
                      (beginning-of-line)
                      (point-marker)))
             (_root2 (progn
                       (end-of-line)
                       (insert "\n" (propertize "Root2" 'button t :depth 1))
                       (beginning-of-line)
                       (point-marker))) )
        (-let [result (treemacs-collect-child-nodes input)]
          (expect result :to-be nil)))))

  (it "Finds only direct childre"
    (with-temp-buffer
      (let* ((root1 (progn
                      (insert (propertize "Root1" 'button t :depth 1))
                      (beginning-of-line)
                      (point-marker)))
             (input (progn
                      (end-of-line)
                      (insert "\n" (propertize " Input" 'button t :depth 2 :parent root1))
                      (beginning-of-line)
                      (point-marker)))
             (child1 (progn
                       (end-of-line)
                       (insert "\n" (propertize "  Child1" 'button t :depth 3 :parent input))
                       (beginning-of-line)
                       (point-marker)))
             (_grand-child (progn
                            (end-of-line)
                            (insert "\n" (propertize "   Grand Child" 'button t :depth 4 :parent child1))
                            (beginning-of-line)
                            (point-marker)))
             (_child2 (progn
                       (end-of-line)
                       (insert "\n" (propertize "  Child2" 'button t :depth 3 :parent input))
                       (beginning-of-line)
                       (point-marker))) )
        (-let [result (-map #'treemacs--get-label-of (treemacs-collect-child-nodes input))]
          (expect result :to-have-same-items-as '("  Child1" "  Child2")))))))

(describe "treemacs--format-bookmark-title"
  (it "Uses the configured pattern"
    (test-treemacs--format-pattern "No replacements" "No replacements" "No replacements" "No replacements"))

  (it "Formats the project name"
    (test-treemacs--format-pattern "${project}" "Project" "Project" "Project"))

  (it "Formats the label"
    (test-treemacs--format-pattern "${label}" "Project" "directory" "file.txt"))

  (it "Formats the parent label"
    (test-treemacs--format-pattern "${label:1}" "" "Project" "directory"))

  (it "Formats the grandparent label"
    (test-treemacs--format-pattern "${label:2}" "" "" "Project"))

  (it "Formats the label path"
    (test-treemacs--format-pattern "${label-path}" "Project" "Project/directory" "Project/directory/file.txt"))

  (it "Formats the limited label path"
    (test-treemacs--format-pattern "${label-path:2}" "Project" "Project/directory" "directory/file.txt"))

  (it "Does not hang with negatie label path limit"
    (test-treemacs--format-pattern "${label-path:-2}" "Project" "Project/directory" "Project/directory/file.txt"))

  (it "Formats the file path"
    (test-treemacs--format-pattern "${file-path}" "/project" "/project/directory" "/project/directory/file.txt"))

  (it "Formats the limited file path"
    (test-treemacs--format-pattern "${file-path:2}" "/project" "/project/directory" "directory/file.txt"))

  (it "Does not hang with negative file path"
    (test-treemacs--format-pattern "${file-path:-1}" "" "" "")))

(describe "treemacs-dom-node->remove-collapse-keys!"
  (it "Removes and deletes all collapse entries"
    (with-temp-buffer
      (let* ((dom-node (make-treemacs-dom-node
                        :key "Main Key"
                        :collapse-keys '("Key 1" "Key 2" "Key 3")))
             (treemacs-dom (ht ("Main Key" dom-node)
                               ("Key 1" dom-node)
                               ("Key 2" dom-node)
                               ("Key 3" dom-node))))
        (treemacs-dom-node->remove-collapse-keys! dom-node '("Key 1" "Key 3"))
        (expect (treemacs-find-in-dom "Main Key") :to-be dom-node)
        (expect (treemacs-find-in-dom "Key 2") :to-be dom-node)
        (expect (treemacs-find-in-dom "Key 1") :to-be nil)
        (expect (treemacs-find-in-dom "Key 3") :to-be nil)
        (expect (treemacs-dom-node->collapse-keys dom-node) :to-equal '("Key 2"))))))

(describe "treemacs--find-repeated-file-name"
  (before-each
    (fset 'fake-file-exists
          (lambda (p)
            (pcase p
              ("/a/file.el" t)
              ("/c/file" t)
              ((guard (and (s-starts-with? "/b/" p)
                           (not (s-contains? "5" p))))
               t)
              ((guard (and (s-starts-with? "/d/" p)
                           (not (s-contains? "5" p))))
               t) )))
    (spy-on 'file-exists-p :and-call-fake 'fake-file-exists))

  (it "Returns input when it does not already exist"
    (expect (treemacs--find-repeated-file-name "/X/Y/Z")
            :to-equal "/X/Y/Z"))

  (it "Find a '(Copy 1)' file with extension"
    (expect (treemacs--find-repeated-file-name "/a/file.el")
            :to-equal "/a/file (Copy 1).el"))

  (it "Find a '(Copy 5)' file with extension"
    (expect (treemacs--find-repeated-file-name "/b/file.el")
            :to-equal "/b/file (Copy 5).el"))

  (it "Find a '(Copy 1)' file without extension"
    (expect (treemacs--find-repeated-file-name "/c/file")
            :to-equal "/c/file (Copy 1)"))

  (it "Find a '(Copy 5)' file without extension"
    (expect (treemacs--find-repeated-file-name "/d/file")
            :to-equal "/d/file (Copy 5)")))

(describe "treemacs--prefix-arg-to-recurse-depth"
  (it "Nil translates to 0"
    (expect (treemacs--prefix-arg-to-recurse-depth nil) :to-be 0))

  (it "Numbers translate to themselves"
    (dotimes (n 10)
      (expect (treemacs--prefix-arg-to-recurse-depth n) :to-be n)))

  (it "Other values translate to 999"
    (expect (treemacs--prefix-arg-to-recurse-depth '(4)) :to-be 999)
    (expect (treemacs--prefix-arg-to-recurse-depth "str") :to-be 999)
    (expect (treemacs--prefix-arg-to-recurse-depth (list 1 2 3)) :to-be 999)
    (expect (treemacs--prefix-arg-to-recurse-depth (treemacs-current-workspace)) :to-be 999)))

(provide 'test-treemacs)

;;; treemacs-test.el ends here
