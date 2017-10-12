;;; treemacs.el --- A tree style file viewer package -*- lexical-binding: t -*-

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
;;; Customize interface definitions.

;;; Code:

;; defined here since, with the exception of the main file that cannot be required
;; elsewhere, this is the first unit to be loaded
(defconst treemacs-no-images
  (condition-case e
      (progn (create-image "" 'xpm) nil)
    (error e))
  "This variable is a non-nil error value when Emacs is unable to create images.
In this scenario (usually caused by running Emacs without a graphical
environment) treemacs will not create any of its icons and will be forced to
perpetually use its simple string icon fallack.")

(defgroup treemacs nil
  "A major mode for displaying the file system in a tree layout."
  :group 'files
  :prefix "treemacs-"
  :link '(url-link :tag "Repository" "https://github.com/Alexander-Miller/treemacs"))

(defgroup treemacs-faces nil
  "Faces for treemacs' syntax highlighting."
  :group 'treemacs
  :group 'faces)

(defgroup treemacs-configuration nil
  "Treemacs configuration options."
  :group 'treemacs
  :prefix "treemacs-")

(defcustom treemacs-indentation 2
  "The number of spaces each level is indented in the tree."
  :type 'integer
  :group 'treemacs-configuration)

(defcustom treemacs-width 35
  "Width of the treemacs window."
  :type 'integer
  :group 'treemacs-configuration)

(defcustom treemacs-show-hidden-files t
  "Dotfiles will be shown if this is set to t and be hidden otherwise."
  :type 'boolean
  :group 'treemacs-configuration)

(defcustom treemacs-default-actions
  '((dir-node-open    . treemacs-push-button)
    (dir-node-closed  . treemacs-push-button)
    (file-node-open   . treemacs-visit-node-no-split)
    (file-node-closed . treemacs-visit-node-no-split)
    (tag-node-open    . treemacs-push-button)
    (tag-node-closed  . treemacs-push-button)
    (tag-node         . treemacs-visit-node-no-split))
  "Defines the behaviour of `treemacs-visit-node-default-action'.

Each alist element maps from a button state to the function that should be used
for that state. The list of all possible button states is defined in
`treemacs-valid-button-states'. Possible values are all treemacs-visit-node-*
functions as well as `treemacs-push-button' for simple open/close actions.

To keep the alist clean changes should not be made directly, but with
`treemacs-define-default-action', for example like this:
\(treemacs-define-default-action 'file-node-closed #'treemacs-visit-node-ace\)"
  :type 'alist
  :group 'treemacs-configuration)

(defcustom treemacs-follow-after-init nil
  "When t always run `treemacs-follow' after building a treemacs-buffer.

A treemacs buffer is built when after calling `treemacs-init' or
`treemacs-projectle-init'. This will ignore `treemacs-follow-mode'."
  :type 'boolean
  :group 'treemacs-configuration)

(defcustom treemacs-header-function 'treemacs--create-header
  "The function which is used to create the header string for treemacs buffers.
Treemacs offers two predefined header creators:
1) `treemacs--create-header' (the default), which will simply output the current
   treemacs root.
2) `treemacs--create-header-projectile', defined in `treemacs-projectile' which
   will first try to find the name of the current projectile project and fall
   back on `treemacs--create-header' if no project name is found.
Other than these two functions this value may be made to use any custom function
which takes as input a string (the absolute path of the current treemacs root)
and outputs the string header to be inserted in the treemacs buffer."
  :type 'function
  :group 'treemacs-configuration)

(defcustom treemacs-icons-hash (make-hash-table :test 'equal)
  "Hash table containing a mapping of icons onto file extensions."
  :type 'plist
  :group 'treemacs-configuration)

(defcustom treemacs-git-integration nil
  "When t use different faces for files' different git states."
  :type 'boolean
  :group 'treemacs-configuration)

(defcustom treemacs-dotfiles-regex (rx bol "." (1+ any))
  "Files matching this regular expression count as dotfiles."
  :type 'regexp
  :group 'treemacs-configuration)

(defcustom treemacs-change-root-without-asking nil
  "When t don't ask to change the root when calling `treemacs-find-file'."
  :type 'boolean
  :group 'treemacs-configuration)

(defcustom treemacs-never-persist nil
  "When t treemacs will never persist its state.
By default treemacs' state is written to disk in `treemacs--persist-file' if it
detects a session saving mechanism like desktop save mode so it can be restored
on the next launch."
  :type 'boolean
  :group 'treemacs-configuration)

(defcustom treemacs-sorting 'alphabetic-desc
  "Indicates how treemacs will sort its files and directories.
Files will still always be shown after directories.

Valid values are
 * alphabetic-asc,
 * alphabetic-desc,
 * size-asc,
 * size-desc,
 * mod-time-asc,
 * mod-time-desc.

Note about performance:
Treemacs does its best to optimize its performance critical path, it does so
by doing as little work as possible and producing as little garbage as possible.
Deciding on the order in which its nodes are inserted is a part of this path. As
such certain tradeoffs need to be accounted far.

In plaintext: some sort settings are much slower than others. Alphabetic sorting
\(the default) is fastest and causes no additional overhead (even when
foregoing sorting altogether).

Modification time sorting takes the middle, being ca. 4x slower than alphabetic.
Sorting by size is slowest, being ca. 5-6x slower than alphabetic. It also
produces the most garbage, making it more like for you to run into a garbage
collection pause.

Lest these numbers scare you off keep in mind that they will likely have little
to no effect on your usage of treemacs until you begin frequently refreshing
treemacs views containing hundreds or even thousands of nodes."
  :type '(choice (const alphabetic-asc)
                 (const alphabetic-desc)
                 (const size-asc)
                 (const size-desc)
                 (const mod-time-asc)
                 (const mod-time-desc))
  :group 'treemacs-configuration)

(defcustom treemacs-ignored-file-predicates
  '(treemacs--std-ignore-file-predicate)
  "List of predicates to test for files ignored by Emacs.

Ignored files will *never* be shown in the treemacs buffer (unlike dotfiles)
whose presence is controlled by `treemacs-show-hidden-files').

Each predicate is a function that takes the filename as its only argument and
returns t if the file should be ignored and nil otherwise. A file whose name
returns t for *any* function in this list counts as ignored.

By default this list contains `treemacs--std-ignore-file-predicate' which
filters out '.', '..', Emacs' lock files as well as flycheck's temp files, and
therefore should not be directly overwritten, but added to and removed from
instead."
  :type 'list
  :group 'treemacs-configuration)

(defcustom treemacs-file-event-delay 5000
  "How long (in milliseconds) to collect file events before refreshing.
When treemacs receives a file change notification it doesn't immediately refresh
and instead waits `treemacs-file-event-delay' milliseconds to collect further
file change events. This is done so as to avoid refreshing multiple times in a
short time.
See also `treemacs-filewatch-mode'."
  :type 'integer
  :group 'treemacs-configuration)

(defcustom treemacs-goto-tag-strategy 'refetch-index
  "Inidicates how to move to a tag when its buffer is dead.
The tags in the treemacs view store their position as markers (or overlays if
semantic mode is on) pointing to a buffer. If that buffer is killed, or has
never really been open, as treemacs kills buffer after fetching their tags if
they did no exist before, the stored positions become stale, and treemacs needs
to use a different method to move to that tag. This variale sets that method.

Its possible values are:

 * refetch-index
   Call up the file's imenu index again and use its information to jump.
 * call-xref
   Call `xref-find-definitions' to find the tag. Only available since emacs25.
 * issue-warning
   Just issue a warning that the tag's position pointer is invalid."
  :type 'integer
  :group 'treemacs-configuration)

(defcustom treemacs-collapse-dirs 0
  "When > 0 treemacs will collapse directories into one when possible.
A directory is collapsible when its content consists of nothing but another
directory.

The value determines how many directories can be collapsed at once, both as a
performance cap and to prevent a too long directory names in the treemacs view.

To minimize this option's impact on display performace the search for
directories to collapse is done asynchronously in a python script and will thus
only work when python installed. The script should work both on python2 and 3."
  :type 'integer
  :group 'treemacs-configuration)

(defcustom treemacs-silent-refresh nil
  "When non-nil a completed refresh will not be announced with a message.
This applies to refreshing both manual as well as automatic (due to e.g.
`treemacs-filewatch-mode')."
  :type 'boolean
  :group 'treemacs-configuration)

(defcustom treemacs-is-never-other-window nil
  "When non-nil treemacs will never be used as `other-window'.
This can prevent other packages from opening other buffers in the treemacs
window. It also means treemacs is never selected by calls to `other-window'."
  :type 'boolean
  :group 'treemacs-configuration)

(defcustom treemacs-position 'left
  "Position of treemacs buffer.

Valid values are
 * left,
 * right."
  :type '(choice (const left)
                 (const right))
  :group 'treemacs-configuration)

(defcustom treemacs-tag-follow-delay 1.5
  "Delay in seconds of inactivity for `treemacs-tag-follow-mode' to trigger."
  :type 'number
  :group 'treemacs-configuration)

(provide 'treemacs-customization)

;;; treemacs-customization.el ends here
