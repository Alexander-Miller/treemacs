;;; treemacs.el --- A tree style file viewer package -*- lexical-binding: t -*-

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
;;; Customize interface definitions.

;;; Code:

(require 'f)

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
  "The number of spaces each level is indented in the file tree.
Indentation is created by repeating `treemacs-indentation-string'."
  :type 'integer
  :group 'treemacs-configuration)

(defcustom treemacs-indentation-string " "
  "The string that is for indentation in the file tree.
Indentation is created by repeating this string `treemacs-indentation' many
times."
  :type 'string
  :group 'treemacs-configuration)

(defcustom treemacs-width 35
  "Width of the treemacs window."
  :type 'integer
  :group 'treemacs-configuration)

(defcustom treemacs-show-hidden-files t
  "Dotfiles will be shown if this is set to t and be hidden otherwise."
  :type 'boolean
  :group 'treemacs-configuration)

(defcustom treemacs-TAB-actions-config
  '((root-node-open   . treemacs-toggle-node)
    (root-node-closed . treemacs-toggle-node)
    (dir-node-open    . treemacs-toggle-node)
    (dir-node-closed  . treemacs-toggle-node)
    (file-node-open   . treemacs-toggle-node)
    (file-node-closed . treemacs-toggle-node)
    (tag-node-open    . treemacs-toggle-node)
    (tag-node-closed  . treemacs-toggle-node)
    (tag-node         . treemacs-visit-node-no-split))
  "Defines the behaviour of `treemacs-TAB-action'.

See the doc string of `treemacs-RET-actions-config' for a detailed description
of how this config works and how to modify it."
  :type 'alist
  :group 'treemacs-configuration)

(defcustom treemacs-doubleclick-actions-config
  '((root-node-open   . treemacs-toggle-node)
    (root-node-closed . treemacs-toggle-node)
    (dir-node-open    . treemacs-toggle-node)
    (dir-node-closed  . treemacs-toggle-node)
    (file-node-open   . treemacs-visit-node-no-split)
    (file-node-closed . treemacs-visit-node-no-split)
    (tag-node-open    . treemacs-toggle-node)
    (tag-node-closed  . treemacs-toggle-node)
    (tag-node         . treemacs-visit-node-no-split))
  "Defines the behaviour of `treemacs-doubleclick-action'.

See the doc string of `treemacs-RET-actions-config' for a detailed description
of how this config works and how to modify it."
  :type 'alist
  :group 'treemacs-configuration)

(defcustom treemacs-RET-actions-config
  '((root-node-open   . treemacs-toggle-node)
    (root-node-closed . treemacs-toggle-node)
    (dir-node-open    . treemacs-toggle-node)
    (dir-node-closed  . treemacs-toggle-node)
    (file-node-open   . treemacs-visit-node-no-split)
    (file-node-closed . treemacs-visit-node-no-split)
    (tag-node-open    . treemacs-toggle-node-prefer-tag-visit)
    (tag-node-closed  . treemacs-toggle-node-prefer-tag-visit)
    (tag-node         . treemacs-visit-node-no-split))
  "Defines the behaviour of `treemacs-RET-action'.

Each alist element maps from a button state to the function that should be used
for that state. The list of all possible button states is defined in
`treemacs-valid-button-states'. Possible values are all treemacs-visit-node-*
functions as well as `treemacs-toggle-node' for simple open/close actions,
though in general you can use any function that accepts the prefix arg as its
single argument.

To keep the alist clean changes should not be made directly, but with
`treemacs-define-RET-action', for example like this:
\(treemacs-define-RET-action 'file-node-closed #'treemacs-visit-node-ace\)"
  :type 'alist
  :group 'treemacs-configuration)

(defcustom treemacs-follow-after-init nil
  "When t always find and focus the current file when treemacs is built.

A treemacs buffer is built when after calling `treemacs-init' or
`treemacs-projectle-init'. This will ignore `treemacs-follow-mode'."
  :type 'boolean
  :group 'treemacs-configuration)

(defcustom treemacs-python-executable (executable-find "python")
  "The python executable used by treemacs.
An asynchronous python process is used in two optional feaures:
`treemacs-collapse-dirs' and the extended variant of `treemacs-git-mode'.

There is generally only one reason to change this value: an extended
`treemacs-git-mode' requires python3 to work. If the default python executable
is pointing to python2 this config variable can be used to direct treemacs to
the python3 binary."
  :type 'string
  :group 'treemacs-configuration)

(defcustom treemacs-dotfiles-regex (rx bol "." (1+ any))
  "Files matching this regular expression count as dotfiles."
  :type 'regexp
  :group 'treemacs-configuration)

(defcustom treemacs-sorting 'alphabetic-desc
  "Indicates how treemacs will sort its files and directories.
Files will still always be shown after directories.

Valid values are
 * alphabetic-asc,
 * alphabetic-desc,
 * alphabetic-case-insensitive-asc,
 * alphabetic-case-insensitive-desc,
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
                 (const alphabetic-case-insensitive-asc)
                 (const alphabetic-case-insensitive-desc)
                 (const size-asc)
                 (const size-desc)
                 (const mod-time-asc)
                 (const mod-time-desc))
  :group 'treemacs-configuration)

(defcustom treemacs-ignored-file-predicates
  '(treemacs--std-ignore-file-predicate)
  "List of predicates to test for files and directories ignored by Emacs.

Ignored files will *never* be shown in the treemacs buffer (unlike dotfiles)
whose presence is controlled by `treemacs-show-hidden-files').

Each predicate is a function that takes 2 arguments: a files's name and its
absolute path and returns t if the file should be ignored and nil otherwise. A
file which returns t for *any* function in this list counts as ignored.

By default this list contains `treemacs--std-ignore-file-predicate' which
filters out '.', '..', Emacs' lock files as well as flycheck's temp files, and
therefore should not be directly overwritten, but added to and removed from
instead."
  :type 'list
  :group 'treemacs-configuration)

(defcustom treemacs-pre-file-insert-predicates nil
  "List of predicates to test for files and directories that shouldn't be shown.
The difference between this and `treemacs-ignored-file-predicates' is that the
functions in this list will be called on files just before they would be
rendered, when the files' git status information is now available. This for
example allows to make files ignored by git invisible.
The functions in this list are therefore expected to have a different signature:
They must take two arguments - a file's absolute path and a hashtable that maps
files to their git status. The files' paths are the table's keys, its values are
characters (and not strings) indicating the file's git condition. The chars map
map as follows: (the pattern is derived from 'git status --porcelain')

 * M - file is modified
 * U - file is in conflict
 * ? - file is untracked
 * ! - file is ignored
 * A - file is added to index
 * other - file is unchanged

Otherwise the behaviour is the same as `treemacs-ignored-file-predicates', in
that any one function returning t for a file means that this file will not
be rendered.

Since removing files ignored by git is the most likely use-case treemacs offers
`treemacs-is-file-git-ignored?' to quickly make this possible."
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
`treemacs-filewatch-mode').
To only disable messages from refreshes induced by filewatch mode use
`treemacs-silent-filewatch'."
  :type 'boolean
  :group 'treemacs-configuration)

(defcustom treemacs-silent-filewatch nil
  "When non-nil a refresh due to filewatch mode will cause no log message.
To disable all refresh messages use `treemacs-silent-refresh'."
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

(defcustom treemacs-no-png-images nil
  "When non-nil treemacs will use TUI string icons even when running in a GUI.
The change will apply the next time a treemacs buffer is created."
  :type 'boolean
  :group 'treemacs-configuration)

(defcustom treemacs-tag-follow-cleanup t
  "When non-nil `treemacs-tag-follow-mode' will close file nodes it is leaving.
When jumping between different files this can prevent the view from being
flooded with their tags."
  :type 'boolean
  :group 'treemacs-configuration)

(defcustom treemacs-recenter-after-file-follow nil
  "When non-nil the view will be centered when follow mode is active.
This is done by calling `recenter' after `treemacs-follow-mode' moves to a new
file."
  :type 'boolean
  :group 'treemacs-configuration)

(defcustom treemacs-recenter-after-tag-follow nil
  "When non-nil the view will be centered when tag follow mode is active.
This is done by calling `recenter' after `treemacs-tag-follow-mode' moves to a
new file."
  :type 'boolean
  :group 'treemacs-configuration)

(defcustom treemacs-pre-refresh-hook nil
  "Hooks to run right before the refresh process for a project kicks off.
During the refresh the project is effectively collapsed and then expanded again.
This hook runs *before* that happens. It runs with treemacs as the
`current-buffer' and receives as its arguments all the information that treemacs
collects for its refresh process:
 * The project being refreshed (might be 'all)
 * The current line number.
 * The current button. Might be nil if point is on the header line.
 * The current button's state. See also `treemacs-valid-button-states'. Is nil
   if the current button is nil.
 * The nearest file path, as collected with `treemacs--nearest-path'. Is nil if
   point is on the header.
 * The current button's tag path, as collected by `treemacs--tags-path-of'. Is
   nil if the current button is nil.
 * The start position of the current window, as given by `window-start'."
  :type 'hook
  :group 'treemacs-configuration)

(defcustom treemacs-post-refresh-hook nil
  "Hooks to run right before the refresh process is finished off.
During the refresh the project is effectively collapsed and then expanded again.
This hook runs *after* that has happened. It runs with treemacs as the
`current-buffer' and receives as its arguments all the information that treemacs
collects for its refresh process. Note that these values were collected at the
start of the refresh, and may now be longer valid (for example the current
button's position will be wrong, even if it wasn't deleted outright):
 * The project being refreshed (might be 'all)
 * The current line number.
 * The current button. Might be nil if point was on the header line.
 * The current button's state. See also `treemacs-valid-button-states'. Is nil
   if the current button is nil.
 * The nearest file path, as collected with `treemacs--nearest-path'. Is nil if
   point was on the header.
 * The current button's tag path, as collected by `treemacs--tags-path-of'. Is
   nil if the current button is nil.
 * The start position of the current window, as given by `window-start'."
  :type 'hook
  :group 'treemacs-configuration)

(defcustom treemacs-pulse-on-success t
  "When non-nil treemacs will pulse the current line as a success indicator.
This applies to actions like `treemacs-copy-path-at-point'."
  :type 'boolean
  :group 'treemacs-configuration)

(defcustom treemacs-pulse-on-failure t
  "When non-nil treemacs will pulse the current line as a failure indicator.
This applies to actions like treemacs not finding any tags it can show when
`treemacs-toggle-node' is called on a file node."
  :type 'boolean
  :group 'treemacs-configuration)

(defcustom treemacs-follow-recenter-distance 0.1
  "Minimum distance from the top/bottom for (tag-)follow mode to recenter.
Treemacs will be calling `recenter' after following a file/tag if the distance
between point and the top/bottom of the treemacs window is less then this many
lines. The value is not an absolute line count, but a percentage, with 0.0
being 0% and 1.0 being 100%. This means that when this variable is set to 0.1
`recenter' will be called within a 10% distance of the window top/bottom. For a
window height of 40 lines that means point being within the first or last 4
lines of the treemacs window.
Will only take effect if `treemacs-recenter-after-tag-follow' and/or
`treemacs-recenter-after-file-follow' is non-nil.

Note that this does *not* take `scroll-margin' into account."
  :type 'float
  :group 'treemacs-configuration)

(defcustom treemacs-elisp-imenu-expression
  (let ((name (rx (1+ whitespace) (? "'") (group-n 2 symbol-start (1+ (or (syntax word) (syntax symbol))) symbol-end)))
        (prefix (rx bol (0+ (syntax whitespace)) "(")))
    `(("Functions"
      ,(concat prefix (rx (? "cl-") (or "defgeneric" "defmethod" "defun" "defadvice")) name)
      2)
      ("Dependencies"
       ,(concat prefix "require" name)
       2)
     ("Inline Functions"
      ,(concat prefix (rx (? "cl-") "defsubst") name)
      2)
     ("Customizations"
      ,(concat prefix "defcustom" name)
      2)
     ;; struct whose name maybe be wrapped in parens
     ("Types" ,(rx (group-n 1 (? "cl-") "defstruct" (1+ whitespace) (? "(" (0+ whitespace)))
                   (group-n 2 symbol-start (1+ (or (syntax word) (syntax symbol))) symbol-end))
      2)
     ("Types"
      ,(concat
        prefix
        (rx (group-n
             1 (or (seq (? "cl-") "defstruct" (? " ("))
                   "defclass"
                   "deftype"
                   "defgroup"
                   "define-widget"
                   "deferror")))
        name)
      2)
     ("Variables"
      ,(concat prefix (rx (or "defvar" "defvar-local" "defconst" "defconst-mode-local")) name)
      2)
     ("Macros"
      ,(concat prefix (rx (? "cl-") (or "define-compiler-macro" "defmacro")) name)
      2)
     ("Faces"
      ,(concat prefix (rx "defface") name)
      2)))
  "The value for `imenu-generic-expression' treemacs uses in elisp buffers.
More discriminating than the default as it distinguishes between functions,
inline functions, macros, faces, variables, customizations and types."
  :type 'alist
  :group 'treemacs-configuration)

(defcustom treemacs-project-follow-cleanup nil
  "When non-nil `treemacs-follow-mode' will close projects it is leaving.
This means that treemacs will make sure that only the currently followed project
is expanded while all others will remain collapsed.

Setting this to t might lead to noticeable slowdowns, at least when `treemacs-git-mode'
is enabled, since constantly expanding an entire project is farily expensive."
  :type 'boolean
  :group 'treemacs-configuration)

(defcustom treemacs-persist-file
  (f-join user-emacs-directory ".cache" "treemacs-persist")
  "Path to the file treemacs uses to persist its state."
  :group 'treemacs-configuration
  :type 'string)

(defcustom treemacs-space-between-root-nodes t
  "When non-nil treemacs will separate root nodes with an empty line."
  :type 'boolean
  :group 'treemacs-configuration)

(defcustom treemacs--fringe-indicator-bitmap
  (if (fboundp 'define-fringe-bitmap)
      (define-fringe-bitmap 'treemacs--fringe-indicator-bitmap
        (vector #b00000111
                #b00000111
                #b00000111
                #b00000111
                #b00000111
                #b00000111
                #b00000111
                #b00000111
                #b00000111
                #b00000111
                #b00000111
                #b00000111
                #b00000111
                #b00000111
                #b00000111
                #b00000111
                #b00000111
                #b00000111
                #b00000111
                #b00000111
                #b00000111
                #b00000111
                #b00000111
                #b00000111
                #b00000111
                #b00000111))
    'vertical-bar)
  "The fringe bitmap used by the fringe-indicator minor mode."
  :type 'fringe-bitmap
  :options (if (fboundp 'fringe-bitmaps)
               (cons 'treemacs--fringe-indicator-bitmap fringe-bitmaps)
             nil)
  :group 'treemacs-configuration)

(provide 'treemacs-customization)

;;; treemacs-customization.el ends here
