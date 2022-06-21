;;; treemacs.el --- A tree style file viewer package -*- lexical-binding: t -*-

;; Copyright (C) 2021 Alexander Miller

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

;; Customize interface definitions.

;;; Code:

(require 's)
(require 'widget)
(require 'dash)

(eval-when-compile
  (require 'cl-lib))

(defun treemacs--find-python3 ()
  "Determine the location of python 3."
  (--if-let (executable-find "python3") it
    (when (eq system-type 'windows-nt)
      (condition-case _
          (->> "where python"
               (shell-command-to-string)
               (s-trim)
               (s-lines)
               (--first
                (when (file-exists-p it)
                  (condition-case _
                      (->> (concat (shell-quote-argument it) " --version")
                           (shell-command-to-string)
                           (s-trim)
                           (s-replace "Python " "")
                           (s-left 1)
                           (version<= "3"))
                    (error nil)))))
        (error nil)))))

(cl-macrolet
    ((define-action-widget (name include-default include-tab include-ret)
       `(define-widget ',name 'lazy
          "Treemacs button action"
          :format "%v"
          :type '(choice
                  :tag "Action"
                  ,@(when include-default `((const :tag "Default visit action" treemacs-visit-node-default)))
                  ,@(when include-tab `((const :tag "Same as TAB" treemacs-TAB-action)))
                  ,@(when include-ret `((const :tag "Same as RET" treemacs-RET-action)))
                  (const :tag "Visit node without splitting" treemacs-visit-node-no-split)
                  (const :tag "Visit node in a vertical split" treemacs-visit-node-vertical-split)
                  (const :tag "Visit node in a horizontal split" treemacs-visit-node-horizontal-split)
                  (const :tag "Visit node with Ace" treemacs-visit-node-ace)
                  (const :tag "Visit node with Ace in a horizontal split" treemacs-visit-node-ace-horizontal-split)
                  (const :tag "Visit node with Ace in a vertical split" treemacs-visit-node-ace-vertical-split)
                  (const :tag "Visit node in the most recently used window" treemacs-visit-node-in-most-recently-used-window)
                  (const :tag "Toggle node" treemacs-toggle-node)
                  (const :tag "Toggle node (prefer tag visit)" treemacs-toggle-node-prefer-tag-visit)
                  (function :tag "Custom function")))))
  (define-action-widget treemacs-default-action nil nil nil)
  (define-action-widget treemacs-ret-action t t nil)
  (define-action-widget treemacs-tab-action t nil t)
  (define-action-widget treemacs-mouse-action t t t))

(defgroup treemacs nil
  "Treemacs configuration options."
  :group 'treemacs
  :prefix "treemacs-")

(defgroup treemacs-faces nil
  "Faces for treemacs' syntax highlighting."
  :group 'treemacs
  :group 'faces
  :prefix "treemacs-"
  :link '(url-link :tag "Repository" "https://github.com/Alexander-Miller/treemacs"))

(defgroup treemacs-git nil
  "Customisations for treemacs' git integration."
  :group 'treemacs
  :prefix "treemacs-"
  :link '(url-link :tag "Repository" "https://github.com/Alexander-Miller/treemacs"))

(defgroup treemacs-hooks nil
  "Hooks provided by treemacs."
  :group 'treemacs
  :prefix "treemacs-"
  :link '(url-link :tag "Repository" "https://github.com/Alexander-Miller/treemacs"))

(defgroup treemacs-follow nil
  "Customisations for the behaviour of the treemacs' file and tag following."
  :group 'treemacs
  :prefix "treemacs-"
  :link '(url-link :tag "Repository" "https://github.com/Alexander-Miller/treemacs"))

(defgroup treemacs-window nil
  "Customisations for the behaviour of the treemacs window."
  :group 'treemacs
  :prefix "treemacs-"
  :link '(url-link :tag "Repository" "https://github.com/Alexander-Miller/treemacs"))

(defcustom treemacs-indentation 2
  "The number of spaces or pixels each level is indented in the file tree.
If the value is integer, indentation is created by repeating
`treemacs-indentation-string'.  If the value is a list of form \\='(INTEGER px),
indentation will be a space INTEGER pixels wide."
  :type '(choice (integer :tag "Spaces" :value 2)
                 (list :tag "Pixels"
                       (integer :tag "Pixels" :value 16)
                       (const :tag "" px)))
  :group 'treemacs)

(defcustom treemacs-litter-directories '("/node_modules" "/.venv" "/.cask")
  "List of directories affected by `treemacs-cleanup-litter'.
Every item in the list is a regular expression, to be recognised a directory
must be matched with `string-match-p'.

Regexp-quoting the items in this list is *not* necessary, the quoting will
happen automatically when needed."
  :type 'list
  :group 'treemacs)

(defcustom treemacs-read-string-input 'from-child-frame
  "The function treemacs uses to read user input.
Only applies to plaintext input, like when renaming a project, file or
workspace.

There are 2 options:
 - `from-child-frame': will use the `cfrs' package to read input from a small
   child frame pop-up.  Only available in GUI frames, otherwise the default
   minibuffer input is used.
 - `from-minibuffer': will read input from the minibuffer, same as baseline
   Emacs."
  :type '(choice (const :tag "With Child Frame Popup" from-child-frame)
                 (const :tag "From the Minibuffer (Emacs Default)" from-minibuffer))
  :group 'treemacs)

(defcustom treemacs-move-forward-on-expand nil
  "When non-nil treemacs will move to the first child of an expanded node."
  :type 'boolean
  :group 'treemacs)

(defcustom treemacs-eldoc-display 'simple
  "Enables eldoc display of the file path at point.

There are 2 options:
 - `simple': shows the absolute path of the file at point
 - `detailed': shows the absolute path, size, last modification time and
   permissions of the file at point

Requires eldoc mode to be enabled."
  :type '(choice (const :tag "Simple" simple)
                 (const :tag "Detailed" detailed))
  :group 'treemacs)

(defcustom treemacs-indent-guide-style 'line
  "Determines the appearance of `treemacs-indent-guide-mode'.
The choices are
 - `line' for indent guides to use the ' ┃ ' character for every indentation
   level
 - `block' to use a thick '██' block interspersed at every second indentation
   level"
  :type '(choice (const :tag "Line" line)
                 (const :tag "Block" block))
  :group 'treemacs)

(defcustom treemacs-indentation-string " "
  "The string that is for indentation in the file tree.
Indentation is created by repeating this string `treemacs-indentation' many
times.  If `treemacs-indentation' is specified in pixels, this value is only
used when there is no windowing system available."
  :type 'string
  :group 'treemacs)

(defcustom treemacs-show-hidden-files t
  "Dotfiles will be shown if this is set to t and be hidden otherwise.

Can be toggled by `treemacs-toggle-show-dotfiles'."
  :type 'boolean
  :group 'treemacs)

(defcustom treemacs-show-edit-workspace-help t
  "When non-nil the workspace-edit buffer will display a short help greeting.
See also `treemacs-edit-workspaces'."
  :type 'boolean
  :group 'treemacs)

(defcustom treemacs-TAB-actions-config
  '((root-node-open   . treemacs-toggle-node)
    (root-node-closed . treemacs-toggle-node)
    (dir-node-open    . treemacs-toggle-node)
    (dir-node-closed  . treemacs-toggle-node)
    (file-node-open   . treemacs-toggle-node)
    (file-node-closed . treemacs-toggle-node)
    (tag-node-open    . treemacs-toggle-node)
    (tag-node-closed  . treemacs-toggle-node)
    (tag-node         . treemacs-visit-node-default))
  "Defines the behaviour of `treemacs-TAB-action'.

See the doc string of `treemacs-RET-actions-config' for a detailed description
of how this config works and how to modify it."
  :type '(alist :key-type symbol :value-type treemacs-tab-action)
  :group 'treemacs)

(defcustom treemacs-doubleclick-actions-config
  '((root-node-open   . treemacs-toggle-node)
    (root-node-closed . treemacs-toggle-node)
    (dir-node-open    . treemacs-toggle-node)
    (dir-node-closed  . treemacs-toggle-node)
    (file-node-open   . treemacs-visit-node-in-most-recently-used-window)
    (file-node-closed . treemacs-visit-node-in-most-recently-used-window)
    (tag-node-open    . treemacs-toggle-node)
    (tag-node-closed  . treemacs-toggle-node)
    (tag-node         . treemacs-visit-node-in-most-recently-used-window))
  "Defines the behaviour of `treemacs-doubleclick-action'.

See the doc string of `treemacs-RET-actions-config' for a detailed description
of how this config works and how to modify it."
  :type '(alist :key-type symbol :value-type treemacs-mouse-action)
  :group 'treemacs)

(defcustom treemacs-default-visit-action
  'treemacs-visit-node-no-split
  "Defines the behaviour of `treemacs-visit-node-default'."
  :type 'treemacs-default-action
  :group 'treemacs)

(defcustom treemacs-RET-actions-config
  '((root-node-open   . treemacs-toggle-node)
    (root-node-closed . treemacs-toggle-node)
    (dir-node-open    . treemacs-toggle-node)
    (dir-node-closed  . treemacs-toggle-node)
    (file-node-open   . treemacs-visit-node-default)
    (file-node-closed . treemacs-visit-node-default)
    (tag-node-open    . treemacs-toggle-node-prefer-tag-visit)
    (tag-node-closed  . treemacs-toggle-node-prefer-tag-visit)
    (tag-node         . treemacs-visit-node-default))
  "Defines the behaviour of `treemacs-RET-action'.

Each alist element maps from a button state to the function that should be used
for that state.  The list of all possible button states is defined in
`treemacs-valid-button-states'.  Possible values are all treemacs-visit-node-*
functions as well as `treemacs-toggle-node' for simple open/close actions,
though in general you can use any function that accepts the prefix arg as its
single argument.

To keep the alist clean changes should not be made directly, but with
`treemacs-define-RET-action', for example like this:
\(treemacs-define-RET-action \\='file-node-closed #'treemacs-visit-node-ace)"
  :type '(alist :key-type symbol :value-type treemacs-ret-action)
  :group 'treemacs)

(defcustom treemacs-COLLAPSE-actions-config
  '((root-node-open   . treemacs-toggle-node)
    (root-node-closed . treemacs-goto-parent-node)
    (dir-node-open    . treemacs-toggle-node)
    (dir-node-closed  . treemacs-goto-parent-node)
    (file-node-open   . treemacs-toggle-node)
    (file-node-closed . treemacs-goto-parent-node)
    (tag-node-open    . treemacs-toggle-node)
    (tag-node-closed  . treemacs-goto-parent-node)
    (tag-node         . treemacs-goto-parent-node))
  "Defines the behaviour of `treemacs-COLLAPSE-action'.

See the doc string of `treemacs-RET-actions-config' for a detailed description
of how this config works and how to modify it."
  :type '(alist :key-type symbol :value-type treemacs-collapse-action)
  :group 'treemacs)

(defcustom treemacs-dotfiles-regex (rx bol "." (1+ any))
  "Files matching this regular expression count as dotfiles.
This controls the matching behaviour of `treemacs-toggle-show-dotfiles'."
  :type 'regexp
  :group 'treemacs)

(defcustom treemacs-hide-dot-git-directory t
  "Indicates whether the .git directory should be hidden.
When this is non-nil the .git dir will be hidden regardless of current setting
of `treemacs-toggle-show-dotfiles'."
  :type 'list
  :group 'treemacs)

(defcustom treemacs-sorting 'alphabetic-asc
  "Indicates how treemacs will sort its files and directories.
Files will still always be shown after directories.

Valid values are:
 * `alphabetic-asc',
 * `alphabetic-desc',
 * `alphabetic-case-insensitive-asc',
 * `alphabetic-case-insensitive-desc',
 * `size-asc',
 * `size-desc',
 * `mod-time-asc',
 * `mod-time-desc'
 * a custom function

In the latter case it must be a function that can be passed to `sort' to sort
absolute filepaths.  For an example see `treemacs--sort-alphabetic-asc'

Note about performance:
Treemacs does its best to optimise its performance critical path, it does so
by doing as little work as possible and producing as little garbage as possible.
Deciding on the order in which its nodes are inserted is a part of this path.
As such certain trade-offs need to be accounted far.

In plaintext: some sort settings are much slower than others.  Alphabetic
sorting (the default) is fastest and causes no additional overhead (even when
compared against foregoing sorting altogether).

Modification time sorting takes the middle, being ca.  4x slower than
alphabetic.  Sorting by size is slowest, being ca.  5-6x slower than alphabetic.
It also produces the most garbage, making it more like for you to run into a
garbage collection pause.

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
  :group 'treemacs)

(defcustom treemacs-ignored-file-predicates
  (pcase system-type
    ('darwin '(treemacs--std-ignore-file-predicate treemacs--mac-ignore-file-predicate))
    (_       '(treemacs--std-ignore-file-predicate)))
  "List of predicates to test for files and directories ignored by treemacs.

Ignored files will *never* be shown in the treemacs buffer (unlike dotfiles
whose presence is controlled by `treemacs-show-hidden-files').

Each predicate is a function that takes 2 arguments: a file's name and its
absolute path and returns t if the file should be ignored and nil otherwise.  A
file which returns t for *any* function in this list counts as ignored.

By default this list contains `treemacs--std-ignore-file-predicate' which
filters out \".\", \"..\", Emacs' lock files as well temp files created by
flycheck.  This means that this variable should *not* be set directly, but
instead modified with functions like `add-to-list'.

Additionally `treemacs--mac-ignore-file-predicate' is also included on
Mac-derived operating systems (when `system-type' is `darwin')."
  :type 'list
  :group 'treemacs)

(defcustom treemacs-pre-file-insert-predicates nil
  "List of predicates to test for files and directories that shouldn't be shown.
The difference between this and `treemacs-ignored-file-predicates' is that the
functions in this list will be called on files just before they would be
rendered, when the files' git status information is now available.  This for
example allows to make files ignored by git invisible (however this particular
use-case is already covered by `treemacs-hide-gitignored-files-mode').

The functions in this list are therefore expected to have a different signature:
They must take two arguments - a file's absolute path and a hash table that maps
files to their git status.  The files' paths are the table's keys, its values
are characters (and not strings) indicating the file's git condition.  The chars
map map as follows: (the pattern is derived from \\='git status --porcelain\\=')

 * M - file is modified
 * U - file is in conflict
 * ? - file is untracked
 * ! - file is ignored
 * A - file is added to index
 * other - file is unchanged

Otherwise the behaviour is the same as `treemacs-ignored-file-predicates', in
that any one function returning t for a file means that this file will not
be rendered."
  :type 'list
  :group 'treemacs)

(defcustom treemacs-file-event-delay 5000
  "How long (in milliseconds) to collect file events before refreshing.
When treemacs receives a file change notification it doesn't immediately refresh
and instead waits `treemacs-file-event-delay' milliseconds to collect further
file change events.  This is done so as to avoid refreshing multiple times in a
short time.
See also `treemacs-filewatch-mode'."
  :type 'integer
  :group 'treemacs)

(defcustom treemacs-goto-tag-strategy 'refetch-index
  "Indicates how to move to a tag when its buffer is dead.
The tags in the treemacs view store their position as markers (or overlays if
semantic mode is on) pointing to a buffer.  If that buffer is killed, or has
never really been open, as treemacs kills buffer after fetching their tags if
they did no exist before, the stored positions become stale, and treemacs needs
to use a different method to move to that tag.  This variable sets that method.

Its possible values are:

 * refetch-index
   Call up the file's imenu index again and use its information to jump.
 * call-xref
   Call `xref-find-definitions' to find the tag.
 * issue-warning
   Just issue a warning that the tag's position pointer is invalid."
  :type 'integer
  :group 'treemacs)

(defcustom treemacs-collapse-dirs 0
  "When > 0 treemacs will collapse directories into one when possible.
A directory is collapsible when its content consists of nothing but another
directory.

The value determines how many directories can be collapsed at once, both as a
performance cap and to prevent too long directory names in the treemacs view.

To minimise this option's impact on display performance the search for
directories to collapse is done asynchronously in a python script and will thus
only work when python installed.  The script should work both on python 2 and 3.

If you experience incorrect display of CJK characters while using this feature
you have to inform Emacs about your language environment using
`set-language-environment'."
  :type 'integer
  :group 'treemacs)

(defcustom treemacs-silent-refresh nil
  "When non-nil a completed refresh will not be announced with a message.
This applies to refreshing both manual as well as automatic (due to e.g.
`treemacs-filewatch-mode').
To only disable messages from refreshes induced by filewatch mode use
`treemacs-silent-filewatch'."
  :type 'boolean
  :group 'treemacs)

(defcustom treemacs-silent-filewatch nil
  "When non-nil a refresh due to filewatch mode will cause no log message.
To disable all refresh messages use `treemacs-silent-refresh'."
  :type 'boolean
  :group 'treemacs)

(defcustom treemacs-no-png-images nil
  "When non-nil treemacs will use TUI string icons even when running in a GUI.
The change will apply the next time a treemacs buffer is created."
  :type 'boolean
  :group 'treemacs)

(defcustom treemacs-expand-after-init t
  "When non-nil expand the first project after treemacs is first initialised.
Might be superseded by `treemacs-follow-after-init'."
  :type 'boolean
  :group 'treemacs)

(defcustom treemacs-expand-added-projects t
  "When non-nil newly added projects will be expanded."
  :type 'boolean
  :group 'treemacs)

(defcustom treemacs-recenter-after-project-jump 'always
  "Decides when to recenter view after moving between projects.
Specifically applies to calling `treemacs-next-project' and
`treemacs-previous-project'.

Possible values are:
 * nil: never recenter
 * \\='always: always recenter
 * \\='on-distance: recenter based on `treemacs-recenter-distance'"
  :type '(choice (const :tag "Always" always)
                 (const :tag "Based on Distance" on-distance)
                 (const :tag "Never" nil))
  :group 'treemacs)

(defcustom treemacs-recenter-after-project-expand 'on-visibility
  "Decides when to recenter view after expanding a project root node.

Possible values are:
 * nil: never recenter
 * \\='always: always recenter
 * \\='on-distance: recenter based on `treemacs-recenter-distance'
 * \\='on-visibility: recenter only when the newly rendered lines don't fit the
   current screen"
  :type '(choice (const :tag "Always" always)
                 (const :tag "Based on Distance" on-distance)
                 (const :tag "Based on Visibility" on-visibility)
                 (const :tag "Never" nil))
  :group 'treemacs)

(defcustom treemacs-pulse-on-success t
  "When non-nil treemacs will pulse the current line as a success indicator.
This applies to actions like `treemacs-copy-relative-path-at-point'."
  :type 'boolean
  :group 'treemacs)

(defcustom treemacs-pulse-on-failure t
  "When non-nil treemacs will pulse the current line as a failure indicator.
This applies to actions like treemacs not finding any tags it can show when
`treemacs-toggle-node' is called on a file node."
  :type 'boolean
  :group 'treemacs)

(defcustom treemacs-recenter-distance 0.1
  "Minimum distance from a window's top/bottom for treemacs to call `recenter'.
This value will apply when any one of the following options is set to
`on-distance':

 * treemacs-recenter-after-tag-follow
 * treemacs-recenter-after-file-follow
 * treemacs-recenter-after-project-jump
 * treemacs-recenter-after-project-expand

In that case a call to `recenter' will be made when the distance between point
and the top/bottom of the treemacs window is less then this many lines.  The
value is not an absolute line count, but a relative floating-point percentage,
with 0.0 being 0% and 1.0 being 100%.
This means that, for example, when this variable is set to 0.1 `recenter' will
be called within a 10% distance of the treemacs window's top/bottom.  For a
window height of 40 lines that means point being within the first or last 4
lines of the treemacs window.

Note that this does *not* take `scroll-margin' into account."
  :type 'float
  :group 'treemacs)

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
       ,(concat prefix (rx (? "cl-") (or "defsubst" "define-inline")) name)
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
inline functions, macros, faces, variables, customisations and types.

Can be set to nil to use the default value."
  :type 'alist
  :group 'treemacs)

(defcustom treemacs-persist-file
  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
  "Path to the file treemacs uses to persist its state.
Can be set to nil to disable workspace persistence."
  :group 'treemacs
  :type 'string)

(defcustom treemacs-last-error-persist-file
  (expand-file-name ".cache/treemacs-persist-at-last-error" user-emacs-directory)
  "File that stores the treemacs state as it was during the last load error."
  :group 'treemacs
  :type 'string)

(defcustom treemacs-missing-project-action 'ask
  "Action to perform when a persisted project is not found on the disk.
If the project is not found, the project can either be kept in the project list,
or removed from it.  If the project is removed, when projects are persisted, the
missing project will not appear in the project list next time Emacs is started."
  :type '(choice (const :tag "Ask whether to remove" ask)
                 (const :tag "Remove without asking" remove)
                 (const :tag "Keep without asking" keep))
  :group 'treemacs)

(defcustom treemacs-space-between-root-nodes t
  "When non-nil treemacs will separate root nodes with an empty line."
  :type 'boolean
  :group 'treemacs)

(defcustom treemacs-wrap-around t
  "When non-nil treemacs will wrap around buffer edges when moving between lines."
  :type 'boolean
  :group 'treemacs)

(defcustom treemacs--fringe-indicator-bitmap
  (if (fboundp 'define-fringe-bitmap)
      (define-fringe-bitmap 'treemacs--fringe-indicator-bitmap-default (make-vector 200 #b00000111))
    'vertical-bar)
  "The fringe bitmap used by the fringe-indicator minor mode."
  :type (append '(choice)
                ;; :type is evaluated before the call to define-fringe-bitmap
                ;; so 'treemacs--fringe-indicator-bitmap-default is not yet in
                ;; fringe-bitmaps
                '((const treemacs--fringe-indicator-bitmap-default))
                ;; `fringe-bitmpas' is void in the CI build Emacs
                (when (bound-and-true-p fringe-bitmaps)
                  (mapcar (lambda (sym) `(const ,sym)) fringe-bitmaps)))
  :group 'treemacs)

(defcustom treemacs-show-cursor nil
  "When non-nil treemacs the cursor will remain visible in the treemacs buffer."
  :type 'boolean
  :group 'treemacs)

(defcustom treemacs-directory-name-transformer #'identity
  "Transformer to apply to directory names before rendering for cosmetic effect."
  :type 'function
  :group 'treemacs)

(defcustom treemacs-file-name-transformer #'identity
  "Transformer to apply to file names before rendering for cosmetic effect."
  :type 'function
  :group 'treemacs)

(make-obsolete-variable 'treemacs-follow-recenter-distance 'treemacs-recenter-distance "v2.5")
(defcustom treemacs-follow-recenter-distance 0.1
  "Minimum distance from the top/bottom for (tag-)follow mode to recenter.
Treemacs will be calling `recenter' after following a file/tag if the distance
between point and the top/bottom of the treemacs window is less then this many
lines.  The value is not an absolute line count, but a percentage, with 0.0
being 0% and 1.0 being 100%.  This means that when this variable is set to 0.1
`recenter' will be called within a 10% distance of the window top/bottom.  For a
window height of 40 lines that means point being within the first or last 4
lines of the treemacs window.
Will only take effect if `treemacs-recenter-after-tag-follow' and/or
`treemacs-recenter-after-file-follow' is non-nil.

Note that this does *not* take `scroll-margin' into account."
  :type 'float
  :group 'treemacs-follow)

(defcustom treemacs-follow-after-init nil
  "When non-nil find the current file in treemacs after it is first initialised.
Might supersede `treemacs-expand-after-init'."
  :type 'boolean
  :group 'treemacs-follow)

(defcustom treemacs-file-follow-delay 0.2
  "Delay in seconds of idle time for treemacs to follow the selected window."
  :type 'number
  :group 'treemacs-follow)

(defcustom treemacs-tag-follow-delay 1.5
  "Delay in seconds of inactivity for `treemacs-tag-follow-mode' to trigger."
  :type 'number
  :group 'treemacs-follow)

(defcustom treemacs-tag-follow-cleanup t
  "When non-nil `treemacs-tag-follow-mode' will close file nodes it is leaving.
When jumping between different files this can prevent the view from being
flooded with their tags."
  :type 'boolean
  :group 'treemacs-follow)

(defcustom treemacs-recenter-after-file-follow nil
  "Decides when to recenter view after following a file.
Possible values are:
 * nil: never recenter
 * \\='always: always recenter
 * \\='on-distance: recenter based on `treemacs-recenter-distance'"
  :type '(choice (const :tag "Always" always)
                 (const :tag "Based on Distance" on-distance)
                 (const :tag "Never" nil))
  :group 'treemacs-follow)

(defcustom treemacs-recenter-after-tag-follow nil
  "Decides when to recenter view after following a tag.
Possible values are:
 * nil: never recenter
 * \\='always: always recenter
 * \\='on-distance: recenter based on `treemacs-recenter-distance'"
  :type '(choice (const :tag "Always" always)
                 (const :tag "Based on Distance" on-distance)
                 (const :tag "Never" nil))
  :group 'treemacs-follow)

(defcustom treemacs-project-follow-cleanup nil
  "When non-nil `treemacs-follow-mode' will close projects it is leaving.
This means that treemacs will make sure that only the currently followed project
is expanded while all others will remain collapsed.

Setting this to t might lead to noticeable slowdowns, at least when
`treemacs-git-mode' is enabled, since constantly expanding an entire project is
fairly expensive."
  :type 'boolean
  :group 'treemacs-follow)

(defcustom treemacs-deferred-git-apply-delay 0.5
  "Delay in seconds of idle time before git fontification is applied.
This is only relevant when using the deferred variant of git-mode."
  :type 'number
  :group 'treemacs-git)

(defcustom treemacs-max-git-entries 5000
  "Maximum number of git status entries treemacs will process.
Information for entries that number will be silently ignored.  The \"entries\"
refer to the lines output by `git status --porcelain --ignored=matching'.  The
limit does not apply to the simple `treemacs-git-mode.'"
  :type 'number
  :group 'treemacs-git)

(defcustom treemacs-python-executable (treemacs--find-python3)
  "The python executable used by treemacs.
An asynchronous python process is used in two optional features:
`treemacs-collapse-dirs' and the extended variant of `treemacs-git-mode'.

There is generally only one reason to change this value: an extended
`treemacs-git-mode' requires python3 to work.  If the default python executable
is pointing to python2 this config variable can be used to direct treemacs to
the python3 binary."
  :type 'string
  :group 'treemacs-git)

(defcustom treemacs-git-command-pipe ""
  "Text to be appended to treemacs' git command.
With `treemacs-git-mode' the command
`git status --porcelain --ignored=matching .' is run to fetch a directory's git
information.  The content of this variable will be appended to this git command.
This might be useful in cases when the output produced by git is so large that
it leads to palpable delays, while setting `treemacs-max-git-entries' leads to
loss of information.  In such a scenario an additional filter statement (for
example `| grep -v \"/vendor_dir/\"') can be used to reduce the size of the
output to a manageable volume for treemacs."
  :type 'string
  :group 'treemacs-git)

(defcustom treemacs-is-never-other-window nil
  "When non-nil treemacs will use the `no-other-window' parameter.

In practice it means that treemacs will become invisible to commands like
`other-window' or `evil-window-left'."
  :type 'boolean
  :group 'treemacs-window)

(defcustom treemacs-width-is-initially-locked t
  "Indicates whether the width of the treemacs window is initially locked.
A locked width means that changes it is only possible with the commands
`treemacs-set-width' or `treemacs-toggle-fixed-width'."
  :type 'boolean
  :group 'treemacs-window)

(defcustom treemacs-window-background-color nil
  "Custom background colours for the treemacs window.
Value must be a cons cell consisting of two colours: first the background of the
treemacs window proper, then a second colour for treemacs' `hl-line' overlay
marking the selected line."
  :type '(cons color color)
  :group 'treemacs-window)

(defcustom treemacs-width 35
  "Width of the treemacs window."
  :type 'integer
  :group 'treemacs-window)

(defcustom treemacs-wide-toggle-width 70
  "When resizing, this value is added or subtracted from the window width."
  :type 'integer
  :group 'treemacs-window)

(defcustom treemacs-width-increment 1
  "When resizing, this value is added or subtracted from the window width."
  :type 'integer
  :group 'treemacs-window)

(defcustom treemacs-display-in-side-window t
  "When non-nil treemacs will use a dedicated side-window.
On the one hand this will alleviate issues of unequally sized window splits when
treemacs is visible (since Emacs does not quite understand that treemacs has
fixed window size).  On the other hand it may lead to issues with other packages
like shell-pop, as making treemacs a side-window renders it un-splittable."
  :type 'boolean
  :group 'treemacs-window)

(defcustom treemacs-no-delete-other-windows t
  "When non-nil treemacs will have the `no-delete-other-windows' parameter.
This parameter prevents the treemacs window from closing when calling
`delete-other-windows' or when a command like `magit-status' would launch a new
full-screen buffer.
Note that treemacs has its own delete-windows command with
`treemacs-delete-other-windows' that behaves the same as `delete-other-windows',
but won't close treemacs itself.
This parameter was only introduced in Emacs 26. On Emacs 25 its effect is
included in `treemacs-display-in-side-window'."
  :type 'boolean
  :group 'treemacs-window)

(defcustom treemacs-user-header-line-format nil
  "The header line used in the treemacs window.
Can be set either to `treemacs-header-buttons-format' or any one of its
constituent parts, or any other value acceptable for `header-line-format'."
  :type 'string
  :group 'treemacs-window)

(defcustom treemacs-text-scale nil
  "Optional scale for the text (not the icons) in the treemacs window.
If set the value will be passed to `text-scale-increase'.  Both positive and
negative values are possible."
  :type 'integer
  :group 'treemacs-window)

(defcustom treemacs-header-scroll-indicators '(nil . "^^^^^^")
  "The strings used for `treemacs-indicate-top-scroll-mode'.
The value must be a cons, where the car is the string used when treemacs is
scrolled all the way to the top, and the cdr is used when it isn't."
  :type '(cons string string)
  :group 'treemacs-window)

(defcustom treemacs-select-when-already-in-treemacs 'move-back
  "How `treemacs-select-window' behaves when treemacs is already selected.

Possible values are:
 - `stay' - remain in the treemacs windows, effectively doing nothing
 - `close' - close the treemacs window
 - `move-back' - move point back to the most recently used window (as selected
    by `get-mru-window')"
  :type '(choice (const stay)
                 (const close)
                 (const move-back))
  :group 'treemacs)

(defcustom treemacs-position 'left
  "Position of treemacs buffer.

Valid values are
 * `left',
 * `right'"
  :type '(choice (const left)
                 (const right))
  :group 'treemacs)

(defcustom treemacs-post-buffer-init-hook nil
  "Hook run after a treemacs buffer is first initialised.
Only applies to treemacs filetree buffers, not extensions."
  :type 'hook
  :group 'treemacs-hooks)

(defcustom treemacs-post-project-refresh-functions nil
  "Hook that runs after a project was updated with `treemacs-refresh'.
Will be called with the new project as the sole argument."
  :type 'hook
  :group 'treemacs-hooks)

(defcustom treemacs-create-project-functions nil
  "Hooks to run whenever a project is created.
Will be called with the new project as the sole argument."
  :type 'hook
  :group 'treemacs-hooks)

(defcustom treemacs-create-file-functions nil
  "Hooks to run whenever a file or directory is created.
Applies only when using `treemacs-create-file' or `treemacs-create-dir'.
Will be called with the created file's or dir's path as the sole argument."
  :type 'hook
  :group 'treemacs-hooks)

(defcustom treemacs-delete-file-functions nil
  "Hooks to run whenever a file or directory is deleted.
Applies only when using `treemacs-delete'.  Will be called with the created
file's or dir's path as the sole argument."
  :type 'hook
  :group 'treemacs-hooks)

(defcustom treemacs-rename-file-functions nil
  "Hooks to run whenever a file or directory is renamed.

Applies only when using `treemacs-rename'.  Will be called with 2 arguments: the
file's old name, and the file's new name, both as absolute paths."
  :type 'hook
  :group 'treemacs-hooks)

(defcustom treemacs-move-file-functions nil
  "Hooks to run whenever a file or directory is moved.

Applies only when using `treemacs-move-file'.  Will be called with 2 arguments:
the file's old location, and the file's new location, both as absolute paths."
  :type 'hook
  :group 'treemacs-hooks)

(defcustom treemacs-copy-file-functions nil
  "Hooks to run whenever a file or directory is copied.

Applies only when using `treemacs-copy-file'.  Will be called with 2 arguments:
the original file's location, and the copy's location, both as absolute paths."
  :type 'hook
  :group 'treemacs-hooks)

(defcustom treemacs-delete-project-functions nil
  "Hooks to run whenever a project is deleted.
Will be called with the deleted project as the sole argument *after* it has been
deleted."
  :type 'hook
  :group 'treemacs-hooks)

(defcustom treemacs-find-workspace-method 'find-for-file-or-pick-first
  "The method by which treemacs selects a workspace when first starting.
There are 3 options:
 - `find-for-file-or-pick-first' means treemacs will select the first workspace
   with a project that contains the current buffer's file.  If no such workspace
   exists, or if the current buffer is not visiting a file, the first workspace
   in the list (as seen in `treemacs-edit-workspaces' or picked with
   `treemacs-set-fallback-workspace') is selected
 - `find-for-file-or-manually-select' works the same, but an interactive
   selection is used as fallback instead
 - `always-ask' means the workspace *always* has to be manually selected

Note that the selection process will be skipped if there is only one workspace."
  :type '(choice (const
                  :tag "Find workspace for current file, pick the first workspace as falback"
                  find-for-file-or-pick-first)
                 (const
                  :tag "Find workspace for current file, interactively select workspace as falback"
                  find-for-file-or-manually-select)
                 (const :tag "Always ask" always-ask))
  :group 'treemacs-hooks)

(defcustom treemacs-rename-project-functions nil
  "Hooks to run whenever a project is renamed.
Will be called with the renamed project and the old name as its arguments."
  :type 'hook
  :group 'treemacs-hooks)

(defcustom treemacs-create-workspace-functions nil
  "Hooks to run whenever a workspace is created.
Will be called with the new workspace as the sole argument."
  :type 'hook
  :group 'treemacs-hooks)

(defcustom treemacs-delete-workspace-functions nil
  "Hooks to run whenever a workspace is deleted.
Will be called with the deleted workspace as the sole argument *after* it has
been deleted."
  :type 'hook
  :group 'treemacs-hooks)

(defcustom treemacs-rename-workspace-functions nil
  "Hooks to run whenever a workspace is renamed.
Will be called with the renamed workspace and the old name as its arguments."
  :type 'hook
  :group 'treemacs-hooks)

(defcustom treemacs-switch-workspace-hook nil
  "Hooks to run whenever the workspace is changed.
The current workspace will be available as `treemacs-current-workspace'."
  :type 'hook
  :group 'treemacs-hooks)

(defcustom treemacs-workspace-edit-hook nil
  "Hooks to run whenever the entire workspace layout has been rebuilt.
This hook runs after `treemacs-finish-edit' has been called.  After such an edit
any number (including zero) of workspaces and projects may have been changed or
created or deleted."
  :type 'hook
  :group 'treemacs-hooks)

(defcustom treemacs-bookmark-title-template "Treemacs - ${project}: ${label}"
  "Template for default bookmark titles.

The following replacements are available:
 * ${project}: The label of the project.
 * ${label}: Label of the current button.
 * ${label:N} Label of the Nth parent.
   If the parent does not exist, an empty string.
 * ${label-path}: Label path of the button.
   For example, \"Project/directory/file.txt\"
 * ${label-path:N}: Last N components of the label path.
 * ${file-path}: Absolute file-system path of the node.
   If the node is a top-level extension node, this expands to an empty string.
   If the node is a directory or or project extension, the path of its parent.
 * ${file-path:N}: Last N components of the filesystem path."
  :type 'string
  :group 'treemacs)

(defcustom treemacs-pre-refresh-hook nil
  "Hooks to run right before the refresh process for a project kicks off.
During the refresh the project is effectively collapsed and then expanded again.
This hook runs *before* that happens.  It runs with treemacs as the
`current-buffer' and receives as its arguments all the information that treemacs
collects for its refresh process:
 * The project being refreshed (might be \\='all)
 * The current screen-line number (can be nil).
 * The current button.  Might be nil if point is on the header line.
 * The current button's state.  See also `treemacs-valid-button-states'.  Is nil
   if the current button is nil.
 * The nearest file path, as collected with `treemacs--nearest-path'.  Is nil if
   point is on the header.
 * The current button's tag path.  Is nil if the current button is nil."
  :type 'hook
  :group 'treemacs-hooks)

(defcustom treemacs-post-refresh-hook nil
  "Hooks to run right before the refresh process is finished off.
During the refresh the project is effectively collapsed and then expanded again.
This hook runs *after* that has happened.  It runs with treemacs as the
`current-buffer' and receives as its arguments all the information that treemacs
collects for its refresh process.  Note that these values were collected at the
start of the refresh, and may now be longer valid (for example the current
button's position will be wrong, even if it wasn't deleted outright):
 * The project being refreshed (might be \\='all)
 * The current screen-line number (can be nil).
 * The current button.  Might be nil if point was on the header line.
 * The current button's state.  See also `treemacs-valid-button-states'.  Is nil
   if the current button is nil.
 * The nearest file path, as collected with `treemacs--nearest-path'.  Is nil if
   point was on the header.
 * The current button's tag path.  Is nil if the current button is nil."
  :type 'hook
  :group 'treemacs-hooks)

(defcustom treemacs-quit-hook nil
  "Hooks to run when `treemacs-quit' is called.
The hooks will be run *after* the treemacs buffer was buried."
  :type 'hook
  :group 'treemacs-hooks)

(defcustom treemacs-kill-hook nil
  "Hooks to run when `treemacs-kill-buffer' is called.
The hooks will be run *after* the treemacs buffer was destroyed."
  :type 'hook
  :group 'treemacs-hooks)

(define-obsolete-variable-alias 'treemacs-select-hook 'treemacs-select-functions "2.9")

(defcustom treemacs-select-functions nil
  "Hooks to run when the treemacs window is selected.
The hook should accept one argument which is a symbol describing treemacs'
visibility before the select was invoked, as it would have been returned by
`treemacs-current-visibility'.

This hook only applies to commands like `treemacs' or `treemacs-select-window',
not general window selection commands like `other-window'."
  :type 'hook
  :group 'treemacs-hooks)

(defcustom treemacs-workspace-first-found-functions nil
  "Hooks that run when treemacs finds a workspace for the first time.
Hooks are expected to take 2 arguments: the workspace that was found and the
current scope (frame or perspective) it was found for."
  :type 'hook
  :group 'treemacs-hooks)

(defconst treemacs-last-period-regex-value "\\.[^.]*\\'")
(defconst treemacs-first-period-regex-value "\\.")
(defcustom treemacs-file-extension-regex treemacs-last-period-regex-value
  "Decides how treemacs determines a file's extension.
There are 2 options:
 - An extension should be everything past the *last* period of the file name.
   In this case this should be set to `treemacs-last-period-regex-value'
 - An extension should be everything past the *first* period of the file name.
   In this case this should be set to `treemacs-first-period-regex-value'"
  :group 'treemacs
  :type `(choice (const :tag "Text after first period" ,treemacs-first-period-regex-value)
                 (const :tag "Text after last period" ,treemacs-last-period-regex-value)))

(defcustom treemacs-user-mode-line-format nil
  "Custom mode line format to be used in `treemacs-mode'.

If nil treemacs will look for default value provided by `spaceline', `moody'
or `doom-modeline' in that order.  Finally, if none of these packages is
available \"Treemacs\" text will be displayed.

Setting this to `none' will disable the modeline.

For more specific information about formatting mode line check
`mode-line-format'."
  :type 'sexp
  :group 'treemacs)

(defcustom treemacs-workspace-switch-cleanup nil
  "Indicates which, if any, buffers should be deleted on a workspace switch.
Only applies when interactively calling `treemacs-switch-workspace'.

Valid values are
 - nil to do nothing
 - `files' to delete buffers visiting files
 - `all' to delete all buffers other than treemacs and the scratch buffer

In any case treemacs itself and the scratch and messages buffer will be
unaffected."
  :type '(choice (const :tag "All Buffers" all)
                 (const :tag "Only File Buffers" files)
                 (const :tag "None" nil))
  :group 'treemacs)

(defcustom treemacs-imenu-scope 'everything
  "Determines which items treemacs' imenu function will collect.
There are 2 options:
 - `everything' will collect entries from every project in the workspace.
 - `current-project' will only gather the index for the project at point."
  :type '(choice (const :tag "Everything" everything)
                 (const :tag "Current Project Only" current-project))
  :group 'treemacs)

(provide 'treemacs-customization)

;;; treemacs-customization.el ends here
