# Treemacs -- a tree style file explorer for Emacs

![](screenshot.png)

## Features

Treemacs is a file explorer like NeoTree or vim's NerdTree. It's displays the file system in a tree layout to allow easy exploration and
navigation as well as *basic* file management utilities. Specifically the single selling points are as follows:

 * Easy navigation - quickly change root directory, or use shortcuts to jump between parent and neighbouring nodes.
 * Versatile file access - decide exactly how and where a file will be opened, including using `ace-window` to chose a buffer or launching
   an external app via `xdg-open`.
 * Projectile integration - open treemacs at your project's root directory.
 * Window numbering integration - Treemacs will always be assgined window number 0, never interferig with the numbering
   layout of your other windows
 * Ease of use - you need to run a single setup function (and optionally define your own keybindings) in order to chose whether to use default
   or evil key bindings. The keybinds themselves are kept simple and mnemonic (as in spacemacs).

## Installation

 Treemacs is still in testing and not yet available on MELPA, thus for now must be cloned and
 loaded manually.

 To finish the installation process execute either `treemacs-default-config` or `treemacs-evil-config`,
 depending on whether you want to use n/p or j/k to navigate up and down. All other keybindings
 will be the same between both configurations.

 Once this is done Treemacs is ready to be used by calluing `treemacs-init`, `treemacs-toggle` or `treemacs-projectile-init`.

## Configuration

Treemacs offers the following configuration variables:

 * `treemacs-indentation` (default value: 2)

   The number of spaces each level is indented in the tree.

 * `treemacs-width` (default value: 35)

   Width of the treemacs buffer.

 * `treemacs-show-hidden-files` (default value: t)

   Dotfiles will be shown if this is set to t and be hidden otherwise.

 * `treemacs-header-format` (default value: "\*%s\*")

    The format string which is used for the header line. Valid formats are all strings
    accepted by the `format' function for a single formatting argument, which is the current root directory.

In addition treemacs defines the following faces:

 * `treemacs-directory-face`
   Face used by treemacs for directories.

 * `treemacs-file-face`
  Face used by treemacs for files.

 * `treemacs-header-face`
  Face used by treemacs for its header.


## Keymap TODO

By default Treemacs's keymap looks as follows:

| Key | action | Description |
|-----|--------|-------------|
| j/n | treemacs-next-line | Goto next line.|
