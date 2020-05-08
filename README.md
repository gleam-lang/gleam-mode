# gleam-mode

An emacs major mode for the [Gleam](https://github.com/gleam-lang/gleam) programming language.

Currently this provides syntax highlighting.

## Installation

If you're using vanilla Emacs, clone this repository into your `.emacs.d` directory or a directory inside this one, e.g. `private`:

```bash
$ cd ~/.emacs.d/private/
$ git clone git@github.com:gleam-lang/gleam-mode.git
```

And in your `init.el` file, add:

```elisp
;;
(load-file "~/.emacs.d/private/gleam-mode/gleam-mode.el")
(require 'gleam-mode)
(add-to-list 'auto-mode-alist '("\\.gleam$" . gleam-mode))
```

If you're using Spacemacs, you can follow the same steps, and add the elisp code in your dotfile's `dotspacemacs/user-config` function.
