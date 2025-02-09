gleam-ts-mode: An Emacs Major Mode for [Gleam]
==============================================

[![MELPA](https://melpa.org/packages/gleam-ts-mode-badge.svg)](https://melpa.org/#/gleam-ts-mode)

This mode uses [tree-sitter] (and Emacs 29's `treesit` package) under-the-hood for syntax highlighting and code navigation.  If you're using an Emacs that's older than 29 (run `M-x version` to find out), or a version compiled without `treesit` (run `M-: (treesit-available-p)` to find out), you probably want [gleam-mode] instead.

https://user-images.githubusercontent.com/2058614/151681785-5d212e1b-191b-4e7d-a868-f6e42d7efe61.mp4

Setup
-----

This project is hosted on [MELPA], and this is the recommended way to install the package.  To add MELPA as a package repository, add the following to your Emacs init file:
```elisp
(require 'package)
;; Add MELPA to archives
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
;; Load and activate packages
(package-initialize)
```

### use-package (recommended)

Add the following to your Emacs init file:
```elisp
(use-package gleam-ts-mode
  :mode (rx ".gleam" eos))
```

### vanilla

Install with
```
M-x package-install RET gleam-ts-mode RET
```
(`M-x` stands for "meta + x" which indicates holding the meta key (usually labeled "Alt") and tapping "x".  Where you see `RET` you should press your "Enter" or "Return" key).

To load the package, run the following (or place it in your Emacs init file):
```elisp
(require "gleam-ts-mode")
(add-to-list 'auto-mode-alist '("\\.gleam\\'" . gleam-ts-mode))
```

### Install the Tree-Sitter Grammar

Unless you have the Gleam tree-sitter grammar installed and treesit knows where to find it, you'll want to run `M-x gleam-ts-install-grammar`.  It should only take a moment, but does require that your OS has a C compiler available.

TODO
----

- [x] Syntax highlighting
- [x] Indentation
- [x] Imenu integration
- [x] Formatting
- [ ] Completion?
- [ ] REPL?

Looking for the old gleam-mode?
-------------------------------

gleam-mode is deprecated and removed in favor of gleam-ts-mode.  However, if you still need gleam-mode for some reason, you can find it [in the git history](https://github.com/gleam-lang/gleam-mode/blob/8656c4080dd2bb7dd6d6167953d6463d090509b0/gleam-mode.el)

[gleam-mode]: https://github.com/gleam-lang/gleam-mode/blob/8656c4080dd2bb7dd6d6167953d6463d090509b0/gleam-mode.el
