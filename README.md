gleam-ts-mode: An Emacs Major Mode for [Gleam]
==============================================

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

---

gleam-mode: An Alternative Emacs Major Mode for [Gleam]
=======================================================

This mode uses [tree-sitter] alongside [tree-sitter-mode] and [tree-sitter-indent] for syntax highlighting and code navigation.

https://user-images.githubusercontent.com/2058614/151681785-5d212e1b-191b-4e7d-a868-f6e42d7efe61.mp4

Setup
-----

The best way to install this is first to clone the project:

```
$ git clone --recurse-submodules git@github.com:gleam-lang/gleam-mode
```

Then you'll need to load this from your init script (`~/.emacs` or `~/.config/emacs/init.el`).

### use-package (recommended)

```elisp
(use-package gleam-mode
  :load-path "~/path/to/gleam-mode")
```

Replace `~/path/to/gleam-mode` with the path where you cloned gleam-mode.

### vanilla

```elisp
(add-to-list 'load-path "~/path/to/gleam-mode")
(load-library "gleam-mode")
```

Replace `~/path/to/gleam-mode` with the path where you cloned gleam-mode.

Configuration
-------------

gleam-mode ships with a `gleam-format` command that is not bound by default to any keybinding. To bind it:

### use-package

Add `:bind (:map gleam-mode-map (<binding> . gleam-format))` to your use-package declaration. e.g.

```elisp
(use-package gleam-mode
  :load-path "~/path/to/gleam-mode"
  :bind (:map gleam-mode-map
              ("C-c g f" . gleam-format)))
```

(here "C-c g f" means `Control`+`C` followed by `g` followed by `f`)

### vanilla

Add the following after the lines where you setup gleam-mode:

```elisp
(define-key gleam-mode-map (kbd "C-c g f") 'gleam-format)
```

(here "C-c g f" means `Control`+`C` followed by `g` followed by `f`)

### automatically format on save

Alternatively, add these lines to your configuration to automatically run `gleam-format` on save:

``` elisp
    (add-hook 'gleam-mode-hook
              (lambda () (add-hook 'before-save-hook 'gleam-format nil t)))
```

TODO
----

- [x] Syntax highlighting
- [x] Indentation (at least somewhat)
- [x] Formatting
- [ ] Completion?
- [ ] REPL?


License
-------

This program is licensed under The Apache License, Version 2.0 or, at your option, under the terms of the GNU General Public License as published by the Free Software Foundation; either version 3, or (at your option) any later version. See [LICENSE-apache](./LICENSE-apache) for the terms of the Apache License, Version 2.0 or [LICENSE-gpl](./LICENSE-gpl) for the terms of the GNU Public License, Version 3.


[Gleam]: https://gleam.run
[tree-sitter]: https://github.com/tree-sitter/tree-sitter
[tree-sitter-mode]: https://emacs-tree-sitter.github.io
[tree-sitter-indent]: https://github.com/emacsmirror/tree-sitter-indent
[gleam-mode]: #gleam-mode-an-alternative-emacs-major-mode-for-gleam
[MELPA]: https://melpa.org
