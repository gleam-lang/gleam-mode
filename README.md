An Emacs Major Mode for [Gleam]
===============================

This mode uses [tree-sitter] (and Emacs 29's `treesit` package) under-the-hood for syntax highlighting and code navigation.  If you're using an Emacs that's older than 29 (run `M-x version` to find out), or a version compiled without `treesit` (run `M-: (treesit-available-p)` to find out), you probably want [gleam-mode] instead.

https://user-images.githubusercontent.com/2058614/151681785-5d212e1b-191b-4e7d-a868-f6e42d7efe61.mp4

Setup
-----

This project will eventually be hosted on [MELPA]. However, until that time, the best way to install this is first to clone the project:

```
$ git clone git@github.com:gleam-lang/gleam-ts-mode
```

Then you'll need to load this from your init script (`~/.emacs` or
`~/.config/emacs/init.el`).

### use-package (recommended)

```elisp
(use-package gleam-ts-mode
  :load-path "~/path/to/gleam-ts-mode")
```

Replace `~/path/to/gleam-ts-mode` with the path where you cloned gleam-mode.

### vanilla

```elisp
(add-to-list 'load-path "~/path/to/gleam-ts-mode")
(load-library "gleam-ts-mode")
```

Replace `~/path/to/gleam-ts-mode` with the path where you cloned gleam-mode.

### Install the Tree-Sitter Grammar

Unless you have the Gleam tree-sitter grammar installed and treesit knows where to find it, you'll want to run `M-x gleam-ts-install-grammar`.  It should only take a moment, but does require that your OS has a C compiler available.

TODO
----

- [x] Syntax highlighting
- [ ] Indentation
- [x] Imenu integration
- [x] Formatting
- [ ] Completion?
- [ ] REPL?

[Gleam]: https://gleam.run
[tree-sitter]: https://github.com/tree-sitter/tree-sitter
[gleam-mode]: https://github.com/gleam-lang/gleam-mode
[MELPA]: https://melpa.org

License
-------

This program is licensed under The Apache License, Version 2.0 or, at your option, under the terms of the GNU General Public License as published by the Free Software Foundation; either version 3, or (at your option) any later version. See [LICENSE-apache](./LICENSE-apache) for the terms of the Apache License, Version 2.0 or [LICENSE-gpl](./LICENSE-gpl) for the terms of the GNU Public License, Version 3.
