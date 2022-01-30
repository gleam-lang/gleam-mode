An Emacs Major Mode for [Gleam]
===============================

This mode uses [tree-sitter] under-the-hood for syntax highlighting and code
navigation.

https://user-images.githubusercontent.com/2058614/151681785-5d212e1b-191b-4e7d-a868-f6e42d7efe61.mp4

Setup
-----

This project will eventually be hosted on [MELPA]. However, until that time, the
best way to install this is first to clone the project:

```
$ git clone --recurse-submodules git@github.com:gleam-lang/gleam-mode
```

Then you'll need to load this from your init script (`~/.emacs` or
`~/.config/emacs/init.el`).

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

gleam-mode ships with a `gleam-format` command that is not bound by default to
any keybinding. To bind it:

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

TODO
----

- [x] Syntax highlighting
- [x] Indentation (at least somewhat)
- [x] Formatting
- [ ] Completion?
- [ ] REPL?

[Gleam]: https://gleam.run
[tree-sitter]: https://github.com/tree-sitter/tree-sitter
[MELPA]: https://melpa.org
