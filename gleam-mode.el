;;; gleam-mode.el --- Major mode for Gleam -*- lexical-binding: t -*-

;; Copyright © 2021-2022 Louis Pilfold <louis@lpil.uk>
;; Authors: Jonathan Arnett <jonathan.arnett@protonmail.com>
;;
;; URL: https://github.com/gleam-lang/gleam-mode
;; Keywords: languages gleam
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1") (tree-sitter "0.15.0"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this package. If not, see https://www.gnu.org/licenses.

;;; Commentary:

;; Provides syntax highlighting, indentation, and code navigation
;; features for the Gleam programming language.

;;; Code:

(require 'tree-sitter)
(require 'tree-sitter-indent)


;;; Customization
(defgroup gleam nil
  "Major mode for Gleam"
  :prefix "gleam-"
  :group 'languages)

(defcustom gleam-mode-hook nil
  "Hook that runs when gleam-mode starts."
  :type 'hook)

(defvar gleam-indent-offset 2
  "Offset used to indent Gleam code.")


;;; Mode definitions

(defvar gleam-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap used in gleam-mode buffers.")

(defcustom tree-sitter-indent-gleam-scopes
  '((indent-all . ())
    (indent-rest . (
                    target_group
                    import
                    function
                    public_function
                    anonymous_function
                    external_function
                    public_external_function
                    type_definition
                    public_type_definition
                    public_opaque_type_definition
                    type_alias
                    public_type_alias
                    arguments
                    parameters
                    case
                    case_clause))
    (indent-body . ())
    (paren-indent . ())
    (multi-line-text . ())
    (outdent . ("}" ")")))
  "`tree-sitter-indent' indentation rules based on AST.")

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.gleam" . gleam-mode))

;;;###autoload
(define-derived-mode gleam-mode prog-mode "Gleam"
  "Major mode for Gleam.

Key bindings:
\\{gleam-mode-map}"

  ;;; Configure tree-sitter and friends

  ;; Compile tree-sitter grammar if we haven't already
  (unless (file-exists-p (concat gleam-mode--tree-sitter-dir "gleam.so"))
    (gleam-mode--compile-grammar))

  (add-to-list 'tree-sitter-load-path gleam-mode--tree-sitter-dir)
  (tree-sitter-load 'gleam)
  (add-to-list 'tree-sitter-major-mode-language-alist '(gleam-mode . gleam))
  (setq tree-sitter-hl-default-patterns (gleam-mode--read-highlight-query))

  (tree-sitter-mode)
  (tree-sitter-hl-mode)
  (tree-sitter-indent-mode)

  ;;; Configure Emacs settings
  (setq-local indent-tabs-mode nil))


;;; Public functions
(defun gleam-format ()
  "Format the current buffer using the `gleam format' command."
  (interactive)
  (if (executable-find "gleam")
    (save-restriction ; Save the user's narrowing, if any
      (widen)         ; Expand scope to the whole, unnarrowed buffer
      (let* ((buf (current-buffer))
             (min (point-min))
             (max (point-max))
             (tmpfile (make-nearby-temp-file "gleam-format")))
        (unwind-protect
            (with-temp-buffer
              (insert-buffer-substring-no-properties buf min max)
              (write-file tmpfile)
              (call-process "gleam" nil nil nil "format" (buffer-file-name))
              (revert-buffer :ignore-autosave :noconfirm)
              (let ((tmpbuf (current-buffer)))
                (with-current-buffer buf
                  (replace-buffer-contents tmpbuf))))
          (if (file-exists-p tmpfile) (delete-file tmpfile)))
        (message "Formatted!")))
    (message "`gleam' executable not found!")))


;;; Private functions

(defconst gleam-mode--dir
  (file-name-directory (locate-library "gleam-mode.el"))
  "The directory where the library `gleam-mode' is located.")

(defconst gleam-mode--tree-sitter-dir
  (concat gleam-mode--dir
          (file-name-as-directory "tree-sitter-gleam"))
  "The directory where `tree-sitter-gleam' is located.")

(defconst gleam-mode--queries-dir
  (concat gleam-mode--tree-sitter-dir
          (file-name-as-directory "queries"))
  "The directory where the tree-sitter-gleam query files can be found.")

(defconst gleam-mode--highlights-query-file
  (concat gleam-mode--queries-dir "highlights.scm")
  "The file containing the highlight queries for tree-sitter-gleam.")

(defun gleam-mode--compile-grammar ()
  "Compile the tree-sitter-grammar to a shared library for loading."
  (if (executable-find "cc")
      (let ((default-directory gleam-mode--tree-sitter-dir))
        (call-process "cc" nil nil nil
                        "-shared"
                        "-fPIC"
                        "-g"
                        "-O2"
                        "-I" "src"
                        "src/parser.c"
                        "-o" "gleam.so"))
    (message "A C compiler is required to build the tree-sitter grammar.")))

(defun gleam-mode--read-highlight-query ()
  "Read the contents of the tree-sitter-gleam highlight query."
  (with-temp-buffer
    (insert-file-contents gleam-mode--highlights-query-file)
    (buffer-string)))

(provide 'gleam-mode)
;;; gleam-mode.el ends here
