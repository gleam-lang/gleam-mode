;;; gleam-ts-mode.el --- Major mode for Gleam -*- lexical-binding: t -*-

;; Copyright © 2024 Louis Pilfold <louis@lpil.uk>
;; Authors: Jonathan Arnett <jonathan.arnett@protonmail.com>
;;
;; URL: https://github.com/gleam-lang/gleam-mode
;; Keywords: languages gleam
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

;; This file is NOT part of GNU Emacs.

;; This program is licensed under The Apache License¹, Version 2.0 or,
;; at your option, under the terms of the GNU General Public License²
;; as published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.

;; ¹ You may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;; http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;; ² This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this package. If not, see https://www.gnu.org/licenses.

;;; Commentary:

;; Provides syntax highlighting, indentation, and code navigation
;; features for the Gleam programming language.

;;; Code:

(require 'prog-mode)
(require 'treesit)


;;; Customization

(defgroup gleam-ts nil
  "Major mode for editing Gleam."
  :prefix "gleam-ts-"
  :group 'languages)

(defcustom gleam-ts-indent-offset 2
  "Offset used to indent Gleam code."
  :type 'integer
  :safe 'integerp
  :group 'gleam-ts)


;;; Tree-sitter font locking

(defface gleam-ts-constructor-face
  '((t (:inherit font-lock-type-face)))
  "Font used for highlighting Gleam type constructors.")

(defface gleam-ts-module-face
  '((t (:inherit font-lock-variable-name-face)))
  "Font used for highlighting Gleam modules.")

(defvar gleam-ts--font-lock-settings
  (treesit-font-lock-rules
   :feature 'comment
   :language 'gleam
   '((comment) @font-lock-comment-face)

   :feature 'string
   :language 'gleam
   '((string) @font-lock-string-face)

   :feature 'number
   :language 'gleam
   '((integer) @font-lock-number-face
     (float) @font-lock-number-face)

   :feature 'function-name
   :language 'gleam
   '((unqualified_import (identifier) @font-lock-function-name-face)
     (function
      name: (identifier) @font-lock-function-name-face)
     (external_function
      name: (identifier) @font-lock-function-name-face)
     (function_call
      function: (identifier) @font-lock-function-name-face))

   :feature 'variable-name
   :language 'gleam
   '((identifier) @font-lock-variable-name-face)

   :feature 'constructor
   :language 'gleam
   '((unqualified_import (type_identifier) @gleam-ts-constructor-face)
     (constructor_name) @gleam-ts-constructor-face)

   :feature 'type-name
   :language 'gleam
   '((unqualified_import "type" (type_identifier) @font-lock-type-face)
     (remote_type_identifier) @font-lock-type-face
     (type_identifier) @font-lock-type-face)

   :feature 'constant-name
   :language 'gleam
   :override t
   '((constant
      name: (identifier) @font-lock-constant-face))

   :feature 'keyword
   :language 'gleam
   '([
      (visibility_modifier)
      (opacity_modifier)
      "as"
      "assert"
      "case"
      "const"
      ;; Deprecated
      "external"
      "fn"
      "if"
      "import"
      "let"
      "panic"
      "todo"
      "type"
      "use"
      ] @font-lock-keyword-face)

   :feature 'operator
   :language 'gleam
   '((binary_expression operator: _ @font-lock-operator-face)
     (boolean_negation "!" @operator)
     (integer_negation "-" @operator))

   :feature 'property
   :language 'gleam
   '((label) @font-lock-property-name-face
     (tuple_access
      index: (integer) @font-lock-property-name-face))

   :feature 'annotation
   :language 'gleam
   :override t
   '((attribute
      "@" @font-lock-preprocessor-face
      name: (identifier) @font-lock-preprocessor-face))

   :feature 'documentation
   :language 'gleam
   '((module_comment) @font-lock-doc-face
     (statement_comment) @font-lock-doc-face)

   :feature 'module
   :language 'gleam
   :override t
   '((module) @gleam-ts-module-face
     (import alias: (identifier) @gleam-ts-module-face)
     (remote_type_identifier
      module: (identifier) @gleam-ts-module-face)
     (remote_constructor_name
      module: (identifier) @gleam-ts-module-face)
     ;; Unfortunately #is-not? local doesn't work here
     ;; ((field_access
     ;;   record: (identifier) @gleam-ts-module-face)
     ;;  (#is-not? local))
     )

   :feature 'builtin
   :language 'gleam
   :override t
   '((bit_string_segment_option) @font-lock-builtin-face)

   :feature 'bracket
   :language 'gleam
   '([
      "("
      ")"
      "["
      "]"
      "{"
      "}"
      "<<"
      ">>"
      ] @font-lock-bracket-face)

   :feature 'delimiter
   :language 'gleam
   '([
      "."
      ","
      ;; Controversial -- maybe some are operators?
      ":"
      "#"
      "="
      "->"
      ".."
      "-"
      "<-"
      ] @font-lock-delimiter-face)))

(defvar gleam-ts--indent-rules
  (let ((offset 'gleam-ts-indent-offset))
    `((gleam
       ((node-is "^source_file$") column-0 0)
       ((node-is "^import$") parent-bol 0)
       ;; We should indent any child node of unqualified_imports UNLESS it's `}'
       ((match "^}$" "^unqualified_imports$") parent-bol 0)
       ((parent-is "^unqualified_imports$") parent-bol ,offset)
       ((node-is "^constant$") parent-bol 0)
       ((parent-is "^constant$") parent-bol ,offset)
       ((parent-is "^constant_tuple_type$") parent-bol ,offset)
       ((parent-is "^type_arguments$") parent-bol ,offset)
       ((node-is "^external_type$") parent-bol 0)
       ((parent-is "^external_type$") parent-bol ,offset)
       ((node-is "^external_function$") parent-bol 0)
       ((parent-is "^external_function$") parent-bol ,offset)
       ((node-is "^type_definition$") parent-bol 0)
       ((node-is "^type_alias$") parent-bol 0)
       ((node-is "^attribute$") parent-bol 0)
       ((node-is "^attribute_value$") parent-bol ,offset)
       ((node-is "^module-comment$") parent-bol 0)
       ((node-is "^statement_comment$") parent-bol 0)
       ((node-is "^comment$") parent-bol 0)
       ((node-is "^}$") parent-bol 0)
       ((node-is "^]$") parent-bol 0)
       ((node-is "^)$") parent-bol 0)
       ((node-is "^>>$") parent-bol 0)
       ((node-is "^function$") parent-bol 0)
       ((parent-is "^function$") parent-bol ,offset)
       ((parent-is "^anonymous_function$") parent-bol ,offset)
       ((parent-is "^function_body$") parent-bol 0)
       ((parent-is "^function_parameters$") parent-bol ,offset)
       ((parent-is "^function_parameter_types$") parent-bol ,offset)
       ((parent-is "^arguments$") parent-bol ,offset)
       ((parent-is "^case$") parent-bol ,offset)
       ((node-is "^case_clause$") gleam-ts--grand-parent-bol ,offset)
       ((parent-is "^case_clause$") parent-bol ,offset)
       ((parent-is "^record_pattern_arguments$") parent-bol ,offset)
       ((node-is "^|>$") parent-bol gleam-ts--pipe-indent-offset)
       ((parent-is "^binary_expression$") parent-bol ,offset)
       ((parent-is "^todo$") parent-bol ,offset)
       ((parent-is "^panic$") parent-bol ,offset)
       ((parent-is "^tuple$") parent-bol ,offset)
       ((parent-is "^list$") parent-bol ,offset)
       ((parent-is "^let$") parent-bol ,offset)
       ((parent-is "^let_assert$") parent-bol ,offset)
       ((parent-is "^bit_string$") parent-bol ,offset)))))

(defun gleam-ts--grand-parent-bol (_n parent &rest _)
  "Return the beginning of line for the PARENT's parent's parent."
  (save-excursion
    (goto-char (treesit-node-start (treesit-node-parent parent)))
    (back-to-indentation)
    (point)))

(defun gleam-ts--pipe-indent-offset (_n parent &rest _)
  "Return the indentation offset for the given pipeline NODE and its PARENT.

If the pipeline's initial expression is in a multi-element list or a
multi-argument function call, the indentation increases by one level;
otherwise, it aligns with the initial expression."
  (let ((top-level
         (treesit-node-parent
          (treesit-parent-while
           parent
           (lambda (node) (equal (treesit-node-type node) "binary_expression"))))))
    (cond
     ;; Indent if pipeline is within multiple function arguments.
     ((and
       (equal (treesit-node-type top-level) "argument")
       (> (treesit-node-child-count (treesit-node-parent top-level) "argument") 1))
      gleam-ts-indent-offset)

     ;; Indent if pipeline is in a list with multiple elements.
     ((and
       (equal (treesit-node-type top-level) "list")
       (> (treesit-node-child-count top-level "binary_expression") 1))
      gleam-ts-indent-offset)

     ;; Otherwise, align with the initial expression.
     (t 0))))


;;; Public functions

(defun gleam-ts-install-grammar ()
  "Install the Gleam tree-sitter grammar."
  (interactive)
  (if (and (treesit-available-p) (boundp 'treesit-language-source-alist))
      (let ((treesit-language-source-alist
             (cons
              '(gleam . ("https://github.com/gleam-lang/tree-sitter-gleam"))
              treesit-language-source-alist)))
        (treesit-install-language-grammar 'gleam))
    (display-warning 'treesit "Emacs' treesit package does not appear to be available")))

(defun gleam-ts-format ()
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
    (display-warning 'gleam-ts "`gleam' executable not found!")))


;;; Private functions
(defun gleam-ts--public (node)
  "Determine whether NODE is public."
  (treesit-filter-child
   node
   (lambda (child) (equal (treesit-node-type child) "visibility_modifier"))))

(defun gleam-ts--private (node)
  "Determine whether NODE is private."
  (not (gleam-ts--public node)))

(defun gleam-ts--opaque (node)
  "Determine whether NODE is opaque."
  (treesit-filter-child
   node
   (lambda (child) (equal (treesit-node-type child) "opacity_modifier"))))

(defun gleam-ts--transparent (node)
  "Determine whether NODE is /not/ opaque (i.e. transparent)."
  (not (gleam-ts--opaque node)))

(defun gleam-ts--external-fun (node)
  "Determine whether function NODE is external."
  (not (treesit-node-child-by-field-name node "body")))

(defun gleam-ts--internal-fun (node)
  "Determine whether function NODE is internal to Gleam."
  (not (gleam-ts--external-fun node)))

(defun gleam-ts--constant-name (node)
  "Retrieve the name from a constant NODE."
  (treesit-node-text (treesit-node-child-by-field-name node "name")))

(defun gleam-ts--function-name (node)
  "Retrieve the name from a function NODE."
  (treesit-node-text (treesit-node-child-by-field-name node "name")))

(defun gleam-ts--type-name (node)
  "Retrieve the name from a type NODE."
  (treesit-node-text
   (car
    (treesit-filter-child
     node
     (lambda (child) (equal (treesit-node-type child) "type_name"))))))


;;; Major mode definition

(define-derived-mode gleam-ts-mode prog-mode "Gleam"
  "Major mode for editing Gleam.

\\<gleam-ts-mode-map>"
  :group 'gleam-ts

  (cond
   ((treesit-ready-p 'gleam)
    (treesit-parser-create 'gleam)

    (setq-local treesit-font-lock-settings gleam-ts--font-lock-settings)
    (setq-local treesit-font-lock-feature-list
                '((comment string number function-name variable-name constructor type-name)
                  (constant-name keyword operator property)
                  (annotation documentation module builtin bracket delimiter)))

    (setq-local treesit-simple-indent-rules gleam-ts--indent-rules)

    (setq-local treesit-simple-imenu-settings
                '(("Public Functions"           "^function$"       (lambda (fun) (and (gleam-ts--public fun) (gleam-ts--internal-fun fun))) gleam-ts--function-name)
                  ("Public Types"               "^type_definition" (lambda (type) (and (gleam-ts--public type) (gleam-ts--transparent type))) gleam-ts--type-name)
                  ("Public Type Alias"          "^type_alias"      gleam-ts--public  gleam-ts--type-name)
                  ("Public Opaque Types"        "^type_definition" (lambda (type) (and (gleam-ts--public type) (gleam-ts--opaque type))) gleam-ts--type-name)
                  ("Public Constants"           "^constant$"       gleam-ts--public  gleam-ts--constant-name)
                  ("Public External Functions"  "^function$"       (lambda (fun) (and (gleam-ts--public fun) (gleam-ts--external-fun fun))) gleam-ts--function-name)
                  ("Public External Types"      "^external_type"   gleam-ts--public  gleam-ts--type-name)
                  ("Private Functions"          "^function$"       gleam-ts--private gleam-ts--function-name)
                  ("Private Types"              "^type_definition" gleam-ts--private gleam-ts--type-name)
                  ("Private Type Alias"         "^type_alias"      gleam-ts--private gleam-ts--type-name)
                  ("Private Constants"          "^constant$"       gleam-ts--private gleam-ts--constant-name)
                  ("Private External Functions" "^todo"            (lambda (fun) (and (gleam-ts--private fun) (gleam-ts--external-fun fun))) gleam-ts--function-name)
                  ("Private External Types"     "^external_type"   gleam-ts--private gleam-ts--type-name)))

    (setq-local treesit-defun-type-regexp (rx bol (or "type_definition"
                                                      "type_alias"
                                                      "function")
                                              eol))

    (setq-local comment-start "// ")
    (setq-local comment-start-skip (rx "/" (+ "/") (* (syntax whitespace))))
    (setq-local comment-end "")
    (setq-local comment-end-skip
                (rx (* (syntax whitespace))
                    (group (or (syntax comment-end) "\n"))))

    (treesit-major-mode-setup))
   (t
    (message "Cannot load tree-sitter-gleam.  Try running `gleam-ts-install-grammar' and report a bug if the issue reoccurs."))))

(provide 'gleam-ts-mode)
;;; gleam-ts-mode.el ends here
