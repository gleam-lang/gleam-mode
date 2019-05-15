;;; gleam-mode.el --- A major emacs mode for editing Gleam source code -*-lexical-binding: t-*-

(eval-when-compile (require 'rx)
                   (require 'compile)
                   (require 'url-vars))

(defvar electric-pair-inhibit-predicate)
(defvar electric-indent-chars)

(defvar gleam-buffer-project)
(make-variable-buffer-local 'gleam-buffer-project)

;; for GNU Emacs < 24.3
(eval-when-compile
  (unless (fboundp 'setq-local)
    (defmacro setq-local (var val)
      "Set variable VAR to value VAL in current buffer."
      (list 'set (list 'make-local-variable (list 'quote var)) val))))

(defconst gleam-re-ident "[[:word:][:multibyte:]_][[:word:][:multibyte:]_[:digit:]]*")
(defconst gleam-re-lc-ident "[[:lower:][:multibyte:]_][[:word:][:multibyte:]_[:digit:]]*")
(defconst gleam-re-uc-ident "[[:upper:]][[:word:][:multibyte:]_[:digit:]]*")
(defconst gleam-re-vis "pub")
(defconst gleam-re-unsafe "unsafe")
(defconst gleam-re-extern "extern")
(defconst gleam-re-generic
  (concat "<[[:space:]]*'" gleam-re-ident "[[:space:]]*>"))
(defconst gleam-re-union
  (rx-to-string
   `(seq
    (or space line-start)
    (group symbol-start "union" symbol-end)
    (+ space) (regexp ,gleam-re-ident))))

;;; Start of a Gleam item
(defvar gleam-top-item-beg-re
  (concat "\\s-*\\(?:priv\\|pub\\)?\\s-*"
          (regexp-opt
           '("enum" "struct" "union" "type" "mod" "use" "fn" "static" "impl"
             "extern" "trait"))
	  "\\_>"))

(defun gleam-looking-back-str (str)
  "Return non-nil if there's a match on the text before point and STR.
Like `looking-back' but for fixed strings rather than regexps (so that it's not so slow)."
  (let ((len (length str)))
    (and (> (point) len)
         (equal str (buffer-substring-no-properties (- (point) len) (point))))))

(defun gleam-looking-back-symbols (SYMS)
  "Return non-nil if the point is just after a complete symbol that is a member of the list of strings SYMS."
  (save-excursion
    (let* ((pt-orig (point))
           (beg-of-symbol (progn (forward-thing 'symbol -1) (point)))
           (end-of-symbol (progn (forward-thing 'symbol 1) (point))))
      (and
       (= end-of-symbol pt-orig)
       (member (buffer-substring-no-properties beg-of-symbol pt-orig) SYMS)))))

(defun gleam-looking-back-ident ()
  "Non-nil if we are looking backwards at a valid gleam identifier."
  (let ((beg-of-symbol (save-excursion (forward-thing 'symbol -1) (point))))
    (looking-back gleam-re-ident beg-of-symbol)))

(defun gleam-looking-back-macro ()
  "Non-nil if looking back at an ident followed by a !"
  (if (> (- (point) (point-min)) 1)
      (save-excursion (backward-char) (and (= ?! (char-after)) (gleam-looking-back-ident)))))

;; Syntax definitions and helpers
(defvar gleam-mode-syntax-table
  (let ((table (make-syntax-table)))

    ;; Operators
    (dolist (i '(?+ ?- ?* ?/ ?% ?& ?| ?^ ?! ?< ?> ?~ ?@))
      (modify-syntax-entry i "." table))

    ;; Strings
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\\ "\\" table)

    ;; Angle brackets.  We suppress this with syntactic propertization
    ;; when needed
    (modify-syntax-entry ?< "(>" table)
    (modify-syntax-entry ?> ")<" table)

    ;; Comments
    (modify-syntax-entry ?/  ". 124b" table)
    (modify-syntax-entry ?*  ". 23n"  table)
    (modify-syntax-entry ?\n "> b"    table)
    (modify-syntax-entry ?\^m "> b"   table)

    table))

(defgroup gleam-mode nil
  "Support for Gleam code."
  :link '(url-link "https://www.gleam-lang.org/")
  :group 'languages)

(defcustom gleam-indent-offset 4
  "Indent Gleam code by this number of spaces."
  :type 'integer
  :group 'gleam-mode
  :safe #'integerp)

(defcustom gleam-indent-method-chain nil
  "Indent Gleam method chains, aligned by the `.' operators."
  :type 'boolean
  :group 'gleam-mode
  :safe #'booleanp)

(defcustom gleam-indent-where-clause nil
  "Indent lines starting with the `where' keyword following a function or trait.
When nil, `where' will be aligned with `fn' or `trait'."
  :type 'boolean
  :group 'gleam-mode
  :safe #'booleanp)

(defcustom gleam-indent-return-type-to-arguments t
  "Indent a line starting with the `->' (RArrow) following a function, aligning
to the function arguments.  When nil, `->' will be indented one level."
  :type 'boolean
  :group 'gleam-mode
  :safe #'booleanp)

(defcustom gleam-playpen-url-format "https://play.gleam-lang.org/?code=%s"
  "Format string to use when submitting code to the playpen."
  :type 'string
  :group 'gleam-mode)
(defcustom gleam-shortener-url-format "https://is.gd/create.php?format=simple&url=%s"
  "Format string to use for creating the shortened link of a playpen submission."
  :type 'string
  :group 'gleam-mode)

(defcustom gleam-match-angle-brackets t
  "Whether to attempt angle bracket matching (`<' and `>') where appropriate."
  :type 'boolean
  :safe #'booleanp
  :group 'gleam-mode)

(defcustom gleam-format-on-save nil
  "Format future gleam buffers before saving using gleamfmt."
  :type 'boolean
  :safe #'booleanp
  :group 'gleam-mode)

(defcustom gleam-gleamfmt-bin "gleamfmt"
  "Path to gleamfmt executable."
  :type 'string
  :group 'gleam-mode)

(defcustom gleam-cargo-bin "cargo"
  "Path to cargo executable."
  :type 'string
  :group 'gleam-mode)

(defcustom gleam-always-locate-project-on-open nil
  "Whether to run `cargo locate-project' every time `gleam-mode' is activated."
  :type 'boolean
  :group 'gleam-mode)

(defface gleam-unsafe-face
  '((t :inherit font-lock-warning-face))
  "Face for the `unsafe' keyword."
  :group 'gleam-mode)

(defface gleam-question-mark-face
  '((t :weight bold :inherit font-lock-builtin-face))
  "Face for the question mark operator."
  :group 'gleam-mode)

(defface gleam-builtin-formatting-macro-face
  '((t :inherit font-lock-builtin-face))
  "Face for builtin formatting macros (print! &c.)."
  :group 'gleam-mode)

(defface gleam-string-interpolation-face
  '((t :slant italic :inherit font-lock-string-face))
  "Face for interpolating braces in builtin formatting macro strings."
  :group 'gleam-mode)

(defun gleam-paren-level () (nth 0 (syntax-ppss)))
(defun gleam-in-str () (nth 3 (syntax-ppss)))
(defun gleam-in-str-or-cmnt () (nth 8 (syntax-ppss)))
(defun gleam-rewind-past-str-cmnt () (goto-char (nth 8 (syntax-ppss))))

(defun gleam-rewind-irrelevant ()
  (let ((continue t))
    (while continue
      (let ((starting (point)))
        (skip-chars-backward "[:space:]\n")
        (when (gleam-looking-back-str "*/")
          (backward-char))
        (when (gleam-in-str-or-cmnt)
          (gleam-rewind-past-str-cmnt))
        ;; Rewind until the point no longer moves
        (setq continue (/= starting (point)))))))


(defun gleam-in-macro ()
  (save-excursion
    (when (> (gleam-paren-level) 0)
      (backward-up-list)
      (gleam-rewind-irrelevant)
      (or (gleam-looking-back-macro)
          (and (gleam-looking-back-ident) (save-excursion (backward-sexp) (gleam-rewind-irrelevant) (gleam-looking-back-str "macro_rules!")))
          (gleam-in-macro))
      )))

(defun gleam-looking-at-where ()
  "Return T when looking at the \"where\" keyword."
  (and (looking-at-p "\\bwhere\\b")
       (not (gleam-in-str-or-cmnt))))

(defun gleam-rewind-to-where (&optional limit)
  "Rewind the point to the closest occurrence of the \"where\" keyword.
Return T iff a where-clause was found.  Does not rewind past
LIMIT when passed, otherwise only stops at the beginning of the
buffer."
  (when (re-search-backward "\\bwhere\\b" limit t)
    (if (gleam-in-str-or-cmnt)
        (gleam-rewind-to-where limit)
      t)))

(defun gleam-align-to-expr-after-brace ()
  (save-excursion
    (forward-char)
    ;; We don't want to indent out to the open bracket if the
    ;; open bracket ends the line
    (when (not (looking-at "[[:blank:]]*\\(?://.*\\)?$"))
      (when (looking-at "[[:space:]]")
    (forward-word 1)
    (backward-word 1))
      (current-column))))

(defun gleam-rewind-to-beginning-of-current-level-expr ()
  (let ((current-level (gleam-paren-level)))
    (back-to-indentation)
    (when (looking-at "->")
      (gleam-rewind-irrelevant)
      (back-to-indentation))
    (while (> (gleam-paren-level) current-level)
      (backward-up-list)
      (back-to-indentation))
    ;; When we're in the where clause, skip over it.  First find out the start
    ;; of the function and its paren level.
    (let ((function-start nil) (function-level nil))
      (save-excursion
        (gleam-beginning-of-defun)
        (back-to-indentation)
        ;; Avoid using multiple-value-bind
        (setq function-start (point)
              function-level (gleam-paren-level)))
      ;; On a where clause
      (when (or (gleam-looking-at-where)
                ;; or in one of the following lines, e.g.
                ;; where A: Eq
                ;;       B: Hash <- on this line
                (and (save-excursion
                       (gleam-rewind-to-where function-start))
                     (= current-level function-level)))
        (goto-char function-start)))))

(defun gleam-align-to-method-chain ()
  (save-excursion
    ;; for method-chain alignment to apply, we must be looking at
    ;; another method call or field access or something like
    ;; that. This avoids rather "eager" jumps in situations like:
    ;;
    ;; {
    ;;     something.foo()
    ;; <indent>
    ;;
    ;; Without this check, we would wind up with the cursor under the
    ;; `.`. In an older version, I had the inverse of the current
    ;; check, where we checked for situations that should NOT indent,
    ;; vs checking for the one situation where we SHOULD. It should be
    ;; clear that this is more robust, but also I find it mildly less
    ;; annoying to have to press tab again to align to a method chain
    ;; than to have an over-eager indent in all other cases which must
    ;; be undone via tab.

    (when (looking-at (concat "\s*\." gleam-re-ident))
      (forward-line -1)
      (end-of-line)
      ;; Keep going up (looking for a line that could contain a method chain)
      ;; while we're in a comment or on a blank line. Stop when the paren
      ;; level changes.
      (let ((level (gleam-paren-level)))
        (while (and (or (gleam-in-str-or-cmnt)
                        ;; Only whitespace (or nothing) from the beginning to
                        ;; the end of the line.
                        (looking-back "^\s*" (point-at-bol)))
                    (= (gleam-paren-level) level))
          (forward-line -1)
          (end-of-line)))

      (let
          ;; skip-dot-identifier is used to position the point at the
          ;; `.` when looking at something like
          ;;
          ;;      foo.bar
          ;;         ^   ^
          ;;         |   |
          ;;         |  position of point
          ;;       returned offset
          ;;
          ((skip-dot-identifier
            (lambda ()
              (when (and (gleam-looking-back-ident) (save-excursion (forward-thing 'symbol -1) (= ?. (char-before))))
                (forward-thing 'symbol -1)
                (backward-char)
                (- (current-column) gleam-indent-offset)))))
        (cond
         ;; foo.bar(...)
         ((looking-back "[)?]" (1- (point)))
          (backward-list 1)
          (funcall skip-dot-identifier))

         ;; foo.bar
         (t (funcall skip-dot-identifier)))))))

(defun gleam-mode-indent-line ()
  (interactive)
  (let ((indent
         (save-excursion
           (back-to-indentation)
           ;; Point is now at beginning of current line
           (let* ((level (gleam-paren-level))
                  (baseline
                   ;; Our "baseline" is one level out from the indentation of the expression
                   ;; containing the innermost enclosing opening bracket.  That
                   ;; way if we are within a block that has a different
                   ;; indentation than this mode would give it, we still indent
                   ;; the inside of it correctly relative to the outside.
                   (if (= 0 level)
                       0
                     (or
                      (when gleam-indent-method-chain
                        (gleam-align-to-method-chain))
                      (save-excursion
                        (gleam-rewind-irrelevant)
                        (backward-up-list)
                        (gleam-rewind-to-beginning-of-current-level-expr)
                        (+ (current-column) gleam-indent-offset))))))
             (cond
              ;; Indent inside a non-raw string only if the previous line
              ;; ends with a backslash that is inside the same string
              ((nth 3 (syntax-ppss))
               (let*
                   ((string-begin-pos (nth 8 (syntax-ppss)))
                    (end-of-prev-line-pos (when (> (line-number-at-pos) 1)
                                            (save-excursion
                                              (forward-line -1)
                                              (end-of-line)
                                              (point)))))
                 (when
                     (and
                      ;; If the string begins with an "r" it's a raw string and
                      ;; we should not change the indentation
                      (/= ?r (char-after string-begin-pos))

                      ;; If we're on the first line this will be nil and the
                      ;; rest does not apply
                      end-of-prev-line-pos

                      ;; The end of the previous line needs to be inside the
                      ;; current string...
                      (> end-of-prev-line-pos string-begin-pos)

                      ;; ...and end with a backslash
                      (= ?\\ (char-before end-of-prev-line-pos)))

                   ;; Indent to the same level as the previous line, or the
                   ;; start of the string if the previous line starts the string
                   (if (= (line-number-at-pos end-of-prev-line-pos) (line-number-at-pos string-begin-pos))
                       ;; The previous line is the start of the string.
                       ;; If the backslash is the only character after the
                       ;; string beginning, indent to the next indent
                       ;; level.  Otherwise align with the start of the string.
                       (if (> (- end-of-prev-line-pos string-begin-pos) 2)
                           (save-excursion
                             (goto-char (+ 1 string-begin-pos))
                             (current-column))
                         baseline)

                     ;; The previous line is not the start of the string, so
                     ;; match its indentation.
                     (save-excursion
                       (goto-char end-of-prev-line-pos)
                       (back-to-indentation)
                       (current-column))))))

              ;; A function return type is indented to the corresponding
	      ;; function arguments, if -to-arguments is selected.
              ((and gleam-indent-return-type-to-arguments
		    (looking-at "->"))
               (save-excursion
                 (backward-list)
                 (or (gleam-align-to-expr-after-brace)
                     (+ baseline gleam-indent-offset))))

              ;; A closing brace is 1 level unindented
              ((looking-at "[]})]") (- baseline gleam-indent-offset))

              ;; Doc comments in /** style with leading * indent to line up the *s
              ((and (nth 4 (syntax-ppss)) (looking-at "*"))
               (+ 1 baseline))

              ;; When the user chose not to indent the start of the where
              ;; clause, put it on the baseline.
              ((and (not gleam-indent-where-clause)
                    (gleam-looking-at-where))
               baseline)

              ;; If we're in any other token-tree / sexp, then:
              (t
               (or
                ;; If we are inside a pair of braces, with something after the
                ;; open brace on the same line and ending with a comma, treat
                ;; it as fields and align them.
                (when (> level 0)
                  (save-excursion
                    (gleam-rewind-irrelevant)
                    (backward-up-list)
                    ;; Point is now at the beginning of the containing set of braces
                    (gleam-align-to-expr-after-brace)))

                ;; When where-clauses are spread over multiple lines, clauses
                ;; should be aligned on the type parameters.  In this case we
                ;; take care of the second and following clauses (the ones
                ;; that don't start with "where ")
                (save-excursion
                  ;; Find the start of the function, we'll use this to limit
                  ;; our search for "where ".
                  (let ((function-start nil) (function-level nil))
                    (save-excursion
                      ;; If we're already at the start of a function,
                      ;; don't go back any farther.  We can easily do
                      ;; this by moving to the end of the line first.
                      (end-of-line)
                      (gleam-beginning-of-defun)
                      (back-to-indentation)
                      ;; Avoid using multiple-value-bind
                      (setq function-start (point)
                            function-level (gleam-paren-level)))
                    ;; When we're not on a line starting with "where ", but
                    ;; still on a where-clause line, go to "where "
                    (when (and
                           (not (gleam-looking-at-where))
                           ;; We're looking at something like "F: ..."
                           (looking-at (concat gleam-re-ident ":"))
                           ;; There is a "where " somewhere after the
                           ;; start of the function.
                           (gleam-rewind-to-where function-start)
                           ;; Make sure we're not inside the function
                           ;; already (e.g. initializing a struct) by
                           ;; checking we are the same level.
                           (= function-level level))
                      ;; skip over "where"
                      (forward-char 5)
                      ;; Unless "where" is at the end of the line
                      (if (eolp)
                          ;; in this case the type parameters bounds are just
                          ;; indented once
                          (+ baseline gleam-indent-offset)
                        ;; otherwise, skip over whitespace,
                        (skip-chars-forward "[:space:]")
                        ;; get the column of the type parameter and use that
                        ;; as indentation offset
                        (current-column)))))

                (progn
                  (back-to-indentation)
                  ;; Point is now at the beginning of the current line
                  (if (or
                       ;; If this line begins with "else" or "{", stay on the
                       ;; baseline as well (we are continuing an expression,
                       ;; but the "else" or "{" should align with the beginning
                       ;; of the expression it's in.)
                       ;; Or, if this line starts a comment, stay on the
                       ;; baseline as well.
                       (looking-at "\\<else\\>\\|{\\|/[/*]")

                       ;; If this is the start of a top-level item,
                       ;; stay on the baseline.
                       (looking-at gleam-top-item-beg-re)

                       (save-excursion
                         (gleam-rewind-irrelevant)
                         ;; Point is now at the end of the previous line
                         (or
                          ;; If we are at the start of the buffer, no
                          ;; indentation is needed, so stay at baseline...
                          (= (point) 1)
                          ;; ..or if the previous line ends with any of these:
                          ;;     { ? : ( , ; [ }
                          ;; then we are at the beginning of an expression, so stay on the baseline...
                          (looking-back "[(,:;[{}]\\|[^|]|" (- (point) 2))
                          ;; or if the previous line is the end of an attribute, stay at the baseline...
                          (progn (gleam-rewind-to-beginning-of-current-level-expr) (looking-at "#")))))
                      baseline

                    ;; Otherwise, we are continuing the same expression from the previous line,
                    ;; so add one additional indent level
                    (+ baseline gleam-indent-offset))))))))))

    (when indent
      ;; If we're at the beginning of the line (before or at the current
      ;; indentation), jump with the indentation change.  Otherwise, save the
      ;; excursion so that adding the indentations will leave us at the
      ;; equivalent position within the line to where we were before.
      (if (<= (current-column) (current-indentation))
          (indent-line-to indent)
        (save-excursion (indent-line-to indent))))))


;; Font-locking definitions and helpers
(defconst gleam-mode-keywords
  '("as" "async"
    "box" "break"
    "const" "continue" "crate"
    "do" "dyn"
    "else" "enum" "extern"
    "false" "fn" "for"
    "if" "impl" "in"
    "let" "loop"
    "match" "mod" "move" "mut"
    "priv" "pub"
    "ref" "return"
    "self" "static" "struct" "super"
    "true" "trait" "type" "try"
    "use"
    "virtual"
    "where" "while"
    "yield"))

(defconst gleam-special-types
  '("u8" "i8"
    "u16" "i16"
    "u32" "i32"
    "u64" "i64"
    "u128" "i128"

    "f32" "f64"
    "isize" "usize"
    "bool"
    "str" "char"))

(defconst gleam-re-type-or-constructor
  (rx symbol-start
      (group upper (0+ (any word nonascii digit "_")))
      symbol-end))

(defconst gleam-re-pre-expression-operators "[-=!%&*/:<>[{(|.^;}]")
(defun gleam-re-word (inner) (concat "\\<" inner "\\>"))
(defun gleam-re-grab (inner) (concat "\\(" inner "\\)"))
(defun gleam-re-shy (inner) (concat "\\(?:" inner "\\)"))
(defun gleam-re-item-def (itype)
  (concat (gleam-re-word itype)
	  (gleam-re-shy gleam-re-generic) "?"
	  "[[:space:]]+" (gleam-re-grab gleam-re-ident)))
(defun gleam-re-item-def-imenu (itype)
  (concat "^[[:space:]]*"
          (gleam-re-shy (concat (gleam-re-word gleam-re-vis) "[[:space:]]+")) "?"
          (gleam-re-shy (concat (gleam-re-word "default") "[[:space:]]+")) "?"
          (gleam-re-shy (concat (gleam-re-word gleam-re-unsafe) "[[:space:]]+")) "?"
          (gleam-re-shy (concat (gleam-re-word gleam-re-extern) "[[:space:]]+"
                               (gleam-re-shy "\"[^\"]+\"[[:space:]]+") "?")) "?"
          (gleam-re-item-def itype)))

(defconst gleam-re-special-types (regexp-opt gleam-special-types 'symbols))


(defun gleam-path-font-lock-matcher (re-ident)
  "Match occurrences of RE-IDENT followed by a double-colon.
Examples include to match names like \"foo::\" or \"Foo::\".
Does not match type annotations of the form \"foo::<\"."
  `(lambda (limit)
     (catch 'gleam-path-font-lock-matcher
       (while t
         (let* ((symbol-then-colons (rx-to-string '(seq (group (regexp ,re-ident)) "::")))
                (match (re-search-forward symbol-then-colons limit t)))
           (cond
            ;; If we didn't find a match, there are no more occurrences
            ;; of foo::, so return.
            ((null match) (throw 'gleam-path-font-lock-matcher nil))
            ;; If this isn't a type annotation foo::<, we've found a
            ;; match, so a return it!
            ((not (looking-at (rx (0+ space) "<")))
	     (throw 'gleam-path-font-lock-matcher match))))))))

(defun gleam-next-string-interpolation (limit)
  "Search forward from point for the next Gleam interpolation marker before LIMIT.
Set point to the end of the occurrence found, and return match beginning
and end."
  (catch 'match
    (save-match-data
      (save-excursion
        (while (search-forward "{" limit t)
          (if (eql (char-after (point)) ?{)
              (forward-char)
            (let ((start (match-beginning 0)))
              ;; According to fmt_macros::Parser::next, an opening brace
              ;; must be followed by an optional argument and/or format
              ;; specifier, then a closing brace. A single closing brace
              ;; without a corresponding unescaped opening brace is an
              ;; error. We don't need to do anything special with
              ;; arguments, specifiers, or errors, so we only search for
              ;; the single closing brace.
              (when (search-forward "}" limit t)
                (throw 'match (list start (point)))))))))))

(defun gleam-string-interpolation-matcher (limit)
  "Match the next Gleam interpolation marker before LIMIT; set match data if found.
Returns nil if the point is not within a Gleam string."
  (when (gleam-in-str)
    (let ((match (gleam-next-string-interpolation limit)))
      (when match
        (set-match-data match)
        (goto-char (cadr match))
        match))))

(defvar gleam-builtin-formatting-macros
  '("eprint"
    "eprintln"
    "format"
    "print"
    "println")
  "List of builtin Gleam macros for string formatting used by `gleam-mode-font-lock-keywords' (`write!' is handled separately).")

(defvar gleam-formatting-macro-opening-re
  "[[:space:]\n]*[({[][[:space:]\n]*"
  "Regular expression to match the opening delimiter of a Gleam formatting macro.")

(defvar gleam-start-of-string-re
  "\\(?:r#*\\)?\""
  "Regular expression to match the start of a Gleam raw string.")

(defvar gleam-mode-font-lock-keywords
  (append
   `(
     ;; Keywords proper
     (,(regexp-opt gleam-mode-keywords 'symbols) . font-lock-keyword-face)

     ;; Contextual keywords
     ("\\_<\\(default\\)[[:space:]]+fn\\_>" 1 font-lock-keyword-face)
     (,gleam-re-union 1 font-lock-keyword-face)

     ;; Special types
     (,(regexp-opt gleam-special-types 'symbols) . font-lock-type-face)

     ;; The unsafe keyword
     ("\\_<unsafe\\_>" . 'gleam-unsafe-face)

     ;; Attributes like `#[bar(baz)]` or `#![bar(baz)]` or `#[bar = "baz"]`
     (,(gleam-re-grab (concat "#\\!?\\[" gleam-re-ident "[^]]*\\]"))
      1 font-lock-preprocessor-face keep)

     ;; Builtin formatting macros
     (,(concat (gleam-re-grab (concat (regexp-opt gleam-builtin-formatting-macros) "!")) (concat gleam-formatting-macro-opening-re "\\(?:" gleam-start-of-string-re) "\\)?")
      (1 'gleam-builtin-formatting-macro-face)
      (gleam-string-interpolation-matcher
       (gleam-end-of-string)
       nil
       (0 'gleam-string-interpolation-face t nil)))

     ;; write! macro
     (,(concat (gleam-re-grab "write\\(ln\\)?!") (concat gleam-formatting-macro-opening-re "[[:space:]]*[^\"]+,[[:space:]]*" gleam-start-of-string-re))
      (1 'gleam-builtin-formatting-macro-face)
      (gleam-string-interpolation-matcher
       (gleam-end-of-string)
       nil
       (0 'gleam-string-interpolation-face t nil)))

     ;; Syntax extension invocations like `foo!`, highlight including the !
     (,(concat (gleam-re-grab (concat gleam-re-ident "!")) "[({[:space:][]")
      1 font-lock-preprocessor-face)

     ;; Field names like `foo:`, highlight excluding the :
     (,(concat (gleam-re-grab gleam-re-ident) ":[^:]") 1 font-lock-variable-name-face)

     ;; CamelCase Means Type Or Constructor
     (,gleam-re-type-or-constructor 1 font-lock-type-face)

     ;; Type-inferred binding
     (,(concat "\\_<\\(?:let\\s-+ref\\|let\\|ref\\)\\s-+\\(?:mut\\s-+\\)?" (gleam-re-grab gleam-re-ident) "\\_>") 1 font-lock-variable-name-face)

     ;; Type names like `Foo::`, highlight excluding the ::
     (,(gleam-path-font-lock-matcher gleam-re-uc-ident) 1 font-lock-type-face)

     ;; Module names like `foo::`, highlight excluding the ::
     (,(gleam-path-font-lock-matcher gleam-re-lc-ident) 1 font-lock-constant-face)

     ;; Lifetimes like `'foo`
     (,(concat "'" (gleam-re-grab gleam-re-ident) "[^']") 1 font-lock-variable-name-face)

     ;; Question mark operator
     ("\\?" . 'gleam-question-mark-face)
     )

   ;; Ensure we highlight `Foo` in `struct Foo` as a type.
   (mapcar #'(lambda (x)
               (list (gleam-re-item-def (car x))
                     1 (cdr x)))
           '(("enum" . font-lock-type-face)
             ("struct" . font-lock-type-face)
             ("union" . font-lock-type-face)
             ("type" . font-lock-type-face)
             ("mod" . font-lock-constant-face)
             ("use" . font-lock-constant-face)
             ("fn" . font-lock-function-name-face)))))

(defun gleam-syntax-class-before-point ()
  (when (> (point) 1)
    (syntax-class (syntax-after (1- (point))))))

(defun gleam-rewind-qualified-ident ()
  (while (gleam-looking-back-ident)
    (backward-sexp)
    (when (save-excursion (gleam-rewind-irrelevant) (gleam-looking-back-str "::"))
      (gleam-rewind-irrelevant)
      (backward-char 2)
      (gleam-rewind-irrelevant))))

(defun gleam-rewind-type-param-list ()
  (cond
   ((and (gleam-looking-back-str ">") (equal 5 (gleam-syntax-class-before-point)))
    (backward-sexp)
    (gleam-rewind-irrelevant))

   ;; We need to be able to back up past the Fn(args) -> RT form as well.  If
   ;; we're looking back at this, we want to end up just after "Fn".
   ((member (char-before) '(?\] ?\) ))
    (let* ((is-paren (gleam-looking-back-str ")"))
           (dest (save-excursion
                  (backward-sexp)
                  (gleam-rewind-irrelevant)
                  (or
                   (when (gleam-looking-back-str "->")
                     (backward-char 2)
                     (gleam-rewind-irrelevant)
                     (when (gleam-looking-back-str ")")
                       (backward-sexp)
                       (point)))
                   (and is-paren (point))))))
      (when dest
        (goto-char dest))))))

(defun gleam-rewind-to-decl-name ()
  "Return the point at the beginning of the name in a declaration.
I.e. if we are before an ident that is part of a declaration that
can have a where clause, rewind back to just before the name of
the subject of that where clause and return the new point.
Otherwise return nil."

  (let* ((ident-pos (point))
         (newpos (save-excursion
                   (gleam-rewind-irrelevant)
                   (gleam-rewind-type-param-list)
                   (cond
                       ((gleam-looking-back-symbols '("fn" "trait" "enum" "struct" "union" "impl" "type")) ident-pos)

                       ((equal 5 (gleam-syntax-class-before-point))
                        (backward-sexp)
                        (gleam-rewind-to-decl-name))

                       ((looking-back "[:,'+=]" (1- (point)))
                        (backward-char)
                        (gleam-rewind-to-decl-name))

                       ((gleam-looking-back-str "->")
                        (backward-char 2)
                        (gleam-rewind-to-decl-name))

                       ((gleam-looking-back-ident)
                        (gleam-rewind-qualified-ident)
                        (gleam-rewind-to-decl-name))))))
    (when newpos (goto-char newpos))
    newpos))

(defun gleam-is-in-expression-context (token)
  "Return t if what comes right after the point is part of an
expression (as opposed to starting a type) by looking at what
comes before.  Takes a symbol that roughly indicates what is
after the point.

This function is used as part of `gleam-is-lt-char-operator' as
part of angle bracket matching, and is not intended to be used
outside of this context."

  (save-excursion
    (let ((postchar (char-after)))
      (gleam-rewind-irrelevant)

      ;; A type alias or ascription could have a type param list.  Skip backwards past it.
      (when (member token '(ambiguous-operator open-brace))
        (gleam-rewind-type-param-list))

      (cond

       ;; Certain keywords always introduce expressions
       ((gleam-looking-back-symbols '("if" "while" "match" "return" "box" "in")) t)

       ;; "as" introduces a type
       ((gleam-looking-back-symbols '("as")) nil)

       ;; An open angle bracket never introduces expression context WITHIN the angle brackets
       ((and (equal token 'open-brace) (equal postchar ?<)) nil)

       ;; An ident! followed by an open brace is a macro invocation.  Consider
       ;; it to be an expression.
       ((and (equal token 'open-brace) (gleam-looking-back-macro)) t)

       ;; In a brace context a "]" introduces an expression.
       ((and (eq token 'open-brace) (gleam-looking-back-str "]")))

       ;; An identifier is right after an ending paren, bracket, angle bracket
       ;; or curly brace.  It's a type if the last sexp was a type.
       ((and (equal token 'ident) (equal 5 (gleam-syntax-class-before-point)))
        (backward-sexp)
        (gleam-is-in-expression-context 'open-brace))

       ;; If a "for" appears without a ; or { before it, it's part of an
       ;; "impl X for y", so the y is a type.  Otherwise it's
       ;; introducing a loop, so the y is an expression
       ((and (equal token 'ident) (gleam-looking-back-symbols '("for")))
        (backward-sexp)
        (gleam-rewind-irrelevant)
        (looking-back "[{;]" (1- (point))))

       ((gleam-looking-back-ident)
        (gleam-rewind-qualified-ident)
        (gleam-rewind-irrelevant)
        (cond
         ((equal token 'open-brace)
          ;; We now know we have:
          ;;   ident <maybe type params> [{([]
          ;; where [{([] denotes either a {, ( or [.  This character is bound as postchar.
          (cond
           ;; If postchar is a paren or square bracket, then if the brace is a type if the identifier is one
           ((member postchar '(?\( ?\[ )) (gleam-is-in-expression-context 'ident))

           ;; If postchar is a curly brace, the brace can only be a type if
           ;; ident2 is the name of an enum, struct or trait being declared.
           ;; Note that if there is a -> before the ident then the ident would
           ;; be a type but the { is not.
           ((equal ?{ postchar)
            (not (and (gleam-rewind-to-decl-name)
                      (progn
                        (gleam-rewind-irrelevant)
                        (gleam-looking-back-symbols '("enum" "struct" "union" "trait" "type"))))))
           ))

         ((equal token 'ambiguous-operator)
          (cond
           ;; An ampersand after an ident has to be an operator rather than a & at the beginning of a ref type
           ((equal postchar ?&) t)

           ;; A : followed by a type then an = introduces an expression (unless it is part of a where clause of a "type" declaration)
           ((and (equal postchar ?=)
                 (looking-back "[^:]:" (- (point) 2))
                 (not (save-excursion (and (gleam-rewind-to-decl-name) (progn (gleam-rewind-irrelevant) (gleam-looking-back-symbols '("type"))))))))

           ;; "let ident =" introduces an expression--and so does "const" and "mut"
           ((and (equal postchar ?=) (gleam-looking-back-symbols '("let" "const" "mut"))) t)

           ;; As a specific special case, see if this is the = in this situation:
           ;;     enum EnumName<type params> { Ident =
           ;; In this case, this is a c-like enum and despite Ident
           ;; representing a type, what comes after the = is an expression
           ((and
             (> (gleam-paren-level) 0)
             (save-excursion
               (backward-up-list)
               (gleam-rewind-irrelevant)
               (gleam-rewind-type-param-list)
               (and
                (gleam-looking-back-ident)
                (progn
                  (gleam-rewind-qualified-ident)
                  (gleam-rewind-irrelevant)
                  (gleam-looking-back-str "enum")))))
            t)

           ;; Otherwise the ambiguous operator is a type if the identifier is a type
           ((gleam-is-in-expression-context 'ident) t)))

         ((equal token 'colon)
          (cond
           ;; If we see a ident: not inside any braces/parens, we're at top level.
           ;; There are no allowed expressions after colons there, just types.
           ((<= (gleam-paren-level) 0) nil)

           ;; We see ident: inside a list
           ((looking-back "[{,]" (1- (point)))
            (backward-up-list)

            ;; If a : appears whose surrounding paren/brackets/braces are
            ;; anything other than curly braces, it can't be a field
            ;; initializer and must be denoting a type.
            (when (looking-at "{")
              (gleam-rewind-irrelevant)
              (gleam-rewind-type-param-list)
              (when (gleam-looking-back-ident)
                ;; We have a context that looks like this:
                ;;    ident2 <maybe type params> { [maybe paren-balanced code ending in comma] ident1:
                ;; the point is sitting just after ident2, and we trying to
                ;; figure out if the colon introduces an expression or a type.
                ;; The answer is that ident1 is a field name, and what comes
                ;; after the colon is an expression, if ident2 is an
                ;; expression.
                (gleam-rewind-qualified-ident)
                (gleam-is-in-expression-context 'ident))))


           ;; Otherwise, if the ident: appeared with anything other than , or {
           ;; before it, it can't be part of a struct initializer and therefore
           ;; must be denoting a type.
	   (t nil)
           ))
         ))

       ;; An operator-like character after a string is indeed an operator
       ((and (equal token 'ambiguous-operator)
             (member (gleam-syntax-class-before-point) '(5 7 15))) t)

       ;; A colon that has something other than an identifier before it is a
       ;; type ascription
       ((equal token 'colon) nil)

       ;; A :: introduces a type (or module, but not an expression in any case)
       ((gleam-looking-back-str "::") nil)

       ((gleam-looking-back-str ":")
        (backward-char)
        (gleam-is-in-expression-context 'colon))

       ;; A -> introduces a type
       ((gleam-looking-back-str "->") nil)

       ;; If we are up against the beginning of a list, or after a comma inside
       ;; of one, back up out of it and check what the list itself is
       ((or
         (equal 4 (gleam-syntax-class-before-point))
         (gleam-looking-back-str ","))
	(condition-case nil
	    (progn
	      (backward-up-list)
	      (gleam-is-in-expression-context 'open-brace))
	  (scan-error nil)))

       ;; A => introduces an expression
       ((gleam-looking-back-str "=>") t)

       ;; A == introduces an expression
       ((gleam-looking-back-str "==") t)

       ;; These operators can introduce expressions or types
       ((looking-back "[-+=!?&*]" (1- (point)))
        (backward-char)
        (gleam-is-in-expression-context 'ambiguous-operator))

       ;; These operators always introduce expressions.  (Note that if this
       ;; regexp finds a < it must not be an angle bracket, or it'd
       ;; have been caught in the syntax-class check above instead of this.)
       ((looking-back gleam-re-pre-expression-operators (1- (point))) t)
       ))))

(defun gleam-is-lt-char-operator ()
  "Return t if the `<' after the point is the less-than operator.
Otherwise, for instance if it's an opening angle bracket, return nil."

  (let ((case-fold-search nil))
    (save-excursion
      (gleam-rewind-irrelevant)
      ;; We are now just after the character syntactically before the <.
      (cond

       ;; If we are looking back at a < that is not an angle bracket (but not
       ;; two of them) then this is the second < in a bit shift operator
       ((and (gleam-looking-back-str "<")
             (not (equal 4 (gleam-syntax-class-before-point)))
             (not (gleam-looking-back-str "<<"))))

       ;; On the other hand, if we are after a closing paren/brace/bracket it
       ;; can only be an operator, not an angle bracket.  Likewise, if we are
       ;; after a string it's an operator.  (The string case could actually be
       ;; valid in gleam for character literals.)
       ((member (gleam-syntax-class-before-point) '(5 7 15)) t)

       ;; If we are looking back at an operator, we know that we are at
       ;; the beginning of an expression, and thus it has to be an angle
       ;; bracket (starting a "<Type as Trait>::" construct.)
       ((looking-back gleam-re-pre-expression-operators (1- (point))) nil)

       ;; If we are looking back at a keyword, it's an angle bracket
       ;; unless that keyword is "self", "true" or "false"
       ((gleam-looking-back-symbols gleam-mode-keywords)
        (gleam-looking-back-symbols '("self" "true" "false")))

       ((gleam-looking-back-str "?")
	(gleam-is-in-expression-context 'ambiguous-operator))

       ;; If we're looking back at an identifier, this depends on whether
       ;; the identifier is part of an expression or a type
       ((gleam-looking-back-ident)
        (backward-sexp)
        (or
         ;; The special types can't take type param lists, so a < after one is
         ;; always an operator
         (looking-at gleam-re-special-types)

         (gleam-is-in-expression-context 'ident)))

       ;; Otherwise, assume it's an angle bracket
       ))))

(defun gleam-electric-pair-inhibit-predicate-wrap (char)
  "Prevent \"matching\" with a `>' when CHAR is the less-than operator.
This wraps the default defined by `electric-pair-inhibit-predicate'."
  (or
   (when (= ?< char)
     (save-excursion
       (backward-char)
       (gleam-is-lt-char-operator)))
   (funcall (default-value 'electric-pair-inhibit-predicate) char)))

(defun gleam-ordinary-lt-gt-p ()
  "Test whether the `<' or `>' at point is an ordinary operator of some kind.

This returns t if the `<' or `>' is an ordinary operator (like
less-than) or part of one (like `->'); and nil if the character
should be considered a paired angle bracket."
  (cond
   ;; If matching is turned off suppress all of them
   ((not gleam-match-angle-brackets) t)

   ;; We don't take < or > in strings or comments to be angle brackets
   ((gleam-in-str-or-cmnt) t)

   ;; Inside a macro we don't really know the syntax.  Any < or > may be an
   ;; angle bracket or it may not.  But we know that the other braces have
   ;; to balance regardless of the < and >, so if we don't treat any < or >
   ;; as angle brackets it won't mess up any paren balancing.
   ((gleam-in-macro) t)

   ((looking-at "<")
    (gleam-is-lt-char-operator))

   ((looking-at ">")
    (cond
     ;; Don't treat the > in -> or => as an angle bracket
     ((member (char-before (point)) '(?- ?=)) t)

     ;; If we are at top level and not in any list, it can't be a closing
     ;; angle bracket
     ((>= 0 (gleam-paren-level)) t)

     ;; Otherwise, treat the > as a closing angle bracket if it would
     ;; match an opening one
     ((save-excursion
	(backward-up-list)
	(not (looking-at "<"))))))))

(defun gleam-mode-syntactic-face-function (state)
  "Return face which distinguishes doc and normal comments in the given syntax STATE."
  (if (nth 3 state) 'font-lock-string-face
    (save-excursion
      (goto-char (nth 8 state))
      (if (looking-at "/\\([*][*!][^*!]\\|/[/!][^/!]\\)")
          'font-lock-doc-face
        'font-lock-comment-face
    ))))

(eval-and-compile
  (defconst gleam--char-literal-rx
    (rx (seq
	 (group "'")
	 (or
	  (seq
	   "\\"
	   (or
	    (: "u{" (** 1 6 xdigit) "}")
	    (: "x" (= 2 xdigit))
	    (any "'nrt0\"\\")))
	  (not (any "'\\"))
	  )
	 (group "'")))
    "A regular expression matching a character literal."))

(defun gleam--syntax-propertize-raw-string (end)
  "A helper for gleam-syntax-propertize.

If point is already in a raw string, this will apply the
appropriate string syntax to the character up to the end of the
raw string, or to END, whichever comes first."
  (let ((str-start (nth 8 (syntax-ppss))))
    (when str-start
      (when (save-excursion
	      (goto-char str-start)
	      (looking-at "r\\(#*\\)\\(\"\\)"))
	;; In a raw string, so try to find the end.
	(let ((hashes (match-string 1)))
	  ;; Match \ characters at the end of the string to suppress
	  ;; their normal character-quote syntax.
	  (when (re-search-forward (concat "\\(\\\\*\\)\\(\"" hashes "\\)") end t)
	    (put-text-property (match-beginning 1) (match-end 1)
			       'syntax-table (string-to-syntax "_"))
	    (put-text-property (1- (match-end 2)) (match-end 2)
			       'syntax-table (string-to-syntax "|"))
	    (goto-char (match-end 0))))))))

(defun gleam-syntax-propertize (start end)
  "A `syntax-propertize-function' to apply properties from START to END."
  (goto-char start)
  (gleam--syntax-propertize-raw-string end)
  (funcall
   (syntax-propertize-rules
    ;; Character literals.
    (gleam--char-literal-rx (1 "\"") (2 "\""))
    ;; Raw strings.
    ("\\(r\\)#*\""
     (1 (prog1 "|"
	  (goto-char (match-end 0))
	  (gleam--syntax-propertize-raw-string end))))
    ("[<>]"
     (0 (ignore
	 (when (save-match-data
		 (save-excursion
		   (goto-char (match-beginning 0))
		   (gleam-ordinary-lt-gt-p)))
	   (put-text-property (match-beginning 0) (match-end 0)
			      'syntax-table (string-to-syntax "."))
	   (goto-char (match-end 0)))))))
   (point) end))

(defun gleam-fill-prefix-for-comment-start (line-start)
  "Determine what to use for `fill-prefix' based on the text at LINE-START."
  (let ((result
         ;; Replace /* with same number of spaces
         (replace-regexp-in-string
          "\\(?:/\\*+?\\)[!*]?"
          (lambda (s)
            ;; We want the * to line up with the first * of the
            ;; comment start
            (let ((offset (if (eq t
                                  (compare-strings "/*" nil nil
                                                   s
                                                   (- (length s) 2)
                                                   (length s)))
                              1 2)))
              (concat (make-string (- (length s) offset)
                                   ?\x20) "*")))
          line-start)))
    ;; Make sure we've got at least one space at the end
    (if (not (= (aref result (- (length result) 1)) ?\x20))
        (setq result (concat result " ")))
    result))

(defun gleam-in-comment-paragraph (body)
  ;; We might move the point to fill the next comment, but we don't want it
  ;; seeming to jump around on the user
  (save-excursion
    ;; If we're outside of a comment, with only whitespace and then a comment
    ;; in front, jump to the comment and prepare to fill it.
    (when (not (nth 4 (syntax-ppss)))
      (beginning-of-line)
      (when (looking-at (concat "[[:space:]\n]*" comment-start-skip))
        (goto-char (match-end 0))))

    ;; We need this when we're moving the point around and then checking syntax
    ;; while doing paragraph fills, because the cache it uses isn't always
    ;; invalidated during this.
    (syntax-ppss-flush-cache 1)
    ;; If we're at the beginning of a comment paragraph with nothing but
    ;; whitespace til the next line, jump to the next line so that we use the
    ;; existing prefix to figure out what the new prefix should be, rather than
    ;; inferring it from the comment start.
    (let ((next-bol (line-beginning-position 2)))
      (while (save-excursion
               (end-of-line)
               (syntax-ppss-flush-cache 1)
               (and (nth 4 (syntax-ppss))
                    (save-excursion
                      (beginning-of-line)
                      (looking-at paragraph-start))
                    (looking-at "[[:space:]]*$")
                    (nth 4 (syntax-ppss next-bol))))
        (goto-char next-bol)))

    (syntax-ppss-flush-cache 1)
    ;; If we're on the last line of a multiline-style comment that started
    ;; above, back up one line so we don't mistake the * of the */ that ends
    ;; the comment for a prefix.
    (when (save-excursion
            (and (nth 4 (syntax-ppss (line-beginning-position 1)))
                 (looking-at "[[:space:]]*\\*/")))
      (goto-char (line-end-position 0)))
    (funcall body)))

(defun gleam-with-comment-fill-prefix (body)
  (let*
      ((line-string (buffer-substring-no-properties
                     (line-beginning-position) (line-end-position)))
       (line-comment-start
        (when (nth 4 (syntax-ppss))
          (cond
           ;; If we're inside the comment and see a * prefix, use it
           ((string-match "^\\([[:space:]]*\\*+[[:space:]]*\\)"
                          line-string)
            (match-string 1 line-string))
           ;; If we're at the start of a comment, figure out what prefix
           ;; to use for the subsequent lines after it
           ((string-match (concat "[[:space:]]*" comment-start-skip) line-string)
            (gleam-fill-prefix-for-comment-start
             (match-string 0 line-string))))))
       (fill-prefix
        (or line-comment-start
            fill-prefix)))
    (funcall body)))

(defun gleam-find-fill-prefix ()
  (gleam-in-comment-paragraph (lambda () (gleam-with-comment-fill-prefix (lambda () fill-prefix)))))

(defun gleam-fill-paragraph (&rest args)
  "Special wrapping for `fill-paragraph' to handle multi-line comments with a * prefix on each line."
  (gleam-in-comment-paragraph
   (lambda ()
     (gleam-with-comment-fill-prefix
      (lambda ()
        (let
            ((fill-paragraph-function
              (if (not (eq fill-paragraph-function 'gleam-fill-paragraph))
                  fill-paragraph-function))
             (fill-paragraph-handle-comment t))
          (apply 'fill-paragraph args)
          t))))))

(defun gleam-do-auto-fill (&rest args)
  "Special wrapping for `do-auto-fill' to handle multi-line comments with a * prefix on each line."
  (gleam-with-comment-fill-prefix
   (lambda ()
     (apply 'do-auto-fill args)
     t)))

(defun gleam-fill-forward-paragraph (arg)
  ;; This is to work around some funny behavior when a paragraph separator is
  ;; at the very top of the file and there is a fill prefix.
  (let ((fill-prefix nil)) (forward-paragraph arg)))

(defun gleam-comment-indent-new-line (&optional arg)
  (gleam-with-comment-fill-prefix
   (lambda () (comment-indent-new-line arg))))

;;; Imenu support
(defvar gleam-imenu-generic-expression
  (append (mapcar #'(lambda (x)
                      (list (capitalize x) (gleam-re-item-def-imenu x) 1))
                  '("enum" "struct" "union" "type" "mod" "fn" "trait" "impl"))
          `(("Macro" ,(gleam-re-item-def-imenu "macro_rules!") 1)))
  "Value for `imenu-generic-expression' in Gleam mode.

Create a hierarchical index of the item definitions in a Gleam file.

Imenu will show all the enums, structs, etc. in their own subheading.
Use idomenu (imenu with `ido-mode') for best mileage.")

;;; Defun Motions

(defun gleam-beginning-of-defun (&optional arg)
  "Move backward to the beginning of the current defun.

With ARG, move backward multiple defuns.  Negative ARG means
move forward.

This is written mainly to be used as `beginning-of-defun-function' for Gleam.
Don't move to the beginning of the line. `beginning-of-defun',
which calls this, does that afterwards."
  (interactive "p")
  (let* ((arg (or arg 1))
	 (magnitude (abs arg))
	 (sign (if (< arg 0) -1 1)))
    ;; If moving forward, don't find the defun we might currently be
    ;; on.
    (when (< sign 0)
      (end-of-line))
    (catch 'done
      (dotimes (_ magnitude)
	;; Search until we find a match that is not in a string or comment.
	(while (if (re-search-backward (concat "^\\(" gleam-top-item-beg-re "\\)")
				       nil 'move sign)
		   (gleam-in-str-or-cmnt)
		 ;; Did not find it.
		 (throw 'done nil)))))
    t))

(defun gleam-end-of-defun ()
  "Move forward to the next end of defun.

With argument, do it that many times.
Negative argument -N means move back to Nth preceding end of defun.

Assume that this is called after `beginning-of-defun'.  So point is
at the beginning of the defun body.

This is written mainly to be used as `end-of-defun-function' for Gleam."
  (interactive)
  ;; Find the opening brace
  (if (re-search-forward "[{]" nil t)
      (progn
        (goto-char (match-beginning 0))
        ;; Go to the closing brace
        (condition-case nil
            (forward-sexp)
          (scan-error
           ;; The parentheses are unbalanced; instead of being unable to fontify, just jump to the end of the buffer
           (goto-char (point-max)))))
    ;; There is no opening brace, so consider the whole buffer to be one "defun"
    (goto-char (point-max))))

(defun gleam-end-of-string ()
  "Skip to the end of the current string."
  (save-excursion
    (skip-syntax-forward "^\"|")
    (skip-syntax-forward "\"|")
    (point)))

(defvar gleam-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-f") 'gleam-format-buffer)
    map)
  "Keymap for Gleam major mode.")

;;;###autoload
(define-derived-mode gleam-mode prog-mode "Gleam"
  "Major mode for Gleam code.

\\{gleam-mode-map}"
  :group 'gleam-mode
  :syntax-table gleam-mode-syntax-table

  ;; Syntax.
  (setq-local syntax-propertize-function #'gleam-syntax-propertize)

  ;; Indentation
  (setq-local indent-line-function 'gleam-mode-indent-line)

  ;; Fonts
  (setq-local font-lock-defaults '(gleam-mode-font-lock-keywords
                                   nil nil nil nil
                                   (font-lock-syntactic-face-function . gleam-mode-syntactic-face-function)
                                   ))

  ;; Misc
  (setq-local comment-start "// ")
  (setq-local comment-end   "")
  (setq-local indent-tabs-mode nil)
  (setq-local open-paren-in-column-0-is-defun-start nil)

  ;; Auto indent on }
  (setq-local
   electric-indent-chars (cons ?} (and (boundp 'electric-indent-chars)
                                       electric-indent-chars)))

  ;; Allow paragraph fills for comments
  (setq-local comment-start-skip "\\(?://[/!]*\\|/\\*[*!]?\\)[[:space:]]*")
  (setq-local paragraph-start
       (concat "[[:space:]]*\\(?:" comment-start-skip "\\|\\*/?[[:space:]]*\\|\\)$"))
  (setq-local paragraph-separate paragraph-start)
  (setq-local normal-auto-fill-function 'gleam-do-auto-fill)
  (setq-local fill-paragraph-function 'gleam-fill-paragraph)
  (setq-local fill-forward-paragraph-function 'gleam-fill-forward-paragraph)
  (setq-local adaptive-fill-function 'gleam-find-fill-prefix)
  (setq-local adaptive-fill-first-line-regexp "")
  (setq-local comment-multi-line t)
  (setq-local comment-line-break-function 'gleam-comment-indent-new-line)
  (setq-local imenu-generic-expression gleam-imenu-generic-expression)
  (setq-local imenu-syntax-alist '((?! . "w"))) ; For macro_rules!
  (setq-local beginning-of-defun-function 'gleam-beginning-of-defun)
  (setq-local end-of-defun-function 'gleam-end-of-defun)
  (setq-local parse-sexp-lookup-properties t)
  (setq-local electric-pair-inhibit-predicate 'gleam-electric-pair-inhibit-predicate-wrap)

  (add-hook 'before-save-hook 'gleam-before-save-hook nil t)
  (add-hook 'after-save-hook 'gleam-after-save-hook nil t)

  (setq-local gleam-buffer-project nil)

  (when gleam-always-locate-project-on-open
    (gleam-update-buffer-project)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.gleam\\'" . gleam-mode))

(provide 'gleam-mode)

;;; gleam-mode.el ends here
