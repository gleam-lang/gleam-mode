;;; gleam-mode.el --- emacs major mode for the Gleam language

(setq gleam-font-lock-keywords
      (let* (
            ;; define several category of keywords
            (x-keywords '("case" "const" "external" "fn" "import" "let" "pub" "type"
                          "assert" "try" "tuple" "opaque"))
            (x-booleans '("True" "False"))

            ;; generate regex string for each category of keywords
            (x-keywords-regexp (regexp-opt x-keywords 'words))
            (x-booleans-regexp (regexp-opt x-booleans 'words)))

        `(
          (,x-keywords-regexp . font-lock-keyword-face)
          (,x-booleans-regexp . font-lock-constant-face)
          ("fn \\([_[:alpha:]]+\\)(" 1 font-lock-function-name-face)
          ("[[:upper:]][[:alpha:]]*" . font-lock-type-face)
          ;; note: order above matters, because once colored, that part won't change.
          ;; in general, put longer words first
          )))

;;;###autoload
(define-derived-mode gleam-mode c-mode "gleam mode"
  "Major mode for editing gleam"
  ;; use // for commenting lines
  (setq-local comment-start "// ")
  (setq-local comment-end "")

  ;; code for syntax highlighting
  (setq font-lock-defaults '((gleam-font-lock-keywords))))

;; add the mode to the `features' list
(provide 'gleam-mode)
