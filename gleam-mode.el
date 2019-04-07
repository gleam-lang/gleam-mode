(defconst gleam-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; " is a string delimiter
    (modify-syntax-entry ?\" "\"" table)

    ;; / is punctuation, but // is a comment starter
    (modify-syntax-entry ?/ ". 12" table)
    ;; \n is a comment ender
    (modify-syntax-entry ?\n ">" table)
    table))

(define-derived-mode gleam-mode prog-mode "Gleam Mode"
  :syntax-table gleam-mode-syntax-table
  (font-lock-fontify-buffer))
