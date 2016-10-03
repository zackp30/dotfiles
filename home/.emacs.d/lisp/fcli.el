(define-derived-mode fcli-mode
  prog-mode "Fortinet")

(add-to-list 'auto-mode-alist '("\\.fcli\\'" . fcli-mode))

(font-lock-add-keywords 'fcli-mode
                        '(("\\<edit\\|config\\|end\\|set\\>" . 'font-lock-keyword-face)))
