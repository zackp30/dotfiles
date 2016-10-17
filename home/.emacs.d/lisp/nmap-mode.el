(define-derived-mode nmap-mode
  prog-mode "NMAP")

(add-to-list 'auto-mode-alist '("\\.nmap\\'" . nmap-mode))

(defface nmap-face-open '((t :background "green"))
  "Face for open ports"
  :group 'nmap-faces)

(font-lock-add-keywords 'nmap-mode
                        '(("\\<open\\>" . nmap-face-open)))
