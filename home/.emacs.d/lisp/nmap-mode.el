(define-derived-mode nmap-mode
  prog-mode "NMAP")

(add-to-list 'auto-mode-alist '("\\.nmap\\'" . nmap-mode))

(defface nmap-face-open `((t (:background "green" :foreground "black")))
  "Face for open ports"
  :group 'nmap-faces)

(defface nmap-face-closed `((t (:background "red" :foreground "black")))
  "Face for open ports"
  :group 'nmap-faces)

(defface nmap-face-filtered `((t (:background "lightblue" :foreground "black")))
  "Face for open ports"
  :group 'nmap-faces)

(font-lock-add-keywords 'nmap-mode
                        `(("\\<open\\>" . 'nmap-face-open)
                          ("\\<closed\\>" . 'nmap-face-closed)
                          (,(regexp-or "PORT" "STATE" "SERVICE") . 'font-lock-keyword-face)
                          ("\\<filtered\\>" . 'nmap-face-filtered)))

(provide 'nmap-mode)
