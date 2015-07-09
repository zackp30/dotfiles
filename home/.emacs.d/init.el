(package-initialize)

(setq vc-follow-symlinks t)

;; Server handling

(defun shell-without-newline (s)
  "remove the newline from the output of a shell command"
  (replace-regexp-in-string "\n$" ""
                            (shell-command-to-string s)))
(defun get-project-name ()
  "get the project that Emacs is running within"
  (if (string=
       (shell-without-newline "basename $(projectroot)")
       (shell-without-newline "basename $HOME"))
      "server"
    (shell-without-newline "basename $(projectroot)")))

(setq pid-dir (concat "/tmp/emacs" (number-to-string (user-uid)) "/ready/"))
(setq server-name (get-project-name))
(setq pid-file (concat pid-dir server-name))

;; Here we determine whether we're about to conflict with another Emacs server.
(when (string= "yes" (shell-without-newline (concat
                                "isemacsrunning "
                               server-name
                                " "
                                (number-to-string 1))))
  (progn (error (concat "Emacs server (" server-name ") is already running, exiting"))
         (kill-emacs)))

(when (and (not server-mode))
  (server-start))


(defun init-pid ()
  (when (file-exists-p pid-file)
    (delete-file pid-file))
  (when (not (file-exists-p pid-dir))
    (make-directory pid-dir t))
  (when (file-exists-p pid-file)
    (add-hook 'kill-emacs-hook
              (lambda ()
                (delete-file (concat "/tmp/emacs" (number-to-string (user-uid)) "/ready/" server-name)))))
  (append-to-file (number-to-string (emacs-pid)) nil pid-file))
(add-hook 'after-init-hook 'init-pid)
(require 'ob-tangle)
(org-babel-load-file "~/.emacs.d/config.org")
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default)))
 '(indent-guide-char "│")
 '(package-selected-packages
   (quote
    (yasnippet yaml-mode ws-butler workgroups2 wgrep-ag wgrep web-mode wanderlust todotxt sx surround spinner smart-mode-line slime-company slime slim-mode scss-mode rust-mode rainbow-identifiers rainbow-delimiters rainbow-blocks racket-mode perspective ocodo-svg-modelines nim-mode mmm-mode mediawiki zenburn-theme markdown-mode magit-tramp magit lua-mode lentic ledger-mode julia-mode js2-mode io-mode indent-guide ibuffer-vc hydra hy-mode highlight-numbers helm-swoop helm-projectile helm-ag go-mode gnuplot-mode gitignore-mode gitconfig-mode git-timemachine git-gutter git-commit-mode gist ghci-completion ggtags flycheck-rust flycheck-ledger flycheck evil-visualstar evil-numbers evil-nerd-commenter evil-matchit evil-leader evil-indent-textobject evil etags-select emmet-mode emacs-eclim elixir-mode ein editorconfig edit-server dired-toggle-sudo dired-rainbow dired+ d-mode ctags company-tern company-ghc company-anaconda company coffee-mode cmake-mode cider browse-kill-ring bookmark+ ag ace-window ace-jump-helm-line ace-flyspell use-package))))

(load-theme 'zenburn t)

