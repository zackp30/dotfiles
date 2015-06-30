
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq vc-follow-symlinks t)
(require 'ob-tangle)
(org-babel-load-file "~/.emacs.d/config.org")
(custom-set-faces '(isearch ((t (:background "#5F5F5F" :foreground "#D0BF8F" :box (:line-width 2 :color "orange red" :style released-button) :weight bold))))
                  '(rainbow-delimiters-depth-1-face ((t (:foreground "orange red"))))
                  '(rainbow-delimiters-depth-2-face ((t (:foreground "yellow"))))
                  '(rainbow-identifiers-identifier-1 ((t (:foreground "orange red")))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (yasnippet yaml-mode ws-butler workgroups2 wgrep-ag wgrep web-mode wanderlust todotxt sx surround spinner smart-mode-line slime-company slime slim-mode scss-mode rust-mode rainbow-identifiers rainbow-delimiters rainbow-blocks racket-mode perspective ocodo-svg-modelines nim-mode mmm-mode mediawiki zenburn-theme markdown-mode magit-tramp magit-gitflow magit-gh-pulls magit lua-mode lentic ledger-mode julia-mode js2-mode io-mode indent-guide ibuffer-vc hydra hy-mode highlight-numbers helm-swoop helm-projectile helm-ag go-mode gnuplot-mode gitignore-mode gitconfig-mode git-timemachine git-gutter git-commit-mode gist ghci-completion ggtags flycheck-rust flycheck-ledger flycheck evil-visualstar evil-numbers evil-nerd-commenter evil-matchit evil-leader evil-indent-textobject evil etags-select emmet-mode emacs-eclim elixir-mode ein editorconfig edit-server dired-toggle-sudo dired-rainbow dired+ d-mode ctags company-tern company-ghc company-anaconda company coffee-mode cmake-mode cider browse-kill-ring bookmark+ ag ace-window ace-jump-helm-line ace-flyspell use-package))))

(load-theme 'zenburn t)
