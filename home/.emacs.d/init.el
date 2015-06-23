
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq vc-follow-symlinks t)
(require 'ob-tangle)
(org-babel-load-file "~/.emacs.d/config.org")
(require 'moe-theme)
(moe-dark)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#262a20" :foreground "#c6c6c6" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 109 :width normal :foundry "unknown" :family "DejaVu Sans Mono")))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default)))
 '(package-selected-packages
   (quote
    (yasnippet yaml-mode ws-butler workgroups2 wgrep-ag wgrep web-mode wanderlust todotxt sx surround spinner smart-mode-line slime-company slime slim-mode scss-mode rust-mode rainbow-identifiers rainbow-delimiters rainbow-blocks racket-mode perspective ocodo-svg-modelines nim-mode mmm-mode mediawiki zenburn-theme markdown-mode magit-tramp magit-gitflow magit-gh-pulls magit lua-mode lentic ledger-mode julia-mode js2-mode io-mode indent-guide ibuffer-vc hydra hy-mode highlight-numbers helm-swoop helm-projectile helm-ag go-mode gnuplot-mode gitignore-mode gitconfig-mode git-timemachine git-gutter git-commit-mode ghci-completion ggtags flycheck-rust flycheck-ledger flycheck evil-visualstar evil-numbers evil-nerd-commenter evil-matchit evil-leader evil-indent-textobject evil etags-select emmet-mode emacs-eclim elixir-mode ein editorconfig edit-server dired-toggle-sudo dired-rainbow dired+ d-mode ctags company-tern company-ghc company-anaconda company coffee-mode cmake-mode cider browse-kill-ring bookmark+ ag ace-window ace-jump-helm-line ace-flyspell use-package moe-theme material-theme gist afternoon-theme))))
