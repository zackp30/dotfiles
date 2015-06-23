
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq vc-follow-symlinks t)
(require 'ob-tangle)
(org-babel-load-file "~/.emacs.d/config.org")
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#262a26" :foreground "#c6c6c6" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 108 :width normal :foundry "unknown" :family "DejaVu Sans Mono"))))
 '(flyspell-incorrect ((t (:background "#ef2929" :foreground "#c6c6c6" :underline (:color foreground-color :style wave) :weight bold))))
 '(font-lock-string-face ((t (:foreground "LavenderBlush4"))))
 '(isearch ((t (:box (:line-width 2 :color "grey75" :style released-button) :underline (:color foreground-color :style wave) :slant oblique :width expanded)))))
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
    (moe-theme zenburn-theme yasnippet yaml-mode ws-butler workgroups2 wgrep-ag web-mode wanderlust use-package todotxt sx surround spinner smart-mode-line slime-company slim-mode scss-mode rust-mode rainbow-identifiers rainbow-delimiters rainbow-blocks racket-mode perspective ocodo-svg-modelines nim-mode mmm-mode mediawiki material-theme magit-tramp magit-gitflow magit-gh-pulls lua-mode lentic ledger-mode julia-mode js2-mode io-mode indent-guide ibuffer-vc hydra hy-mode highlight-numbers helm-swoop helm-projectile helm-ag go-mode gnuplot-mode gitignore-mode gitconfig-mode git-timemachine git-gutter gist ghci-completion ggtags flycheck-rust flycheck-ledger evil-visualstar evil-numbers evil-nerd-commenter evil-matchit evil-leader evil-indent-textobject etags-select emmet-mode emacs-eclim elixir-mode ein editorconfig edit-server dired-toggle-sudo dired-rainbow dired+ d-mode ctags company-tern company-ghc company-anaconda coffee-mode cmake-mode cider browse-kill-ring bookmark+ ag ace-window ace-jump-helm-line ace-flyspell))))
