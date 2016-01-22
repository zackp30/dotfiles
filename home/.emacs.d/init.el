(defvar melpa-url
  "https://melpa.org/packages/"
  "URL to the MELPA Emacs Lisp package Archive.")

(setq package-archives `(("melpa" . ,melpa-url)))

(package-initialize)

;; A utility function (borrowed from Bling's
;; (https://github.com/bling)) configuration to install a package.
(defun require-package (package)
  "Install given PACKAGE."
  (unless (package-installed-p package)
    (unless (assoc package package-archive-contents)
      (package-refresh-contents))
    (package-install package)))

(require-package 'el-get)

;; Since my configuration is a giant Org document, this needs to go
;; here since the correct version of Org isn't loaded, as `(require
;; 'org)' requires the built-in Emacs Org.

(el-get-bundle org-mode ;; following from https://raw.githubusercontent.com/dimitri/el-get/master/recipes/org-mode.rcp
  :website "http://orgmode.org/"
  :description "Org-mode is for keeping notes, maintaining ToDo lists, doing project planning, and authoring with a fast and effective plain-text system."
  :type git
  :url "git://orgmode.org/org-mode"
  :info "doc"
  :build/berkeley-unix `,(mapcar
                          (lambda (target)
                            (list "gmake" target (concat "EMACS=" (shell-quote-argument el-get-emacs))))
                          '("oldorg"))
  :build `,(mapcar
            (lambda (target)
              (list "make" target (concat "EMACS=" (shell-quote-argument el-get-emacs))))
            '("oldorg"))
  :load-path ("." "contrib/lisp" "lisp")
  :load ("lisp/org-loaddefs.el"))

(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'load-path "~/emacs-dbgr/")
(setq custom-theme-directory "~/.emacs.d/themes")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(require 'org)
(require 'htmlize)

(setq vc-follow-symlinks t)

;; Server handling
(defun shell-without-newline (s)
  "Remove the newline from the output of a shell command.
S: shell command to run"
  (replace-regexp-in-string "\n$" ""
                            (shell-command-to-string s)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode t)
 '(custom-safe-themes
   (quote
    ("c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "26614652a4b3515b4bbbb9828d71e206cc249b67c9142c06239ed3418eff95e2" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default)))
 '(el-get-git-shallow-clone t)
 '(indent-guide-char "│")
 '(magit-rebase-arguments (quote ("--keep-empty" "--preserve-merges")))
 '(magit-revision-show-gravatars (quote ("^Author:     " . "^Commit:     ")))
 '(package-selected-packages
   (quote
    (list-utils "list-utils" loc-changes load-relative material-theme yasnippet yaml-mode ws-butler workgroups2 wgrep-ag wgrep web-mode wanderlust todotxt sx surround spinner smart-mode-line slime-company slime slim-mode scss-mode rust-mode rainbow-identifiers rainbow-delimiters rainbow-blocks racket-mode perspective ocodo-svg-modelines nim-mode mmm-mode mediawiki zenburn-theme markdown-mode magit-tramp magit lua-mode lentic ledger-mode julia-mode js2-mode io-mode indent-guide ibuffer-vc hydra hy-mode highlight-numbers helm-swoop helm-projectile helm-ag go-mode gnuplot-mode gitignore-mode gitconfig-mode git-timemachine git-gutter git-commit-mode gist ghci-completion ggtags flycheck-rust flycheck-ledger flycheck evil-visualstar evil-numbers evil-nerd-commenter evil-matchit evil-leader evil-indent-textobject evil etags-select emmet-mode emacs-eclim elixir-mode ein editorconfig edit-server dired-toggle-sudo dired-rainbow dired+ d-mode ctags company-tern company-ghc company-anaconda company coffee-mode cmake-mode cider browse-kill-ring bookmark+ ag ace-window ace-jump-helm-line ace-flyspell use-package)))
 '(persp-keymap-prefix "w")
 '(safe-local-variable-values
   (quote
    ((auto-fill-mode . t)
     (Package . SYSTEM)
     (Syntax . Common-Lisp)
     (indent-tabs-mode t)
     (encoding . utf-8)
     (haskell-process-use-ghci . t)
     (haskell-indent-spaces . 4))))
 '(sml/full-mode-string " …"))

(require 'ob-tangle)
(org-babel-load-file "~/.emacs.d/config.org")
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flyspell-incorrect ((t (:underline (:color "#CC9393" :style wave) :slant italic))))
 '(font-lock-comment-face ((t (:foreground "#7F9F7F" :slant italic))))
 '(helm-match ((t (:background "#3F3F3F"))))
 '(markdown-markup-face ((t (:inherit shadow :foreground "dim gray" :slant normal :weight normal))))
 '(mode-line ((t (:background "#440000" :foreground "#ffffff" :inverse-video nil :box (:line-width 5 :style released-button)))))
 '(org-document-title ((t (:inherit org-level-1 :box 2 :underline nil :height 2.0)))))

(server-start)

(load-theme 'zenburn t)
