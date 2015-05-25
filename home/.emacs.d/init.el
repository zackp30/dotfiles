;;; Commentary:
;; A hacked together Emacs config.
;;; Code:
;; Add repositories for package archives
(setq package-archives '(("melpa" . "http://melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))
(if (file-exists-p "~/.local.el")
    (load "~/.local.el"))

(defun require-package (package)
  "Install given PACKAGE."
  (unless (package-installed-p package)
    (unless (assoc package package-archive-contents)
      (package-refresh-contents))
    (package-install package))) ;; https://github.com/bling/dotemacs

(package-initialize)

(require 'cl)
;; Install packages.
(add-to-list 'load-path "~/.emacs.d/lisp")
(defvar pkgs '(use-package
                evil
                evil-leader
                evil-numbers
                evil-visualstar
                evil-nerd-commenter
                editorconfig
                evil-indent-textobject
                evil-matchit
                etags-select
                git-commit-mode
                gitconfig-mode
                git-timemachine
                gitignore-mode
                scss-mode ;; mode for the Sass language
                ag ;; the silver searcher
                workgroups2
                edit-server ;; used by Edit With Emacs
                gist
                robe
                rainbow-identifiers ;; rainbows!
                rainbow-blocks ;; omg more rainbows
                yaml-mode
                flycheck-rust ;; flycheck for the Rust language
                rust-mode ;; mode for the Rust language
                slime-company
                zenburn-theme ;; Zenburn theme
                surround ;; Delete surrounding characters (`()', `[]', etc.).
                highlight-numbers ;; rainbowify numbers
                todotxt ;; Mode for the todo.txt markup
                magit ;; git integration
                magit-gh-pulls ;; see GitHub pull requests in Magit
                magit-gitflow ;; Magit interface for git-flow
                magit-tramp ;; TRAMP integration for Magit
                table ;; tables!
                smex ;; fancy
                emmet-mode ;; http://emmet.io implementation for Emacs
                ibuffer-vc ;; ibuffer integration for vc.el
                mediawiki ;; mediawiki client
                wgrep-ag ;; writable grep, but for ag
                racket-mode ;; mode for the Racket 
                undo-tree ;; vim-like undo tree
                hydra ;; micro-states!
                hy-mode ;; hy mode
                company ;; auto completion
                company-anaconda ;; Python completion for company
                company-ghc ;; Haskell completion for company
                projectile ;; project management
                helm-swoop ;; grep-like tool for Helm
                ein ;; iPython Notebook for Emacs
                bookmark+ ;; 
                helm-ag
                browse-kill-ring ;; menu for the killring
                emacs-eclim ;; turn emacs into an even more IDEer thing using eclim!
                coffee-mode ;; mode for the CoffeeScript language
                git-gutter ;; Git status in left fringe
                material-theme
                markdown-mode ;; mode for the Markdown markup
                indent-guide ;; a "ruler" for indentation
                rainbow-delimiters ;; RAINNNNNNNNNNBOOOOWWZZ
                nim-mode
                helm-projectile ;; projectile integration for helm
                perspective ;; basically tabs
                smart-mode-line ;; a nice mode line
                wanderlust ;; email
                yasnippet ;; snippets
                evil-snipe
                mmm-mode
                ggtags
                ws-butler
                sx
                helm ;; menus for ALL the things
                flycheck ;; on the fly syntax checking
                haskell-mode ;; mode for Haskell
                ruby-mode ;; mode for the Ruby language
                clojure-mode ;; mode for the Clojure language
                cider ;; REPL for Clojure
                lua-mode ;; mode for the Lua language
                ctags
                ledger-mode
                flycheck-ledger
                ace-jump-mode ;; easymotion
                ace-flyspell ;; ace-jump-mode for flyspell
                ace-jump-helm-line
                ace-window
                d-mode ;; mode for the D language
                js2-mode
                ocodo-svg-modelines
                company-tern
                web-mode ;; mode for web stuff
                ghc 
                ghci-completion
                cmake-mode ;; mode for the CMake language
                julia-mode ;; mode for the Julia language
                slim-mode ;; mode for the Slim templating language
                slime
                dired-toggle-sudo
                dired-rainbow
                gnuplot-mode
                dired+
                spinner
                go-mode
                elixir-mode
                io-mode))

(loop for pkg in pkgs do
      (require-package pkg))
(require 'use-package)
(defun a-mode (ext mode)
  "A 'shortcut' for `(add-to-list 'auto-mode-alist [...])`'"
  (add-to-list 'auto-mode-alist
               (cons
                (format "\\%s\\'" ext)
                (intern (concat mode "-mode")))))
(setq-default flycheck-emacs-lisp-load-path 'inherit)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default)))
 '(delete-selection-mode nil)
 '(eclim-eclipse-dirs (quote ("~/eclipse/eclipse")))
 '(eclim-executable "~/eclipse/eclipse/eclim")
 '(evilnc-hotkey-comment-operator "gco")
 '(global-hl-line-mode t)
 '(global-hl-line-sticky-flag t)
 '(indent-guide-char "│")
 '(inhibit-startup-screen t)
 '(mark-even-if-inactive t)
 '(mediawiki-site-alist
   (quote
    (("http://wiki.apertron.net" "http://wiki.apertron.net/" "zackp30" nil "Main Page")
     ("Wikipedia" "http://en.wikipedia.org/w/" "username" "password" "Main Page"))))
 '(package-selected-packages
   (quote
    (zenburn-theme yasnippet yaml-mode ws-butler workgroups2 wgrep-ag web-mode wanderlust use-package todotxt tabbar sx surround spinner smex smart-mode-line slime-company slim-mode scss-mode rust-mode robe rainbow-identifiers rainbow-delimiters rainbow-blocks racket-mode perspective ocodo-svg-modelines nim-mode mmm-mode mediawiki material-theme magit-tramp magit-gitflow magit-gh-pulls lua-mode ledger-mode julia-mode js2-mode io-mode indent-guide ibuffer-vc hydra hy-mode highlight-numbers helm-swoop helm-projectile helm-ag haskell-mode go-mode gnuplot-mode gitignore-mode gitconfig-mode git-timemachine git-gutter gist ghci-completion ggtags flycheck-rust flycheck-ledger evil-visualstar evil-tabs evil-snipe evil-numbers evil-nerd-commenter evil-matchit evil-leader evil-indent-textobject etags-select emmet-mode emacs-eclim elixir-mode ein editorconfig edit-server dired-toggle-sudo dired-rainbow dired+ define-word d-mode ctags company-tern company-ghc company-anaconda coffee-mode cmake-mode cider browse-kill-ring bookmark+ auto-complete-clang ag ace-window ace-jump-helm-line ace-flyspell)))
 '(safe-local-variable-values (quote ((auto-fill-mode) (auto-fill-mode . 1))))
 '(sml/full-mode-string " ...")
 '(sml/show-client t)
 '(sml/theme (quote dark))
 '(transient-mark-mode 1))

;; Misc requires
(require 'htmlize)
(use-package indent-guide
  :config
  (indent-guide-global-mode 1))
(use-package company-ghc
  :config
  (add-hook 'haskell-mode-hook (lambda ()
                                 (add-to-list 'company-backends 'company-ghc))))
(use-package helm-projectile
  :config
  (global-set-key (kbd "C-c h") 'helm-projectile))
(use-package surround
  :config
  (global-surround-mode 1))
(use-package git-gutter
  :config
  (global-git-gutter-mode 1)
  (git-gutter:linum-setup))
(use-package mmm-mode
  :config
  (mmm-add-classes
   '((markdown-latex
      :submode latex-mode
      :front "\\\\begin" ;; 2 blackslashes because of basedocument requiring 2 because of macro processing.
      :back "\\\\end")
     (markdown-erb
      :submode ruby-mode
      :front "<%"
      :back "%>")))
  (mmm-add-mode-ext-class 'markdown-mode "\\.md\\'" 'markdown-latex)
  (mmm-add-mode-ext-class 'markdown-mode "\\.mderb\\'" 'markdown-erb))

(use-package ace-jump-mode
  :config
  (define-key global-map (kbd "C-c SPC") 'ace-jump-mode))
(use-package ace-window
  :config
  (define-key global-map (kbd "C-c w") 'ace-window))
(use-package ibuffer-vc
  :bind ("C-x C-b" . ibuffer)
  :init
  (require 'ibuffer-vc)
  :config
  (setq ibuffer-formats
        '((mark modified read-only vc-status-mini " "
                (name 18 18 :left :elide)
                " "
                (size 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " "
                (vc-status 16 16 :left)
                " "
                filename-and-process)))
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-vc-set-filter-groups-by-vc-root))))
(use-package editorconfig)
(use-package bookmark+)
(use-package bitbake)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((ruby . t)
   (dot . t)
   (gnuplot . t)
   (org . t)))
(setq org-src-fontify-natively t)
(setq org-startup-with-inline-images t)

(use-package web-mode
  :init
  (a-mode ".phtml" "web")
  (a-mode ".tpl\\.php" "web")
  (a-mode ".[agj]sp" "web")
  (a-mode ".as[cp]x" "web")
  (a-mode ".erb" "web")
  (a-mode ".mustache" "web")
  (a-mode ".djhtml" "web")
  (a-mode ".ejs" "web")
  (a-mode ".html?" "web")
  (a-mode ".php" "web"))
(use-package js2-mode
  :init
  (a-mode ".js" "js2")
  (add-hook 'js2-mode-hook (lambda ()
                             (tern-mode t)
                             (add-to-list 'company-backends 'company-tern))))
(use-package ace-flyspell
  :config
  (define-key global-map (kbd "C-c .") 'ace-flyspell-jump-word))
(use-package ggtags
  :config
  (add-hook 'prog-mode-hook 'ggtags-mode))
(use-package ace-jump-helm-line
  :config
  (define-key helm-map (kbd "C-@") 'ace-jump-helm-line))
(use-package yasnippet
  :config
  (yas-global-mode 1)
  (a-mode ".snip" "snippet")
  (define-key yas-minor-mode-map (kbd "C-c n") 'yas-next-field)
  (define-key yas-minor-mode-map (kbd "C-c p") 'yas-prev-field)
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (define-key evil-insert-state-map (kbd "C-c RET") 'yas-expand))
(use-package mediawiki)
(use-package ws-butler
  :config
  (add-hook 'prog-mode-hook 'ws-butler-mode))
(use-package todotxt)
(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-idle-delay 0.1)
  (setq company-minimum-prefix-length 1)
  (unbind-key (kbd "C-w") company-active-map)
  (define-key company-active-map (kbd "C-u") 'company-show-location)
  (make-variable-buffer-local 'company-backends))
(use-package company-robe
  :config
  (add-to-list 'company-backends 'company-robe))
(use-package company-anaconda
  :config
  (add-hook 'python-mode-hook (lambda ()
                                (anaconda-mode)
                                (add-to-list 'company-backends 'company-anaconda))))
(use-package projectile
  :config
  (projectile-global-mode))
(use-package smart-mode-line
  :config
  (sml/setup) ;; modeline setup
  (sml/apply-theme 'dark)) ;; dark modeline
(use-package smex
  :bind ("M-x" . smex)
  :bind ("M-X" . smex-major-mode-commands))
(use-package haskell-mode
  :config
  (setq haskell-font-lock-symbols t)
  (add-hook 'haskell-mode-hook 'ghc-init)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation))
(use-package helm
  :config
  (helm-mode 1)
  (helm-autoresize-mode 1))
(use-package evil
  :config
  (evil-mode 1)
  (evil-define-key 'normal global-map (kbd "}]") 'emmet-next-edit-point)
  (evil-define-key 'normal global-map (kbd "{[") 'emmet-prev-edit-point)
  (evil-define-key 'normal global-map (kbd "U") 'undo-tree-visualize)
  (use-package evil-nerd-commenter
    :config
    (define-key evil-normal-state-map "gci" 'evilnc-comment-or-uncomment-lines)
    (define-key evil-normal-state-map "gcl" 'evilnc-quick-comment-or-uncomment-to-the-line)
    (define-key evil-normal-state-map "gll" 'evilnc-quick-comment-or-uncomment-to-the-line)
    (define-key evil-normal-state-map "gcc" 'evilnc-copy-and-comment-lines)
    (define-key evil-normal-state-map "gcp" 'evilnc-comment-or-uncomment-paragraphs)
    (define-key evil-normal-state-map "gcr" 'comment-or-uncomment-region)
    (define-key evil-normal-state-map "gcv" 'evilnc-toggle-invert-comment-line-by-line)))
(use-package edit-server
  :config
  (when (string= (system-name) "linux-nyit.site") ;; home PC
    (edit-server-start)))
(require 'slime-autoloads)
(use-package slime
  :config
  (add-hook 'slime-repl-mode-hook
            (lambda ()
              ;; my portable keyboard + VX Connectbot doesn't like M-p and M-n.
              (evil-define-key 'insert slime-repl-mode-map (kbd "C-p") 'slime-repl-previous-input)
              (evil-define-key 'insert slime-repl-mode-map (kbd "C-n") 'slime-repl-next-input)
              (evil-define-key 'normal slime-repl-mode-map (kbd "C-p") 'slime-repl-previous-input)
              (evil-define-key 'normal slime-repl-mode-map (kbd "C-n") 'slime-repl-next-input)))
  (slime-setup '(slime-fancy slime-repl slime-company)))
(defun turn-on-emmet-mode ()
  (emmet-mode 1))
(use-package emmet-mode
  :config
  (add-hook 'web-mode-hook 'turn-on-emmet-mode)
  (add-hook 'sgml-mode-hook 'turn-on-emmet-mode)
  (add-hook 'css-mode-hook 'turn-on-emmet-mode))
(use-package io-mode)
(use-package cmake-mode
  :init
  (add-to-list 'auto-mode-alist 
               '("CMakeLists.txt" . cmake-mode)) )
(use-package cider
  :config
  (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode))
(use-package flycheck
  :init
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (setq flycheck-highlighting-mode 'symbols)
  (setq flycheck-indication-mode 'left-fringe))
(require 'tramp)
(require 'whitespace)

(column-number-mode 1)

(menu-bar-mode -1) ;; disabe menubar
(tool-bar-mode -1) ;; disable toolbar
(scroll-bar-mode -1) ;; disable scrollbar
(global-linum-mode 1) ;; enable line numbers
(require 'ctags)
(setq ctags-command "/usr/bin/ctags-exuberant -e -R ")
(setq vc-follow-symlinks t)

;; Haskell!
(autoload 'ghc-init "ghc" nil t)

(defun get-rnd-list (lst)
  "Get a random item from a list."
  (nth (random* (length lst)) lst))

(defun random-color ()
  "Get a random color."
  (get-rnd-list '("blue" "red" "yellow" "pink")))

(autoload 'wl "wl" "Wanderlust" t)
(add-to-list 'auto-mode-alist 
             '(".wl" . emacs-lisp-mode)) 
(a-mode ".md" "markdown")
(a-mode ".markdown" "markdown")
(a-mode ".mderb" "markdown")
(a-mode ".mw" "mediawiki")
(a-mode "Gemfile" "ruby")
(a-mode "Guardfile" "ruby")
(a-mode "Rakefile" "ruby")
(a-mode ".ledger" "ledger")
(add-to-list 'auto-mode-alist
             '("mutt-" . mail-mode)) ;; mutt temporary files

;; From Bling
(defun my-evil-modeline-change (default-color)
  "changes the modeline color when the evil mode changes"
  (let ((color (cond ((evil-insert-state-p) '("#002233" . "#ffffff"))
                     ((evil-visual-state-p) '("#330022" . "#ffffff"))
                     ((evil-normal-state-p) default-color)
                     (t '("#440000" . "#ffffff")))))
    (set-face-background 'mode-line (car color))
    (set-face-foreground 'mode-line (cdr color))))

(lexical-let ((default-color (cons (face-background 'mode-line)
                                   (face-foreground 'mode-line))))
  (add-hook 'post-command-hook (lambda () (my-evil-modeline-change default-color))))

(use-package gl-conf-mode
  :config
  (add-to-list 'auto-mode-alist '("gitolite\\.conf\\'" .
                                  gl-conf-mode)))

(electric-pair-mode 1)

(require 'org)
(define-key global-map (kbd "C-c l") 'org-store-link)
(define-key global-map (kbd "C-c a") 'org-agenda)
(setq org-log-done t)
(setq org-directory "~/org")

(add-hook 'after-init-hook 'global-flycheck-mode)
(add-hook 'prog-mode-hook  'flyspell-prog-mode)
(add-hook 'text-mode-hook  'flyspell-mode)
(add-hook 'ruby-mode-hook 'robe-mode)
(setq python-shell-interpreter "python3") ;; I use Python 3


(setq slime-contribs '(slime-fancy))
(setq inferior-lisp-program "clisp")

;; Yay material design.
(load-theme 'material t)

;; Key bindings
;;(global-set-key (kbd "C-TAB") )
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
(global-set-key (kbd "C-c r") 'random-commit-message)

(use-package evil-leader
  :config
  (evil-leader/set-leader "<SPC>") ;; space is my leader
  (global-evil-leader-mode 1)
  (evil-leader/set-key
    "p b" 'projectile-switch-to-buffer
    "p D" 'projectile-dired
    "p d" 'projectile-find-dir
    "p s" 'projectile-switch-project
    "p R" 'projectile-regenerate-tags
    "p j" 'projectile-find-tag
    "g t r" 'ctags-create-or-update-tags-table))

(setq list-command-history-max 500)
(setq-default indent-tabs-mode nil)

(use-package undo-tree
  :config
  (setq undo-tree-auto-save-history 1)
  (setq undo-tree-history-directory-alist (quote (("." . "~/.emacs.d/undo/"))))
  (setq undo-tree-visualizer-diff t))

(setq-default tab-width 2)

(use-package scss-mode
  :config
  (setq scss-compile-at-save nil)
  (a-mode ".scss" "scss"))

(use-package highlight-numbers
  :config
  (add-hook 'prog-mode-hook 'highlight-numbers-mode))
(use-package rainbow-identifiers
  :config
  (add-hook 'prog-mode-hook 'rainbow-identifiers-mode))
(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; Misc functions
(defun increment-number-at-point ()
  (interactive)
  (skip-chars-backward "0123456789")
  (or (looking-at "[0123456789]+")
      (error "No number at point"))
  (replace-match (number-to-string (1+ (string-to-number (match-string 0))))))

(add-to-list 'imenu-generic-expression
             '("Used Packages"
               "\\(^\\s-*(use-package +\\)\\(\\_<.+\\_>\\)" 2))


(defun decrement-number-at-point ()
  (interactive)
  (skip-chars-backward "0123456789")
  (or (looking-at "[0123456789]+")
      (error "No number at point"))
  (replace-match (number-to-string (1- (string-to-number (match-string 0))))))

(global-set-key (kbd "C-c +") 'increment-number-at-point)
(global-set-key (kbd "C-c -") 'decrement-number-at-point)

(require 'saveplace)
(setq-default save-place t)

(use-package ag
  :config
  (define-key ag-mode-map (kbd "k") nil)) ;; stop conflicts with evil

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(defun random-commit-message ()
  (interactive)
  (insert (get-rnd-list '("¯\\_(ツ)_/¯"
                          "I need to think of better commit messages."
                          "blah"))))

(setq evil-snipe-auto-disable-substitute nil)
(global-evil-snipe-mode 1)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(eval-after-load 'flycheck '(require 'flycheck-ledger))

(evil-define-key 'normal evil-snipe-mode-map "zA" 'evil-snipe-f)
(evil-define-key 'normal evil-snipe-mode-map "]S" 'flyspell-goto-next-error)
(define-key evil-normal-state-map (kbd "TAB") 'org-cycle)

(add-hook 'mail-mode-hook 'auto-fill-mode)
(defun foo-wl ()
  (when evil-mode (evil-change-state 'emacs)))

(add-hook 'wl-hook 'foo-wl)
(add-hook 'wl-folder-mode-hook 'foo-wl)
(add-hook 'wl-summary-mode-hook 'foo-wl)
(add-hook 'wl-message-mode-hook 'foo-wl)
(add-hook 'mime-view-mode-hook 'foo-wl)

(setq helm-display-header-line nil)
(set-face-attribute 'helm-source-header nil :height 0.1)

(use-package magit
  :config
  (setq magit-auto-revert-mode nil)
  (setq magit-last-seen-setup-instructions "1.4.0")

  (use-package magit-gitflow
    :config
    (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)
    (add-hook 'magit-mode-hook 'turn-on-magit-gitflow)))

(defun insert-shell-command (command)
  (interactive "scommand: ")
  (insert (shell-command-to-string command)))

(define-key global-map (kbd "C-c C-g") 'insert-shell-command)

(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))
(setq savehist-file "~/.emacs.d/savehist")
(savehist-mode 1)
(setq history-length t)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables
      '(kill-ring
        search-ring
        regexp-search-ring))

(add-hook 'python-mode-hook 'turn-on-eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)


(electric-indent-mode 1)
(show-paren-mode 1)

(mouse-avoidance-mode 'banish)

(setq evil-insert-state-cursor '((bar . 2) "blue")
      evil-visual-state-cursor '((bar . 5) "red")
      evil-normal-state-cursor '((hollow . 5) "white"))

(setq initial-scratch-message
      (format ";; Emacs was started at %s"
              (format-time-string "%Y-%m-%dT%T")))

(setq package-menu-async nil)

(setq mmm-global-mode 'maybe)

(provide 'init)
;;; init.el ends here
