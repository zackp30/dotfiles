;;; Commentary:
;; A hacked together Emacs config.
;;; Code:
;; Add repositories for package archives
(setq package-archives '(("melpa" . "http://melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))
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
                git-rebase-mode
                gitconfig-mode
                gitignore-mode
                scss-mode ;; mode for the Sass language
                pophint
                ag ;; the silver searcher
                workgroups2
                auto-complete-clang
                robe
                rainbow-identifiers ;; rainbows!
                rainbow-blocks ;; omg more rainbows
                yaml-mode
                flycheck-rust ;; flycheck for the Rust language
                rust-mode ;; mode for the Rust language
                slime-company
                zenburn-theme ;; Zenburn theme
                surround ;; Delete surrounding characters (`()', `[]', etc.).
                auto-complete ;; autocompletion
                highlight-numbers ;; rainbowify numbers
                todotxt ;; Mode for the todo.txt markup
                magit ;; git integration
                magit-gh-pulls
                magit-gitflow
                magit-tramp
                table ;; tables!
                smex
                emmet-mode
                ibuffer-vc
                mediawiki ;; mediawiki client
                wgrep-ag ;; writable grep, but for ag
                racket-mode ;; mode for the Racket 
                undo-tree ;; vim-like undo tree
                hydra ;; micro-states!
                hy-mode
                company
                company-anaconda
                company-ghc
                projectile ;; project management
                smartparens ;; automatically insert parenthesis
                helm-swoop
                ein
                bookmark+
                helm-ag
                browse-kill-ring ;; menu for the killring
                emacs-eclim ;; turn emacs into an even more IDEer thing using eclim!
                coffee-mode ;; mode for the CoffeeScript language
                git-gutter ;; Git status in left fringe
                markdown-mode ;; mode for the Markdown markup
                indent-guide ;; a "ruler" for indentation
                rainbow-delimiters ;; RAINNNNNNNNNNBOOOOWWZZ
                nim-mode
                helm-projectile ;; projectile integration for helm
                perspective ;; basically tabs
                smart-mode-line ;; a nice mode line
                wanderlust ;; email
                sx
                yasnippet ;; snippets
                evil-snipe
                mmm-mode
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
                ace-window
                d-mode ;; mode for the D language
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
                sourcegraph
                go-mode
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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bmkp-last-as-first-bookmark-file "/home/zack/.emacs.d/bookmarks")
 '(custom-safe-themes
   (quote
    ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "756597b162f1be60a12dbd52bab71d40d6a2845a3e3c2584c6573ee9c332a66e" "6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default)))
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
 '(safe-local-variable-values (quote ((auto-fill-mode) (auto-fill-mode . 1))))
 '(sml/full-mode-string " ...")
 '(sml/show-client t)
 '(sml/theme (quote dark))
 '(socks-server (quote ("Default server" "localhost" 9001 5)))
 '(transient-mark-mode 1))


;; Misc requires
(require 'htmlize)
(use-package pophint
  :bind ("C-'" . pophint:do-flexibly))
(use-package indent-guide)
(use-package company-ghc
  :config
  (add-to-list 'company-backends 'company-ghc))

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
  (a-mode "php" "web"))
(use-package yasnippet
  :config
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil))
(use-package mediawiki)
(use-package todotxt)
(defun activate-company-ispell ()
  "Activate the company ispell backend. Used for hooks."
  (add-to-list 'company-backends 'company-ispell))
(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-idle-delay 0.1)
  (setq company-minimum-prefix-length 1)
  (unbind-key (kbd "C-w") company-active-map)
  (define-key company-active-map (kbd "C-u") 'company-show-location)
  (make-variable-buffer-local 'company-backends)
  (add-hook 'markdown-mode-hook 'activate-company-ispell))
(use-package slime
  :config
  (slime-setup '(slime-company)))
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
  (add-hook 'haskell-mode-hook 'ghc-init)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation))
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
(a-mode ".mw" "mediawiki")
(a-mode "Gemfile" "ruby")
(a-mode "Guardfile" "ruby")
(a-mode "Rakefile" "ruby")
(a-mode ".ledger" "ledger")
(add-to-list 'auto-mode-alist
             '("mutt-" . mail-mode))



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

(require 'gl-conf-mode)
(add-to-list 'auto-mode-alist '("gitolite\\.conf\\'" .
                                gl-conf-mode))

;; ALL the modes!
(eval-after-load "evil"
  '(define-key evil-insert-state-map (kbd "C-k") 'yas-expand))

(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
(define-key global-map (kbd "C-c w") 'ace-window)

(yas-global-mode 1)
(indent-guide-global-mode 1)
(helm-mode 1)
(global-git-gutter-mode 1)
(smartparens-global-mode t)
(global-surround-mode t)
(global-evil-leader-mode)

(require 'org)
(define-key global-map (kbd "C-c l") 'org-store-link)
(define-key global-map (kbd "C-c a") 'org-agenda)
(setq org-log-done t)

(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'prog-mode-hook  'flyspell-prog-mode)
(add-hook 'text-mode-hook  'flyspell-mode)
(add-hook 'ruby-mode-hook 'robe-mode)
(setq python-shell-interpreter "python3") ;; I use Python 3


(setq slime-contribs '(slime-fancy))
(setq inferior-lisp-program "clisp")

;; Yay zenburn.
(load-theme 'zenburn t)

;; Key bindings
;;(global-set-key (kbd "C-TAB") )
(global-set-key (kbd "C-c h") 'helm-projectile)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
(global-set-key (kbd "C-c r") 'random-commit-message)

(evil-leader/set-leader "<SPC>") ;; space is my leader
(evil-leader/set-key
  "p b" 'projectile-switch-to-buffer
  "p D" 'projectile-dired
  "p d" 'projectile-find-dir
  "p s" 'projectile-switch-project
  "p R" 'projectile-regenerate-tags
  "p j" 'projectile-find-tag
  "g t r" 'ctags-create-or-update-tags-table
  ";" 'replace-with-comma)



(setq list-command-history-max 500)
(setq-default indent-tabs-mode nil)

(require 'undo-tree)

(setq-default tab-width 2)
(setq undo-tree-auto-save-history 1)
(setq undo-tree-history-directory-alist (quote (("." . "~/.emacs.d/undo/"))))

;; Haskell unicode symbols! (from the Haskell wiki)
(setq haskell-font-lock-symbols t)
(autoload 'scss-mode "scss-mode")

(a-mode ".scss" "scss")
(setq scss-compile-at-save nil)

(add-hook 'prog-mode-hook 'highlight-numbers-mode)
(add-hook 'prog-mode-hook 'rainbow-identifiers-mode)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)




;; Misc functions
(defun increment-number-at-point ()
  (interactive)
  (skip-chars-backward "0123456789")
  (or (looking-at "[0123456789]+")
      (error "No number at point"))
  (replace-match (number-to-string (1+ (string-to-number (match-string 0))))))

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


(defun replace-with-comma ()
  (interactive)
  (delete-forward-char 1)
  (insert ",")
  (evil-append 1))

(require 'ag)
(define-key ag-mode-map (kbd "k") nil) ;; stop conflicts with evil

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-line ((t (:background "color-239"))))
 '(linum ((t (:background "brightblack" :foreground "#9FC59F"))))
 '(sml/prefix ((t (:inherit sml/global :foreground "#bf6000")))))

(defun random-commit-message ()
  (interactive)
  (insert (get-rnd-list '("¯\\_(ツ)_/¯"
                          "I need to think of better commit messages."
                          "blah"))))

(setq evil-snipe-auto-disable-substitute nil)
(global-evil-snipe-mode 1)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(sourcegraph-mode 1)

(eval-after-load 'flycheck '(require 'flycheck-ledger))

(evil-define-key 'normal evil-snipe-mode-map "zA" 'evil-snipe-f)
(evil-define-key 'normal evil-snipe-mode-map "]S" 'flyspell-goto-next-error)
(define-key evil-normal-state-map (kbd "TAB") 'org-cycle)

(defun testthing ()
  (when (file-exists-p (format "%s#%s#"
                               (file-name-directory (buffer-file-name (current-buffer)))
                               (file-name-nondirectory (buffer-file-name (current-buffer)))))
    t))


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

(helm-autoresize-mode 1)

(require 'magit-gitflow)
(add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)
(add-hook 'magit-mode-hook 'turn-on-magit-gitflow)

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

(add-hook 'python-mode-hook 'eldoc-mode)

(winner-mode 1)

(setq magit-auto-revert-mode nil)
(setq magit-last-seen-setup-instructions "1.4.0")

(show-paren-mode 1)

(provide 'init)
;;; init.el ends here
