;; Commentary:

;; A hacked together Emacs config.

;;; Code:


;; Add repositories for package archives
(setq package-archives '(("melpa" . "http://melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))

(defun require-package (package)
  "Install given PACKAGE."
  (unless (package-installed-p package)
    (unless (assoc package package-archive-contents)
      (package-refresh-contents))
    (package-install package))) ;; https://github.com/bling/dotemacs
(package-initialize)

;; Install packages.
(add-to-list 'load-path "~/.emacs.d/lisp")
(defvar pkgs '(evil
               evil-leader
               evil-numbers
               evil-visualstar
               evil-nerd-commenter
               editorconfig
               evil-indent-textobject
               evil-matchit
               scss-mode ;; mode for the Sass language
               pophint
               ag ;; the silver searcher
               rainbow-identifiers ;; rainbows!
               dired-rainbow
               dired-subtree
               dired-k
               rainbow-blocks ;; omg more rainbows
               icicles 
               flycheck-rust ;; flycheck for the Rust language
               rust-mode ;; mode for the Rust language
               zenburn-theme ;; Zenburn theme
               surround ;; Delete surrounding characters (`()', `[]', etc.).
               auto-complete ;; autocompletion
               highlight-numbers ;; rainbowify numbers
               todotxt ;; Mode for the todo.txt markup
               magit ;; git integration
               table ;; tables!
               ac-dcd ;; D Completion Daemon source for autocomplete
               mediawiki ;; mediawiki client
               wgrep-ag ;; writable grep, but for ag
               notmuch ;; notmuch MUA integration
               racket-mode ;; mode for the Racket 
               undo-tree ;; vim-like undo tree
               ac-haskell-process ;; autocomplete for the Haskell language
               projectile ;; project management
               smartparens ;; automatically insert parenthesis
               browse-kill-ring ;; menu for the killring
               emacs-eclim ;; turn emacs into an even more IDEer thing using eclim!
               coffee-mode ;; mode for the CoffeeScript language
               git-gutter ;; Git status in left fringe
               markdown-mode ;; mode for the Markdown markup
               indent-guide ;; a "ruler" for indentation
               rainbow-delimiters ;; RAINNNNNNNNNNBOOOOWWZZ
               php-mode ;; mode for the PHP language
               helm-projectile ;; projectile integration for helm
               perspective ;; basically tabs
               smart-mode-line ;; a nice mode line
               langtool ;; Language linting
               yasnippet ;; snippets
               helm ;; menus for ALL the things
               flycheck ;; on the fly syntax checking
               haskell-mode ;; mode for Haskell
               ruby-mode ;; mode for the Ruby language
               clojure-mode ;; mode for the Clojure language
               cider ;; REPL for Clojure
               ac-cider ;; autocomplete for CIDER
               lua-mode ;; mode for the Lua language
               ctags
               ace-jump-mode ;; easymotion
               d-mode ;; mode for the D language
               ac-emmet ;; a mode for efficient production of HTML and XML
               web-mode
               ghc 
               ghci-completion
               cmake-mode
               julia-mode
               langtool
               slim-mode
               io-mode))

(require 'cl)
(loop for pkg in pkgs do
      (require-package pkg))
(require 'langtool)
(setq langtool-language-tool-jar "/home/zack/LanguageTool-2.6/languagetool-commandline.jar")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-ispell-fuzzy-limit 1)
 '(custom-safe-themes
   (quote
    ("756597b162f1be60a12dbd52bab71d40d6a2845a3e3c2584c6573ee9c332a66e" "6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default)))
 '(delete-selection-mode nil)
 '(eclim-eclipse-dirs (quote ("~/eclipse/eclipse")))
 '(eclim-executable "~/eclipse/eclipse/eclim")
 '(inhibit-startup-screen t)
 '(mark-even-if-inactive t)
 '(mediawiki-site-alist
   (quote
    (("http://wiki.apertron.net" "http://wiki.apertron.net/" "zackp30" nil "Main Page")
     ("Wikipedia" "http://en.wikipedia.org/w/" "username" "password" "Main Page"))))
 '(socks-server (quote ("Default server" "localhost" 9001 5)))
 '(transient-mark-mode 1))

(setq ido-enable-flex-matching t)



;; Misc requires
(require 'ac-ispell)
(require 'notmuch)
(require 'pophint)
(define-key global-map (kbd "C-'") 'pophint:do-flexibly)
(require 'icicles)
(require 'smartparens-config)
(require 'htmlize)
(require 'indent-guide)
(require 'editorconfig)
(require 'ido)
(require 'auto-complete)
(require 'password-store)
(require 'ctags)
(require 'mediawiki)
(require 'todotxt)
(require 'ac-dcd)
(require 'auto-complete-config)
(require 'smart-mode-line)
(require 'helm-config)
(require 'ruby-mode)
(require 'ac-cider)
(require 'ac-emmet)
(require 'emmet-mode)
(require 'io-mode)

(require 'cmake-mode)

(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook 'emmet-mode)
(add-hook 'sgml-mode-hook 'ac-emmet-html-setup)
(add-hook 'css-mode-hook 'ac-emmet-css-setup)
(require 'tramp)
(require 'whitespace)
(add-hook 'cider-mode-hook 'ac-flyspell-workaround)
(add-hook 'cider-mode-hook 'ac-cider-setup)
(add-hook 'cider-repl-mode-hook 'ac-cider-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'cider-mode))


;; Misc settings
(evilnc-default-hotkeys)
(setq flycheck-check-syntax-automatically '(save mode-enabled))
(setq flycheck-highlighting-mode 'symbols)
(setq flycheck-indication-mode 'left-fringe)
(menu-bar-mode -1) ;; disabe menubar
(tool-bar-mode -1) ;; disable toolbar
(setq org-mobile-directory "/home/zack/orgstuff/")
(scroll-bar-mode -1) ;; disable scrollbar
(global-linum-mode 1) ;; enable line numbers
(sml/setup) ;; modeline setup
(sml/apply-theme 'dark) ;; dark modeline
(setq ac-auto-show-menu t)
(setq ac-auto-start t)
(setq ac-delay 0.1)
(setq ac-quick-help-delay 0.3)
(setq ac-quick-help-height 30)
(setq ac-show-menu-immediately-on-auto-complete t)
(ac-config-default)
(setq ctags-command "/usr/bin/ctags-exuberant -e -R ")
(setq vc-follow-symlinks t)
(global-set-key (kbd "C-x v p") 'git-messenger:popup-message)
;; Haskell!
(autoload 'ghc-init "ghc" nil t)
(add-hook 'haskell-mode-hook 'ghc-init)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'dired 'dired-k)
(define-key dired-mode-map (kbd "C-f") 'dired-k)


;; Enable markdown-mode for .txt, .markdown, and .md
(add-to-list 'auto-mode-alist 
             '("\\.txt\\'" . markdown-mode)) 
(add-to-list 'auto-mode-alist 
             '("CMakeLists.txt" . cmake-mode)) 
(add-to-list 'auto-mode-alist 
             '("\\.markdown\\'" . markdown-mode)) 
(add-to-list 'auto-mode-alist 
             '("\\.md\\'" . markdown-mode))

(add-to-list 'auto-mode-alist 
             '("\\.mw\\'" . mediawiki-mode))

(add-to-list 'auto-mode-alist
             '("\\Gemfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist
             '("\\Guardfile\\'" . ruby-mode))

(add-to-list 'auto-mode-alist
             '("\\Rakefile\\'" . ruby-mode))

(add-hook 'd-mode-hook 'ac-dcd-setup)


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
(ido-mode t)
(projectile-global-mode)
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(eval-after-load "evil"
  '(define-key evil-insert-state-map (kbd "C-k") 'yas-expand))

(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

(yas-global-mode 1)
(indent-guide-global-mode)
(helm-mode 1)
(global-git-gutter-mode 1)
(smartparens-global-mode t)
(evil-mode t) ;; Vim!
(global-surround-mode t)
(global-evil-leader-mode)
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

(ac-flyspell-workaround)

(require 'org)
(define-key global-map (kbd "C-c l") 'org-store-link)
(define-key global-map (kbd "C-c a") 'org-agenda)
(setq org-log-done t)

;; Hooks
(add-hook 'git-commit-mode-hook 'ac-ispell-ac-setup)
(add-hook 'mail-mode-hook 'ac-ispell-ac-setup)
(add-hook 'markdown-mode-hook 'ac-ispell-ac-setup)
(eval-after-load "auto-complete"
  '(progn
     (ac-ispell-setup)))
(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'prog-mode-hook  'hs-minor-mode)
(add-hook 'prog-mode-hook  'flyspell-prog-mode)
(add-hook 'text-mode-hook  'flyspell-mode)
(add-hook 'haskell-mode-hook 'hs-minor-mode)
(add-hook 'python-mode-hook 'hs-minor-mode)
(add-hook 'ruby-mode-hook 'hs-minor-mode)




;; Yay zenburn.
(load-theme 'zenburn t)

;; Key bindings
;;(global-set-key (kbd "C-TAB") )
(global-set-key (kbd "C-c h") 'helm-projectile)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(evil-leader/set-leader "<SPC>") ;; space is my leader
(evil-leader/set-key
  "p b" 'projectile-switch-to-buffer
  "p D" 'projectile-dired
  "p d" 'projectile-find-dir
  "p s" 'projectile-switch-project
  "p R" 'projectile-regenerate-tags
  "p j" 'projectile-find-tag
  "g t r" 'ctags-create-or-update-tags-table
  "f" 'ido-find-file
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

(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
(setq scss-compile-at-save nil)

(require 'langtool)
(setq langtool-language-tool-jar "~/LanguageTool-2.6/languagetool-commandline.jar")


(add-hook 'prog-mode-hook 'highlight-numbers-mode)
(add-hook 'prog-mode-hook 'rainbow-identifiers-mode)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)


(evil-define-key 'normal prog-mode-map (kbd "U") 'undo-tree-visualize)
(evil-define-key 'normal text-mode-map (kbd "U") 'undo-tree-visualize)
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ejs\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))


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

(add-to-list 'auto-mode-alist '("\\todo.txt\\'" . todotxt-mode))

(defun replace-with-comma ()
  (interactive)
  (delete-forward-char 1)
  (insert ",")
  (evil-append 1))

(require 'ag)

(require 'hl-anything)
(hl-paren-mode 1)

(define-key ag-mode-map (kbd "k") nil) ;; stop conflicts with evil

(add-hook 'interactive-haskell-mode-hook 'ac-haskell-process-setup)
(add-hook 'haskell-interactive-mode-hook 'ac-haskell-process-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'haskell-interactive-mode))
(provide 'init)
;;; init.el ends here
