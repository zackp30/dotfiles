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
               workgroups2
               auto-complete-clang
               rainbow-identifiers ;; rainbows!
               rainbow-blocks ;; omg more rainbows
               flycheck-rust ;; flycheck for the Rust language
               rust-mode ;; mode for the Rust language
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
               ac-dcd ;; D Completion Daemon source for autocomplete
               mediawiki ;; mediawiki client
               wgrep-ag ;; writable grep, but for ag
               notmuch ;; notmuch MUA integration
               racket-mode ;; mode for the Racket 
               undo-tree ;; vim-like undo tree
               hydra ;; micro-states!
               hy-mode
               ac-haskell-process ;; autocomplete for the Haskell language
               projectile ;; project management
               jedi ;; python auto-completion
               smartparens ;; automatically insert parenthesis
               helm-swoop
               ein
               bookmark+
               browse-kill-ring ;; menu for the killring
               emacs-eclim ;; turn emacs into an even more IDEer thing using eclim!
               coffee-mode ;; mode for the CoffeeScript language
               git-gutter ;; Git status in left fringe
               markdown-mode ;; mode for the Markdown markup
               indent-guide ;; a "ruler" for indentation
               rainbow-delimiters ;; RAINNNNNNNNNNBOOOOWWZZ
               php-mode ;; mode for the PHP language
               nim-mode
               ac-nim
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
               ac-cider ;; autocomplete for CIDER
               lua-mode ;; mode for the Lua language
               ctags
               ledger-mode
               flycheck-ledger
               ac-cider
               ace-jump-mode ;; easymotion
               ace-window
               d-mode ;; mode for the D language
               ac-emmet ;; a mode for efficient production of HTML and XML
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
               ac-slime
               io-mode))

(loop for pkg in pkgs do
      (require-package pkg))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-ispell-fuzzy-limit 1)
 '(bmkp-last-as-first-bookmark-file "/home/zack/.emacs.d/bookmarks")
 '(custom-safe-themes
   (quote
    ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "756597b162f1be60a12dbd52bab71d40d6a2845a3e3c2584c6573ee9c332a66e" "6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default)))
 '(delete-selection-mode nil)
 '(eclim-eclipse-dirs (quote ("~/eclipse/eclipse")))
 '(eclim-executable "~/eclipse/eclipse/eclim")
 '(global-hl-line-mode t)
 '(global-hl-line-sticky-flag t)
 '(indent-guide-char "│")
 '(inhibit-startup-screen t)
 '(mark-even-if-inactive t)
 '(mediawiki-site-alist
   (quote
    (("http://wiki.apertron.net" "http://wiki.apertron.net/" "zackp30" nil "Main Page")
     ("Wikipedia" "http://en.wikipedia.org/w/" "username" "password" "Main Page"))))
 '(sml/full-mode-string " ...")
 '(sml/show-client t)
 '(sml/theme (quote dark))
 '(socks-server (quote ("Default server" "localhost" 9001 5)))
 '(transient-mark-mode 1))


;; Misc requires
(require 'ac-ispell)
(require 'notmuch)
(require 'pophint)
(define-key global-map (kbd "C-'") 'pophint:do-flexibly)
(require 'smartparens-config)
(require 'htmlize)
(require 'indent-guide)
(require 'editorconfig)
(require 'bookmark+)
(require 'bitbake)
(require 'auto-complete)
(require 'ctags)
(require 'mediawiki)
(require 'ac-cider)
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

(add-hook 'web-mode-hook 'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook 'emmet-mode)
(add-hook 'web-mode-hook 'ac-emmet-html-setup)
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

;; Haskell!
(autoload 'ghc-init "ghc" nil t)
(add-hook 'haskell-mode-hook 'ghc-init)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

(defun get-rnd-list (lst)
  "Get a random item from a list."
  (nth (random* (length lst)) lst))

(defun random-color ()
  "Get a random color."
  (get-rnd-list '("blue" "red" "yellow" "pink")))

(defun a-mode (ext mode)
  "A 'shortcut' for `(add-to-list 'auto-mode-alist [...])`'"
  (add-to-list 'auto-mode-alist
               (cons
                (format "\\%s\\'" ext)
                (intern (concat mode "-mode")))))


(add-to-list 'auto-mode-alist 
             '("CMakeLists.txt" . cmake-mode)) 

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
(add-hook 'cider-mode-hook 'ac-flyspell-workaround)
(add-hook 'cider-mode-hook 'ac-cider-setup)
(add-hook 'cider-repl-mode-hook 'ac-cider-setup)
(eval-after-load 'nim-mode '(add-hook 'nim-mode-hook 'ac-nim-enable))
(eval-after-load "auto-complete"
  '(progn
     (add-to-list 'ac-modes 'cider-mode)
          (add-to-list 'ac-modes 'cider-repl-mode)))
(setq ac-fuzzy-enable 1)
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

(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

(global-auto-complete-mode t)


(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'slime-repl-mode))

(setq slime-contribs '(slime-fancy))
(setq inferior-lisp-program "sbcl")

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

(evil-define-key 'normal global-map (kbd "}]") 'emmet-next-edit-point)
(evil-define-key 'normal global-map (kbd "{[") 'emmet-prev-edit-point)
(evil-define-key 'normal global-map (kbd "U") 'undo-tree-visualize)
(require 'web-mode)

(a-mode ".phtml" "web")
(a-mode ".tpl\\.php" "web")
(a-mode ".[agj]sp" "web")
(a-mode ".as[cp]x" "web")
(a-mode ".erb" "web")
(a-mode ".mustache" "web")
(a-mode ".djhtml" "web")
(a-mode ".ejs" "web")
(a-mode ".html?" "web")

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

(add-hook 'interactive-haskell-mode-hook 'ac-haskell-process-setup)
(add-hook 'haskell-interactive-mode-hook 'ac-haskell-process-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'haskell-interactive-mode))
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
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(sourcegraph-mode 1)

(eval-after-load 'flycheck '(require 'flycheck-ledger))

(evil-define-key 'normal evil-snipe-mode-map "zA" 'evil-snipe-f)
(evil-define-key 'normal evil-snipe-mode-map "]S" 'flyspell-goto-next-error)

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

(require 'zone)
(zone-when-idle 120)

(setq helm-display-header-line nil)
(set-face-attribute 'helm-source-header nil :height 0.1)

(helm-autoresize-mode 1)

(require 'magit-gitflow)
(add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)
(add-hook 'magit-mode-hook 'turn-on-magit-gitflow)

(provide 'init)
;;; init.el ends here
