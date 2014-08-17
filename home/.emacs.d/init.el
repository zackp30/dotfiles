;; A hacked together Emacs config.


;; Add repositories for package archives
(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
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

(require-package 'evil)
(require-package 'evil-leader)
(require-package 'evil-numbers)
(require-package 'evil-visualstar)
(require-package 'evil-nerd-commenter)
(require-package 'evil-indent-textobject)
(require-package 'evil-matchit)
(require-package 'color-theme-solarized)
(require-package 'surround)
(require-package 'auto-complete)
(require-package 'magit)
(require-package 'undo-tree)
(require-package 'projectile)
(require-package 'smartparens)
(require-package 'browse-kill-ring)
(require-package 'emacs-eclim)
(require-package 'coffee-mode)
(require-package 'git-gutter)
(require-package 'markdown-mode)
(require-package 'indent-guide)
(require-package 'rainbow-delimiters)
(require-package 'helm-projectile)
(require-package 'smex)
(require-package 'perspective)
(require-package 'smart-mode-line)
(require-package 'yasnippet)
(require-package 'helm)
(require-package 'flycheck)
(require-package 'haskell-mode)
(require-package 'ruby-mode)
(require-package 'clojure-mode)
(require-package 'cider)
(require-package 'ac-cider)
(require-package 'mediawiki)
(require-package 'lua-mode)




(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-ispell-fuzzy-limit 1)
 '(ac-ispell-requires 1)
 '(custom-safe-themes (quote ("756597b162f1be60a12dbd52bab71d40d6a2845a3e3c2584c6573ee9c332a66e" "6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default)))
 '(delete-selection-mode nil)
 '(eclim-eclipse-dirs (quote ("~/eclipse/eclipse")))
 '(eclim-executable "~/eclipse/eclipse/eclim")
 '(inhibit-startup-screen t)
 '(mark-even-if-inactive t)
 '(scroll-bar-mode (quote right))
 '(transient-mark-mode 1))






(setq ido-enable-flex-matching t)

(require-package 'ac-ispell)



;; Misc requires
(require 'smartparens-config)
(require 'indent-guide)
(require 'ido)
(require 'ac-emacs-eclim-source)
(require 'auto-complete)
(require 'auto-complete-config)
(require 'smart-mode-line)
(require 'helm-config)
(require 'ruby-mode)
(require 'ac-cider)
(require 'tramp)
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
(setq ac-quick-help-delay 0.3)
(setq ac-quick-help-height 30)
(setq ac-show-menu-immediately-on-auto-complete t)
(ac-config-default)
(setq vc-follow-symlinks t)




(setq ac-sources '(ac-source-yasnippet ac-source-eclim))
(ac-emacs-eclim-config)

;; Enable markdown-mode for .txt, .markdown, and .md
(add-to-list 'auto-mode-alist 
             '("\\.txt\\'" . markdown-mode)) 
(add-to-list 'auto-mode-alist 
             '("\\.markdown\\'" . markdown-mode)) 
(add-to-list 'auto-mode-alist 
             '("\\.md\\'" . markdown-mode))

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
;; ALL the modes!
(ido-mode t)
(projectile-global-mode)
(yas-global-mode 1)
(indent-guide-global-mode)
(helm-mode 1)
(global-git-gutter-mode 1)
(smartparens-global-mode t)
(evil-mode t) ;; Vim!
(global-surround-mode t)
(global-evil-leader-mode)
(smex-initialize)
(persp-mode)
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)


;; Hooks
(add-hook 'git-commit-mode-hook 'ac-ispell-ac-setup)
(add-hook 'mail-mode-hook 'ac-ispell-ac-setupa)
(add-hook 'markdown-mode-hook 'ac-ispell-ac-setupa)
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




;; Yay solarized.
(load-theme 'solarized-dark)

;; Key bindings
(global-set-key (kbd "C-c h") 'helm-projectile)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(evil-leader/set-leader "<SPC>") ;; space is my leader
(evil-leader/set-key
  "p b" 'projectile-switch-to-buffer
  "p D" 'projectile-dired
  "p d" 'projectile-find-dir
  "p s" 'projectile-switch-project
  "p R" 'projectile-regenerate-tags
  "p j" 'projectile-find-tag
  "f" 'ido-find-file
)

(setq list-command-history-max 500)
(setq-default indent-tabs-mode nil)
(setq tab-width 4)
