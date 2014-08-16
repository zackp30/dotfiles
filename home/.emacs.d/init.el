;; A hacked together Emacs config.
(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))
(setq package-enable-at-startup nil)

(defun require-package (package)
  "Install given PACKAGE."
  (unless (package-installed-p package)
    (unless (assoc package package-archive-contents)
      (package-refresh-contents))
    (package-install package))) ;; https://github.com/bling/dotemacs
(package-initialize)
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
(require 'smartparens-config)
(projectile-global-mode)
(require-package 'yasnippet)

(yas-global-mode 1)
(require 'auto-complete)
(require 'auto-complete-config)

(require-package 'smart-mode-line)
(require 'smart-mode-line)
(sml/setup)
(sml/apply-theme 'dark)
(setq ac-auto-show-menu t)
(setq ac-auto-start t)
(setq ac-quick-help-delay 0.3)
(setq ac-quick-help-height 30)
(setq ac-show-menu-immediately-on-auto-complete t)
(ac-config-default)
(evil-mode t)
(global-surround-mode t)
(global-evil-leader-mode)
(load-theme 'solarized-dark)
(require-package 'helm)
(require 'helm-config)
(require-package 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'lisp-mode-hook 'hs-minor-mode)
(add-hook 'haskell-mode-hook 'hs-minor-mode)
(add-hook 'python-mode-hook 'hs-minor-mode)
(add-hook 'ruby-mode-hook 'hs-minor-mode)



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
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq flycheck-check-syntax-automatically '(save mode-enabled))
(setq flycheck-highlighting-mode 'symbols)
(setq flycheck-indication-mode 'left-fringe)
(menu-bar-mode -1) ;; disabe menubar
(tool-bar-mode -1) ;; disable toolbar
(scroll-bar-mode -1) ;; disable scrollbar
(global-linum-mode 1) ;; enable line numbers

(require-package 'markdown-mode)
(add-to-list 'auto-mode-alist 
             '("\\.text\\'" . markdown-mode)) 
(add-to-list 'auto-mode-alist 
             '("\\.markdown\\'" . markdown-mode)) 
(add-to-list 'auto-mode-alist 
             '("\\.md\\'" . markdown-mode))



(evil-leader/set-key
  "p b" 'projectile-switch-to-buffer
  "p D" 'projectile-dired
  "p d" 'projectile-find-dir
  "p s" 'projectile-switch-project
  "p R" 'projectile-regenerate-tags
  "p j" 'projectile-find-tag
  "f" 'ido
)



(setq ac-sources '(ac-source-yasnippet ac-source-eclim))

(require 'ac-emacs-eclim-source)
(ac-emacs-eclim-config)
(require-package 'helm-projectile)

(global-set-key (kbd "C-c h") 'helm-projectile)
(require-package 'perspective)
(persp-mode)

(require-package 'git-gutter)

(require-package 'rainbow-delimiters)


(global-git-gutter-mode 1)
(smartparens-global-mode t)

(require-package 'smex)
(smex-initialize)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)


(require-package 'indent-guide)
(require 'indent-guide)
(indent-guide-global-mode)

(require 'ido) ;; ido! (wow very descriptive comment...)
(ido-mode t)

(setq ido-enable-flex-matching t)

(require-package 'ac-ispell)



(eval-after-load "auto-complete"
  '(progn
     (ac-ispell-setup)))

(add-hook 'git-commit-mode-hook 'ac-ispell-ac-setup)
(add-hook 'mail-mode-hook 'ac-ispell-ac-setupa)
(add-hook 'markdown-mode-hook 'ac-ispell-ac-setupa)
(helm-mode 1)
