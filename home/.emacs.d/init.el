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
(require-package 'powerline)
(require-package 'undo-tree)
(require-package 'projectile)
(projectile-global-mode)
(require-package 'yasnippet)

(yas-global-mode 1)
(require 'auto-complete)
(require 'auto-complete-config)
(powerline-center-evil-theme)
(defun my-evil-modeline-change (default-color)
  "changes the modeline color when the evil mode changes"
  (let ((color (cond ((evil-insert-state-p) '("#002233" . "#ffffff"))
                     ((evil-visual-state-p) '("#330022" . "#ffffff"))
                     ((evil-normal-state-p) default-color)
                     (t '("#440000" . "#ffffff")))))
    (set-face-background 'mode-line (car color))
    (set-face-foreground 'mode-line (cdr color))))

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


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default)))
 '(eclim-eclipse-dirs (quote ("~/eclipse/eclipse")))
 '(eclim-executable "~/eclipse/eclipse/eclim"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq flycheck-check-syntax-automatically '(save mode-enabled))
(setq flycheck-highlighting-mode 'symbols)
(setq flycheck-indication-mode 'left-fringe)
(menu-bar-mode -1)
(global-linum-mode 1)

(require-package 'markdown-mode)
(add-to-list 'auto-mode-alist 
	     '("\\.text\\'" . markdown-mode)) 
(add-to-list 'auto-mode-alist 
	     '("\\.markdown\\'" . markdown-mode)) 
(add-to-list 'auto-mode-alist 
	     '("\\.md\\'" . markdown-mode))




(require-package 'browse-kill-ring)
(require-package 'emacs-eclim)


(setq ac-sources '(ac-source-yasnippet ac-source-eclim))

(require 'ac-emacs-eclim-source)
(ac-emacs-eclim-config)



(require-package 'git-gutter)

(require-package 'rainbow-delimiters)



(global-git-gutter-mode +1)
