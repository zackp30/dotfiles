(package-initialize)
(add-to-list 'load-path "~/.emacs.d/lisp")
(require 'org)
(require 'htmlize)

(setq vc-follow-symlinks t)

;; Server handling
;; I use multiple Emacs servers, and had a problem where loading `config.org'
;; and tangling it into `config.el' ended up causing a lock.
;; So I went on the quest to make my own system to run them one at a time.
;; This involved:
;; 1. Running the servers in a contained manner (tmux!).
;; 2. Wait for a PID file to appear, which will happen a few seconds after `after-init-hook'
;; 3. Make Emacs servers depend on each other, for example:
;;   - fb-mapper depends on documents which depends on dotfiles which depends on server
;;
;; This system works really well.

(defun shell-without-newline (s)
  "Remove the newline from the output of a shell command.
S: shell command to run"
  (replace-regexp-in-string "\n$" ""
                            (shell-command-to-string s)))
(defvar project-name (getenv "PROJECT_NAME")
  "Get the project that Emacs is running within.")

(defvar pid-dir (concat "/tmp/emacs" (number-to-string (user-uid)) "/ready/")
  "Where the PIDs are stored.")
(setq server-name project-name)
(defvar pid-file (concat pid-dir server-name)
  "Where the PID file is.")

;; Here we determine whether we're about to conflict with another Emacs server.
(when (string= "yes" (shell-without-newline (concat
                                             "isemacsrunning "
                                             server-name
                                             " "
                                             (number-to-string 1))))
  (progn (error (concat "Emacs server (" server-name ") is already running, exiting"))
         (kill-emacs)))

(when (not server-mode)
  (server-start))


(defun init-pid ()
  "saves a PID file"
  (when (file-exists-p pid-file)
    (delete-file pid-file))
  (when (not (file-exists-p pid-dir))
    (make-directory pid-dir t))
  (when (file-exists-p pid-file)
    (add-hook 'kill-emacs-hook
              (lambda ()
                (delete-file (concat "/tmp/emacs" (number-to-string (user-uid)) "/ready/" server-name)))))
  (append-to-file (number-to-string (emacs-pid)) nil pid-file))
(add-hook 'after-init-hook 'init-pid)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default)))
 '(indent-guide-char "│")
 '(org-agenda-files
   (quote
    ("~/.homesick/repos/dotfiles/home/.config/awesome/rc.org")))
 '(package-selected-packages
   (quote
    (material-theme yasnippet yaml-mode ws-butler workgroups2 wgrep-ag wgrep web-mode wanderlust todotxt sx surround spinner smart-mode-line slime-company slime slim-mode scss-mode rust-mode rainbow-identifiers rainbow-delimiters rainbow-blocks racket-mode perspective ocodo-svg-modelines nim-mode mmm-mode mediawiki zenburn-theme markdown-mode magit-tramp magit lua-mode lentic ledger-mode julia-mode js2-mode io-mode indent-guide ibuffer-vc hydra hy-mode highlight-numbers helm-swoop helm-projectile helm-ag go-mode gnuplot-mode gitignore-mode gitconfig-mode git-timemachine git-gutter git-commit-mode gist ghci-completion ggtags flycheck-rust flycheck-ledger flycheck evil-visualstar evil-numbers evil-nerd-commenter evil-matchit evil-leader evil-indent-textobject evil etags-select emmet-mode emacs-eclim elixir-mode ein editorconfig edit-server dired-toggle-sudo dired-rainbow dired+ d-mode ctags company-tern company-ghc company-anaconda company coffee-mode cmake-mode cider browse-kill-ring bookmark+ ag ace-window ace-jump-helm-line ace-flyspell use-package)))
 '(persp-keymap-prefix "w"))

(require 'ob-tangle)
(org-babel-load-file "~/.emacs.d/config.org")
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(load-theme 'material t)
