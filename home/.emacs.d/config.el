#+begin_src emacs-lisp :tangle yes
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
