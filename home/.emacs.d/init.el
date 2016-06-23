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
;; 'org)` requires the built-in Emacs Org.

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
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks")
 '(custom-safe-themes
   (quote
    ("c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "26614652a4b3515b4bbbb9828d71e206cc249b67c9142c06239ed3418eff95e2" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default)))
 '(edit-server-url-major-mode-alist
   (quote
    (("b.6697.eu" . markdown-mode)
     ("news.ycombinator.com" . markdown-mode)
     ("reddit.com" . markdown-mode))))
 '(el-get-git-shallow-clone t)
 '(indent-guide-char "│")
 '(magit-revision-show-gravatars (quote ("^Author:     " . "^Commit:     ")))
 '(org-capture-templates
   (quote
    (("l" "Link and text" entry
      (file+headline "~/org/notes.org" "Links")
      "* [[%:link][%:description]]
Captured On: %U
#+BEGIN_QUOTE
%i
#+END_QUOTE

%?")
     ("w" "Web site" entry
      (file "~/org/notes.org")
      "* %a :website:

%U %?

%:initial")
     ("L" "Link" entry
      (file+headline "~/org/notes.org" "Links")
      "* %? [[%:link][%:description]]
Captured On: %U")
     ("z" "Work TODO item" entry
      (file+headline "~/org/work.org" "Work")
      "* TODO %^{Name} %^G
%?"))))
 '(org-emphasis-alist
   (quote
    (("*" bold)
     ("/" italic)
     ("_" underline)
     ("=" org-verbatim verbatim)
     ("~" org-code verbatim)
     ("+"
      (:strike-through t)))))
 '(org-latex-compiler "xelatex")
 '(org-latex-default-packages-alist
   (quote
    (("" "minted" t)
     ("AUTO" "inputenc" t
      ("pdflatex"))
     ("T1" "fontenc" t
      ("pdflatex"))
     ("" "graphicx" t)
     ("" "grffile" t)
     ("" "longtable" nil)
     ("" "wrapfig" nil)
     ("" "rotating" nil)
     ("normalem" "ulem" t)
     ("" "amsmath" t)
     ("" "textcomp" t)
     ("" "amssymb" t)
     ("" "fontspec" t)
     ("" "capt-of" nil)
     ("" "hyperref" nil))))
 '(org-priority-faces (quote ((67 . "green") (66 . "yellow") (65 . "red"))))
 '(package-selected-packages
   (quote
    (spaceline highlight yaml-mode ws-butler which-key wgrep-ag web-mode wanderlust use-package tuareg todotxt tiny surround stumpwm-mode smart-mode-line slime-company slim-mode scss-mode rust-mode rinari realgud rainbow-identifiers rainbow-delimiters rainbow-blocks racket-mode purescript-mode puml-mode projectile-rails org-bullets ocodo-svg-modelines nim-mode mode-icons mmm-mode material-theme markdown-mode magit-tramp lua-mode lentic julia-mode js2-mode io-mode indent-guide ibuffer-vc hydra hy-mode highlight-numbers highlight-indentation helm-swoop helm-projectile helm-ag graphviz-dot-mode go-mode gnuplot-mode gnuplot gitignore-mode gitconfig-mode git-timemachine git-gutter gist ghci-completion geiser fountain-mode flycheck-rust evil-visualstar evil-space evil-numbers evil-nerd-commenter evil-matchit evil-leader evil-indent-textobject evil-args evil-anzu etags-select emojify emmet-mode emacs-eclim elixir-mode elfeed el-get ein editorconfig edit-server dired-toggle-sudo dired-rainbow dired+ d-mode ctags company-tern company-ghc company-anaconda coffee-mode cmake-mode cider calfw browse-kill-ring bpr boxquote bookmark+ beacon bbdb ag ace-window ace-jump-helm-line ace-flyspell)))
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
 '(markdown-markup-face ((t (:inherit shadow :foreground "white smoke" :slant normal :weight extra-bold))))
 '(mode-line ((t nil)))
 '(org-block-begin-line ((t (:background "LavenderBlush4" :foreground "white" :box (:line-width 2 :color "grey75" :style released-button)))))
 '(org-block-end-line ((t (:inherit org-block-begin-line :foreground "white"))))
 '(org-document-title ((t (:inherit org-level-1 :foreground "#8CD0D3" :box nil :underline nil :height 2.0)))))

(server-start)

(load-theme 'zenburn t)
