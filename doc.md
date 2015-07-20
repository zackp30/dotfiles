<div id="table-of-contents">
<h2>Table of Contents</h2>
<div id="text-table-of-contents">
<ul>
<li><a href="#orgheadline85">1. Emacs configuration</a>
<ul>
<li><a href="#orgheadline1">1.1. Commentary</a></li>
<li><a href="#orgheadline2">1.2. Package archives</a></li>
<li><a href="#orgheadline3">1.3. Local files</a></li>
<li><a href="#orgheadline4">1.4. <code>require-package</code></a></li>
<li><a href="#orgheadline5">1.5. List of packages</a></li>
<li><a href="#orgheadline6">1.6. use-package</a></li>
<li><a href="#orgheadline7">1.7. a-mode</a></li>
<li><a href="#orgheadline11">1.8. Org mode</a>
<ul>
<li><a href="#orgheadline8">1.8.1. Babel</a></li>
<li><a href="#orgheadline9">1.8.2. Misc. settings</a></li>
<li><a href="#orgheadline10">1.8.3. Indentation</a></li>
</ul>
</li>
<li><a href="#orgheadline14">1.9. Flycheck</a>
<ul>
<li><a href="#orgheadline12">1.9.1. Flyspell</a></li>
<li><a href="#orgheadline13">1.9.2. Load-path inheritance</a></li>
</ul>
</li>
<li><a href="#orgheadline16">1.10. Emmet</a>
<ul>
<li><a href="#orgheadline15">1.10.1. <code>turn-on-emmet-mode</code></a></li>
</ul>
</li>
<li><a href="#orgheadline20">1.11. ace-jump</a>
<ul>
<li><a href="#orgheadline17">1.11.1. Flyspell</a></li>
<li><a href="#orgheadline18">1.11.2. Helm</a></li>
<li><a href="#orgheadline19">1.11.3. Windows</a></li>
</ul>
</li>
<li><a href="#orgheadline21">1.12. Magit</a></li>
<li><a href="#orgheadline24">1.13. Auto completion</a>
<ul>
<li><a href="#orgheadline22">1.13.1. Anaconda</a></li>
<li><a href="#orgheadline23">1.13.2. Haskell</a></li>
</ul>
</li>
<li><a href="#orgheadline25">1.14. edit-server</a></li>
<li><a href="#orgheadline33">1.15. EVIL</a>
<ul>
<li><a href="#orgheadline26">1.15.1. Text-object delimiters</a></li>
<li><a href="#orgheadline27">1.15.2. NERD-commenter</a></li>
<li><a href="#orgheadline29">1.15.3. Leader</a></li>
<li><a href="#orgheadline30">1.15.4. Modeline color changing</a></li>
<li><a href="#orgheadline31">1.15.5. Cursor changing</a></li>
<li><a href="#orgheadline32">1.15.6. Matching</a></li>
</ul>
</li>
<li><a href="#orgheadline34">1.16. GNU Global</a></li>
<li><a href="#orgheadline35">1.17. Git-gutter</a></li>
<li><a href="#orgheadline39">1.18. Helm</a>
<ul>
<li><a href="#orgheadline36">1.18.1. Projectile</a></li>
<li><a href="#orgheadline37">1.18.2. Helm-M-x</a></li>
<li><a href="#orgheadline38">1.18.3. Helm-swoop</a></li>
</ul>
</li>
<li><a href="#orgheadline54">1.19. Languages</a>
<ul>
<li><a href="#orgheadline40">1.19.1. SCSS</a></li>
<li><a href="#orgheadline42">1.19.2. Common Lisp</a></li>
<li><a href="#orgheadline43">1.19.3. Clojure</a></li>
<li><a href="#orgheadline44">1.19.4. Haskell</a></li>
<li><a href="#orgheadline45">1.19.5. Cider</a></li>
<li><a href="#orgheadline47">1.19.6. JavaScript</a></li>
<li><a href="#orgheadline48">1.19.7. Web</a></li>
<li><a href="#orgheadline49">1.19.8. Gitolite</a></li>
<li><a href="#orgheadline50">1.19.9. GDScript</a></li>
<li><a href="#orgheadline51">1.19.10. CMake</a></li>
<li><a href="#orgheadline52">1.19.11. VisualBasic</a></li>
<li><a href="#orgheadline53">1.19.12. Scheme</a></li>
</ul>
</li>
<li><a href="#orgheadline56">1.20. ibuffer</a>
<ul>
<li><a href="#orgheadline55">1.20.1. VC</a></li>
</ul>
</li>
<li><a href="#orgheadline57">1.21. indent-guide</a></li>
<li><a href="#orgheadline58">1.22. Multiple-Major-Modes</a></li>
<li><a href="#orgheadline59">1.23. Projectile</a></li>
<li><a href="#orgheadline60">1.24. Smart-Mode-Line</a></li>
<li><a href="#orgheadline61">1.25. Ag</a></li>
<li><a href="#orgheadline62">1.26. ws-butler</a></li>
<li><a href="#orgheadline63">1.27. YASnippet</a></li>
<li><a href="#orgheadline65">1.28. Email</a>
<ul>
<li><a href="#orgheadline64">1.28.1. Wanderlust</a></li>
</ul>
</li>
<li><a href="#orgheadline66">1.29. undotree</a></li>
<li><a href="#orgheadline67">1.30. Rainbows</a></li>
<li><a href="#orgheadline68">1.31. imenu</a></li>
<li><a href="#orgheadline70">1.32. Misc</a>
<ul>
<li><a href="#orgheadline69">1.32.1. Backups</a></li>
</ul>
</li>
<li><a href="#orgheadline71">1.33. Utility functions</a></li>
<li><a href="#orgheadline77">1.34. Sort sexps</a>
<ul>
<li><a href="#orgheadline72">1.34.1. Get a random item from a list</a></li>
<li><a href="#orgheadline74">1.34.2. Get a random color</a></li>
<li><a href="#orgheadline75">1.34.3. Increment the number at point, like VIM's <code>C-a</code></a></li>
<li><a href="#orgheadline76">1.34.4. Decrement the number at point, like VIM's <code>C-x</code></a></li>
</ul>
</li>
<li><a href="#orgheadline78">1.35. Insert shell command</a></li>
<li><a href="#orgheadline79">1.36. History</a></li>
<li><a href="#orgheadline80">1.37. Eldoc</a></li>
<li><a href="#orgheadline81">1.38. The end</a></li>
<li><a href="#orgheadline84">1.39. Cascade startup system</a>
<ul>
<li><a href="#orgheadline82">1.39.1. <code>waitforemacs</code></a></li>
<li><a href="#orgheadline83">1.39.2. <code>emacsinotify</code></a></li>
</ul>
</li>
</ul>
</li>
<li><a href="#orgheadline120">2. Window manager (Awesome)</a>
<ul>
<li><a href="#orgheadline86">2.1. Requires</a></li>
<li><a href="#orgheadline89">2.2. Utility functions</a>
<ul>
<li><a href="#orgheadline88">2.2.1. <code>does_monitor_exist()</code></a></li>
</ul>
</li>
<li><a href="#orgheadline92">2.3. Error handling</a>
<ul>
<li><a href="#orgheadline90">2.3.1. Handle startup errors</a></li>
<li><a href="#orgheadline91">2.3.2. Handle runtime errors</a></li>
</ul>
</li>
<li><a href="#orgheadline93">2.4. Theme</a></li>
<li><a href="#orgheadline94">2.5. Adjust screens</a></li>
<li><a href="#orgheadline95">2.6. Miscellaneous variables</a></li>
<li><a href="#orgheadline97">2.7. Layouts</a>
<ul>
<li><a href="#orgheadline96">2.7.1. <span class="todo nilTODO">TODO</span> <code>[0/1]</code></a></li>
</ul>
</li>
<li><a href="#orgheadline98">2.8. Wallpaper</a></li>
<li><a href="#orgheadline99">2.9. Tags</a></li>
<li><a href="#orgheadline113">2.10. Widgets</a>
<ul>
<li><a href="#orgheadline100">2.10.1. Menu</a></li>
<li><a href="#orgheadline101">2.10.2. Clock</a></li>
<li><a href="#orgheadline102">2.10.3. CPU</a></li>
<li><a href="#orgheadline103">2.10.4. Initial variables</a></li>
<li><a href="#orgheadline104">2.10.5. Tag buttons</a></li>
<li><a href="#orgheadline105">2.10.6. Task bar buttons</a></li>
<li><a href="#orgheadline106">2.10.7. Task bar buttons (continued)</a></li>
<li><a href="#orgheadline107">2.10.8. Volume</a></li>
<li><a href="#orgheadline108">2.10.9. Layout</a></li>
<li><a href="#orgheadline109">2.10.10. Wibox</a></li>
<li><a href="#orgheadline110">2.10.11. Left aligned</a></li>
<li><a href="#orgheadline111">2.10.12. Bring it all together</a></li>
<li><a href="#orgheadline112">2.10.13. Next/prev tag</a></li>
</ul>
</li>
<li><a href="#orgheadline116">2.11. Keys</a>
<ul>
<li><a href="#orgheadline114">2.11.1. Initial table</a></li>
<li><a href="#orgheadline115">2.11.2. Pull it all together</a></li>
</ul>
</li>
<li><a href="#orgheadline117">2.12. Rules</a></li>
<li><a href="#orgheadline119">2.13. Signals</a>
<ul>
<li><a href="#orgheadline118">2.13.1. Hover focus</a></li>
</ul>
</li>
</ul>
</li>
<li><a href="#orgheadline146">3. zsh</a>
<ul>
<li><a href="#orgheadline121">3.1. Misc</a></li>
<li><a href="#orgheadline122">3.2. PATH</a></li>
<li><a href="#orgheadline123">3.3. Variables</a></li>
<li><a href="#orgheadline133">3.4. Functions</a>
<ul>
<li><a href="#orgheadline124">3.4.1. Detect command</a></li>
<li><a href="#orgheadline125">3.4.2. Run command</a></li>
<li><a href="#orgheadline126">3.4.3. <code>source_if_exists</code></a></li>
<li><a href="#orgheadline130">3.4.4. Emacs projects</a></li>
<li><a href="#orgheadline131">3.4.5. </a></li>
<li><a href="#orgheadline132">3.4.6. Load Minetest mods into config</a></li>
</ul>
</li>
<li><a href="#orgheadline134">3.5. <code>gpg-agent</code></a></li>
<li><a href="#orgheadline135">3.6. zkbd</a></li>
<li><a href="#orgheadline136">3.7. Source plugins</a></li>
<li><a href="#orgheadline137">3.8. <code>pyenv</code></a></li>
<li><a href="#orgheadline138">3.9. Completion</a></li>
<li><a href="#orgheadline139">3.10. key bindings</a></li>
<li><a href="#orgheadline140">3.11. Prompt</a></li>
<li><a href="#orgheadline141">3.12. FZF</a></li>
<li><a href="#orgheadline142">3.13. pprompt</a></li>
<li><a href="#orgheadline145">3.14. Aliases</a>
<ul>
<li><a href="#orgheadline143">3.14.1. <code>gcem</code></a></li>
<li><a href="#orgheadline144">3.14.2. <code>grmv</code></a></li>
</ul>
</li>
</ul>
</li>
<li><a href="#orgheadline156">4. tmux</a>
<ul>
<li><a href="#orgheadline147">4.1. Powerline</a></li>
<li><a href="#orgheadline148">4.2. Vi keys</a></li>
<li><a href="#orgheadline149">4.3. Bindings</a></li>
<li><a href="#orgheadline150">4.4. Settings</a></li>
<li><a href="#orgheadline151">4.5. Plugins</a></li>
<li><a href="#orgheadline155">4.6. tmuxinator</a>
<ul>
<li><a href="#orgheadline152">4.6.1. Emacs server</a></li>
<li><a href="#orgheadline153">4.6.2. School</a></li>
<li><a href="#orgheadline154">4.6.3. X11</a></li>
</ul>
</li>
</ul>
</li>
</ul>
</div>
</div>

The documentation for my dotfiles.

# Emacs configuration<a id="orgheadline85"></a>

## Commentary<a id="orgheadline1"></a>

Such commentary, wow.

    ;;; Commentary:
    ;; A hacked together Emacs config.
    ;;; Code:

## Package archives<a id="orgheadline2"></a>

I use the Org mode archive, MELPA, Marmalade and GNU ELPA.

    (setq package-archives '(("melpa" . "http://melpa.org/packages/")
                             ("org" . "http://orgmode.org/elpa/")
                             ("marmalade" . "http://marmalade-repo.org/packages/")
                             ("gnu" . "https://elpa.gnu.org/packages/")))
    (package-initialize)

## Local files<a id="orgheadline3"></a>

I sometimes have a file called \`local.el' which contains functions which have passwords (ERC), this is not checked into version control for obvious reasons.

    (if (file-exists-p "~/.local.el")
        (load "~/.local.el"))

## `require-package`<a id="orgheadline4"></a>

A utility function (borrowed from Bling's (<https://github.com/bling>)) configuration to install a package.

    (defun require-package (package)
      "Install given PACKAGE."
      (unless (package-installed-p package)
        (unless (assoc package package-archive-contents)
          (package-refresh-contents))
        (package-install package)))

## List of packages<a id="orgheadline5"></a>

This (huge) list contains each package I use, at the end each is passed to \`require-package' to download and install.

    (require 'cl)
    ;; Install packages.
    (defvar pkgs '(use-package
                    ace-flyspell ;; ace-jump-mode for flyspell
                    ace-jump-helm-line ;; use `ace-jump' to jump to a line in Helm.
                    ace-jump-mode ;; easymotion
                    ace-window ;; jump to a window using `ace-jump'.
                    ag ;; the silver searcher
                    bookmark+ ;; improved boomarks
                    browse-kill-ring ;; menu for the killring
                    cider ;; REPL for Clojure
                    clojure-mode ;; mode for the Clojure language
                    cmake-mode ;; mode for the CMake language
                    coffee-mode ;; mode for the CoffeeScript language
                    company ;; auto completion
                    company-anaconda ;; Python completion for company
                    company-ghc ;; Haskell completion for company
                    company-tern ;; completion for `tern-mode'.
                    ctags ;; ctags integration for Emacs
                    d-mode ;; mode for the D language
                    dired+ ;; extensions for Dired
                    dired-rainbow ;; RAINBOWS
                    projectile-rails
                    rinari ;; Rails
                    dired-toggle-sudo
                    edit-server ;; used by Edit With Emacs
                    editorconfig ;; make sure I conform to style guides
                    ein ;; iPython Notebook for Emacs
                    elixir-mode ;; major mode for Elixir
                    emacs-eclim ;; turn emacs into an even more IDEer thing using eclim!
                    emmet-mode ;; http://emmet.io implementation for Emacs
                    etags-select
                    evil ;; VIM for Emacs
                    evil-indent-textobject ;; indentation text object for Evil
                    evil-leader ;; VIM's leader key for Emacs
                    evil-matchit ;; `%' extensions in Evil
                    evil-nerd-commenter ;; efficent commenting for Evil
                    evil-numbers
                    evil-visualstar
                    flycheck ;; on the fly syntax checking
                    flycheck-rust ;; flycheck for the Rust language
                    ggtags ;; GNU Global
                    ghc
                    ghci-completion ;; GHC integration for company
                    gist ;; GitHub Gist integration for Emacs.
                    geiser ;; Racket, Chicken and Guile integration (REPLs, auto-completion) for Emacs.
                    git-gutter ;; Git status in left fringe
                    git-timemachine ;; rollback to previous revisions of a buffer
                    gitconfig-mode ;; major mode for `.gitconfig'
                    gitignore-mode ;; major mode for `.gitignore'
                    gnuplot-mode ;; major mode for gnuplot
                    go-mode ;; major mode for go
                    haskell-mode ;; major mode for Haskell
                    helm ;; menus for ALL the things
                    helm-ag ;; integration with Helm and `ag'
                    helm-projectile ;; projectile integration for helm
                    helm-swoop ;; grep-like tool for Helm
                    highlight-numbers ;; rainbowify numbers
                    hy-mode ;; hy mode
                    hydra ;; micro-states!
                    ibuffer-vc ;; ibuffer integration for vc.el
                    indent-guide ;; a "ruler" for indentation
                    io-mode ;; major mode for Io
                    js2-mode ;; major mode for JavaScript
                    julia-mode ;; mode for the Julia language
                    lentic
                    lua-mode ;; mode for the Lua language
                    magit ;; git integration
                    magit-tramp ;; TRAMP integration for Magit
                    markdown-mode ;; mode for the Markdown markup
                    zenburn-theme ;; Zenburn theme
                    mediawiki ;; mediawiki client
                    mmm-mode ;; Multiple Major Modes
                    nim-mode ;; major mode for Nim
                    ocodo-svg-modelines
                    perspective ;; basically tabs
                    projectile ;; project management
                    racket-mode ;; mode for the Racket
                    rainbow-blocks ;; omg more rainbows
                    rainbow-delimiters ;; RAINNNNNNNNNNBOOOOWWZZ
                    rainbow-identifiers ;; rainbows!
                    ruby-mode ;; mode for the Ruby language
                    rust-mode ;; mode for the Rust language
                    scss-mode ;; mode for the Sass language
                    slim-mode ;; mode for the Slim templating language
                    slime ;; REPL for Common Lisp
                    slime-company
                    smart-mode-line ;; a nice mode line
                    surround ;; Delete surrounding characters (`()', `[]', etc.).
                    table ;; tables!
                    todotxt ;; Mode for the todo.txt markup
                    undo-tree ;; vim-like undo tree
                    wanderlust ;; email
                    web-mode ;; mode for web stuff
                    wgrep ;; writable grep
                    wgrep-ag ;; writable grep, but for ag
                    workgroups2 ;; work groups for Emacs
                    ws-butler ;; whitespace removal
                    yaml-mode ;; major mode for YAML
                    yasnippet ;; snippets
                    ))
    
    (loop for pkg in pkgs do
          (require-package pkg))

## use-package<a id="orgheadline6"></a>

use-package (<https://github.com/jwiegley/use-package>) handles loading of packages, allowing a neatly organized configuration.

    (require 'use-package)

## a-mode<a id="orgheadline7"></a>

\`a-mode' is (if I remember correctly) my first Emacs Lisp function, quite simple really, all it does is act as a wrapper for \`auto-mode-alist', shortening the overall use of it from:

    (add-to-list 'auto-mode-alist "\\.markdown\\" 'markdown-mode)

to:

    (a-mode "markdown" "markdown-mode")

which I prefer

    (defun a-mode (ext mode)
      "A 'shortcut' for `(add-to-list 'auto-mode-alist [...])`'"
      (add-to-list 'auto-mode-alist
                   (cons
                    (format "\\%s\\'" ext)
                    (intern (concat mode "-mode")))))

## Org mode<a id="orgheadline11"></a>

    (require 'org)
    (define-key global-map (kbd "C-c l") 'org-store-link)
    (define-key global-map (kbd "C-c a") 'org-agenda)
    (setq org-log-done t)
    (setq org-directory "~/org") ;; where I store the documents
    (add-hook 'after-init-hook (lambda () 
                                 (define-key evil-normal-state-map (kbd "TAB") 'org-cycle))) ;; readd TAB back to normal mode in EVIL

### Babel<a id="orgheadline8"></a>

Load the Babel languages that I use, and also use nifty embedded highlighting (syntax within syntax)).

    (setq org-src-fontify-natively t)
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((ruby . t)
       (gnuplot . t)
       (org . t)))

### Misc. settings<a id="orgheadline9"></a>

Miscellaneous setting that don't go anywhere else.  

    (setq org-startup-with-inline-images t)

### Indentation<a id="orgheadline10"></a>

Enables indentation for the language when in a SRC block.

    (setq org-src-tab-acts-natively t)

## Flycheck<a id="orgheadline14"></a>

Flycheck is the "modern equivalent of flymake", think of it as [Syntastic](https://github.com/scrooloose/syntastic) but for Emacs.

It adds:

-   Markers in the fringe where syntax errors/style errors/warnings occur.
-   Adds an underline exactly where the error occurs. Fantastic when using a spell checker.

    (use-package flycheck
      :init
      (setq flycheck-check-syntax-automatically '(save mode-enabled)) ;; check when the file is written, or a new mode is enabled.
      (setq flycheck-highlighting-mode 'symbols)
      (add-hook 'after-init-hook 'global-flycheck-mode) ;; enable flycheck globally
      (setq flycheck-indication-mode 'left-fringe)) ;; indicate syntax errors/warnings in the left-fringe.

### Flyspell<a id="orgheadline12"></a>

Spell checking for Flycheck.

    (add-hook 'prog-mode-hook  'flyspell-prog-mode)
    (add-hook 'text-mode-hook  'flyspell-mode)
    (setq python-shell-interpreter "python3") ;; I use Python 3

### Load-path inheritance<a id="orgheadline13"></a>

This makes sure that when requiring a file that's within my \`load-path' when editing Emacs Lisp code that Flycheck uses my load-path instead of an internal one.

    (setq-default flycheck-emacs-lisp-load-path 'inherit)

## Emmet<a id="orgheadline16"></a>

[Emmet](http:/emmet.io) is an incredibly useful tool when dealing with HTML, think of it as "super-charged snippets for HTML".

### `turn-on-emmet-mode`<a id="orgheadline15"></a>

Tiny function to use instead of `(lambda [...])` to DRY the code.

    (defun turn-on-emmet-mode ()
      (emmet-mode 1))

For `(web|sgml|css)-mode`, turn on emmet-mode.

    (use-package emmet-mode
      :config
      (add-hook 'web-mode-hook 'turn-on-emmet-mode)
      (add-hook 'sgml-mode-hook 'turn-on-emmet-mode)
      (add-hook 'css-mode-hook 'turn-on-emmet-mode))

## ace-jump<a id="orgheadline20"></a>

`ace-jump` is like VIM's [EasyMotion](https://github.com/Lokaltog/vim-easymotion) but for Emacs.

    (use-package ace-jump-mode
      :config
      (define-key global-map (kbd "C-c SPC") 'ace-jump-mode))

### Flyspell<a id="orgheadline17"></a>

`ace-flyspell` navigating to incorrect spelling of words using `ace-jump`, I bind it to =C-c .

    (use-package ace-flyspell
      :config
      (define-key global-map (kbd "C-c .") 'ace-flyspell-jump-word))

### Helm<a id="orgheadline18"></a>

`ace-jump-helm-line` allows jumping to a Helm completion entry using `ace-jump`.

    (use-package ace-jump-helm-line
      :bind ("C-@" . ace-jump-helm-line)
      :config
      (setq helm-display-header-line nil)
      (set-face-attribute 'helm-source-header nil :height 0.1))

### Windows<a id="orgheadline19"></a>

`ace-window` allows jumping to a window using `ace-jump`

    (use-package ace-window
      :config
      (define-key global-map (kbd "C-c w") 'ace-window))

## Magit<a id="orgheadline21"></a>

Magit is fantastic!

    (use-package magit
      :config
      (setq magit-auto-revert-mode nil)
      (setq magit-last-seen-setup-instructions "1.4.0"))

## Auto completion<a id="orgheadline24"></a>

`Company` is a fantastic alternative to `auto-complete`.

The following:

-   Enables it globally.
-   Makes the completion window popup almost instantly.
-   Makes the completion window popup even if I type a single character.
-   Unbinds \`C-w\` when within the completion window to prevent a conflict with `evil-mode`.
-   Rebind the previously unbound `company-show-location` to `C-u`.
-   And finally makes `company-backends` local.

    (use-package company
      :config
      (add-hook 'after-init-hook 'global-company-mode) ;; enable company-mode globally
      (setq company-idle-delay 0.1)
      (setq company-minimum-prefix-length 1)
      (unbind-key (kbd "C-w") company-active-map)
      (define-key company-active-map (kbd "C-u") 'company-show-location)
      (make-variable-buffer-local 'company-backends))

### Anaconda<a id="orgheadline22"></a>

Allows for auto-completion with Python and Company.

    (use-package company-anaconda
      :config
      (add-hook 'python-mode-hook (lambda ()
                                    (anaconda-mode)
                                    (add-to-list 'company-backends 'company-anaconda))))

### Haskell<a id="orgheadline23"></a>

Utilize `ghc` to autocomplete using Company.

    (use-package company-ghc
      :config
      (add-hook 'haskell-mode-hook (lambda ()
                                     (add-to-list 'company-backends 'company-ghc)))
      ;; Haskell!
      (autoload 'ghc-init "ghc" nil t))

## edit-server<a id="orgheadline25"></a>

The Chrom(e|ium) addon [Edit with Emacs](https://chrome.google.com/webstore/detail/edit-with-emacs/ljobjlafonikaiipfkggjbhkghgicgoh) requires this.

    (use-package edit-server
      :config
      (when (string= (system-name) "linux-nyit.site") ;; home PC
        (edit-server-start)))

## EVIL<a id="orgheadline33"></a>

EVIL is VIM within Emacs.

    (use-package evil
      :config
      (evil-mode 1)
      (evil-define-key 'normal global-map (kbd "}]") 'emmet-next-edit-point)
      (evil-define-key 'normal global-map (kbd "{[") 'emmet-prev-edit-point)
      (evil-define-key 'normal global-map (kbd "U") 'undo-tree-visualize))

### Text-object delimiters<a id="orgheadline26"></a>

    (use-package surround
      :config
      (global-surround-mode 1))

### NERD-commenter<a id="orgheadline27"></a>

VIM's NERD-commenter but for Emacs.

    (use-package evil-nerd-commenter
      :config
      (define-key evil-normal-state-map "gci" 'evilnc-comment-or-uncomment-lines)
      (define-key evil-normal-state-map "gcl" 'evilnc-quick-comment-or-uncomment-to-the-line)
      (define-key evil-normal-state-map "gll" 'evilnc-quick-comment-or-uncomment-to-the-line)
      (define-key evil-normal-state-map "gcc" 'evilnc-copy-and-comment-lines)
      (define-key evil-normal-state-map "gcp" 'evilnc-comment-or-uncomment-paragraphs)
      (define-key evil-normal-state-map "gcr" 'comment-or-uncomment-region)
      (define-key evil-normal-state-map "gcv" 'evilnc-toggle-invert-comment-line-by-line))

### Leader<a id="orgheadline29"></a>

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

1.  TODO <code>[0/1]</code>

    -   [ ] Replace this with Hydra maybe?

### Modeline color changing<a id="orgheadline30"></a>

I found this in Bling's dotemacs.

    (set-face-background 'mode-line "bright-black")
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

### Cursor changing<a id="orgheadline31"></a>

    (setq evil-insert-state-cursor '((bar . 2) "green")
          evil-visual-state-cursor '((bar . 5) "green")
          evil-normal-state-cursor '((hollow . 5) "white"))

### Matching<a id="orgheadline32"></a>

Extends `%`.

    (use-package evil-matchit
      :config
      (global-evil-matchit-mode))

## GNU Global<a id="orgheadline34"></a>

    (use-package ggtags
      :config
      (add-hook 'prog-mode-hook 'ggtags-mode)) ;; enable ggtags for all prgoramming-mode

## Git-gutter<a id="orgheadline35"></a>

Git-gutter displays a summary of `git diff` in the left fringe of the current buffer.

    (use-package git-gutter
      :config
      (global-git-gutter-mode 1))

## Helm<a id="orgheadline39"></a>

    (use-package helm
      :config
      (helm-mode 1)
      (helm-autoresize-mode 1))

### Projectile<a id="orgheadline36"></a>

Integrate Projectile and Helm.

    (use-package helm-projectile
      :config
      (global-set-key (kbd "C-c h") 'helm-projectile))

### Helm-M-x<a id="orgheadline37"></a>

Helm's version of M-x.

    (define-key global-map (kbd "M-x") 'helm-M-x)

### Helm-swoop<a id="orgheadline38"></a>

Alternative to I-search.

    (use-package helm-swoop
      :config
      (define-key global-map (kbd "C-c C-a C-c") 'helm-swoop))

## Languages<a id="orgheadline54"></a>

### SCSS<a id="orgheadline40"></a>

    (use-package scss-mode
      :config
      (setq scss-compile-at-save nil)
      (a-mode ".scss" "scss"))

### Common Lisp<a id="orgheadline42"></a>

1.  SLIME

    SLIME (Superior Lisp Interaction Mode for Emacs) turns Emacs into an excellent IDE for Common Lisp.
    
    The following makes sure that I can still use the SLIME REPL history when on-the-go with my physical keyboard and phone.
    
    \`slime-setup' is also loads:
    
    -   slime-fancy: makes SLIME spiffy with history, and other stuff.
    -   slime-repl: the core of SLIME
    -   slime-company: auto-completion in the REPL when using SLIME.
    
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
          (slime-setup '(slime-fancy slime-repl slime-company))
          (setq inferior-lisp-program "sbcl")) ;; use SBCL

### Clojure<a id="orgheadline43"></a>

### Haskell<a id="orgheadline44"></a>

I don't program in Haskell much, but someday I will.

    (use-package haskell-mode
      :config
      (setq haskell-font-lock-symbols t) ;; spiffy symbols.
      (add-hook 'haskell-mode-hook 'ghc-init)
      (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation))

### Cider<a id="orgheadline45"></a>

I like Clojure, so CIDER is a must for me.

The following enables Eldoc for use with CIDER.

    (use-package cider
      :config
      (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
      (a-mode ".boot" "clojure")
      (add-to-list 'magic-mode-alist '(". boot" . clojure-mode)))

### JavaScript<a id="orgheadline47"></a>

I like JavaScript.

js2-mode is a great alternative to the standard js-mode.

    (use-package js2-mode
      :init
      (a-mode ".js" "js2")
      (add-hook 'js2-mode-hook (lambda ()
                                 (tern-mode t) ;; enable auto-completion using ternjs.
                                 (add-to-list 'company-backends 'company-tern))))

1.  Notes

    -   js2-mode works great with ES6
    -   ternjs doesn't work at all with ES6, but it is in the works.

### Web<a id="orgheadline48"></a>

    (use-package web-mode
      :config
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

### Gitolite<a id="orgheadline49"></a>

    (use-package gl-conf-mode
      :config
      (add-to-list 'auto-mode-alist '("gitolite\\.conf\\'" .
                                      gl-conf-mode)))

### GDScript<a id="orgheadline50"></a>

Godot's scripting language.

    (require 'gdscript-mode)

Also enable rainbow things for GDScript.

    (add-hook 'gdscript-mode-hook 'rainbow-identifiers-mode)
    (add-hook 'gdscript-mode-hook 'rainbow-delimiters-mode)

### CMake<a id="orgheadline51"></a>

CMake is a great alternative to autotools/automake. I use it for any C/C++ project I work on.

The following makes `CMakeLists.txt` use `cmake-mode`.

    (use-package cmake-mode
      :init
      (add-to-list 'auto-mode-alist
                   '("CMakeLists.txt" . cmake-mode)) )

### VisualBasic<a id="orgheadline52"></a>

Used for work experience.

    (autoload 'visual-basic-mode "visual-basic-mode" "Visual Basic mode." t)
    (a-mode ".vbs" "visual-basic")

### Scheme<a id="orgheadline53"></a>

Geiser is great for scheme.

    (use-package geiser
      :config
      (add-hook 'scheme-mode-hook (lambda ()
                                    (add-to-list 'company-backends 'geiser-company-backend))))

## ibuffer<a id="orgheadline56"></a>

ibuffer is an enhanced version of the standard `buffer-menu`.

### VC<a id="orgheadline55"></a>

Integrate ibuffer and vc.el.

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

## indent-guide<a id="orgheadline57"></a>

Indent-guide adds a fancy line to indicate the current indentation position.

    (use-package indent-guide
      :config
      (indent-guide-global-mode 1)) ;; enable globally

## Multiple-Major-Modes<a id="orgheadline58"></a>

This package is **fantastic** for things that embed other languages.

    (use-package mmm-mode
      :config
      (setq mmm-global-mode 'maybe)
      (mmm-add-classes
       '((markdown-latex
          :submode latex-mode
          :front "\\\\begin" ;; 2 blackslashes because of basedocument requiring 2 because of macro processing.
          :back "\\\\end")
         (markdown-erb
          :submode ruby-mode
          :front "<%"
          :back "%>")
         (markdown-clojure
          :submode clojure-mode
          :front "```clojure"
          :back "```")
         (markdown-ruby
          :submode ruby-mode
          :front "```ruby"
          :back "```")
         (markdown-haskell
          :submode haskell-mode
          :front "```haskell"
          :back "```")
         (markdown-lisp
          :submode common-lisp-mode
          :front "```commonlisp"
          :back "```")
         (shell-json
          :submode javascript-mode
          :front "<<JSON"
          :back "JSON")))
      (mmm-add-mode-ext-class 'markdown-mode "\\.md\\'" 'markdown-latex)
      (mmm-add-mode-ext-class 'markdown-mode "\\.mderb\\'" 'markdown-erb)
      (mmm-add-mode-ext-class 'shell-mode "\\.sh\\'" 'shell-json)
      (mmm-add-mode-ext-class 'markdown-mode "\\.md\\'" 'markdown-clojure)
      (mmm-add-mode-ext-class 'markdown-mode "\\.md\\'" 'markdown-ruby)
      (mmm-add-mode-ext-class 'markdown-mode "\\.md\\'" 'markdown-lisp)
      (mmm-add-mode-ext-class 'markdown-mode "\\.md\\'" 'markdown-haskell))
    (a-mode ".mderb" "markdown")

## Projectile<a id="orgheadline59"></a>

Projectile is great for handling large projects.

    (use-package projectile
      :bind ("C-c v f" . helm-projectile-find-file)
      :config
      (setq projectile-completion-system 'helm)
      (projectile-global-mode)
      (add-hook 'projectile-mode-hook 'projectile-rails-on))

## Smart-Mode-Line<a id="orgheadline60"></a>

I have yet to get around to making my own mode-line, but Smart-Mode-Line is great, so I don't see why I need to, other than for fun of course.

    (use-package smart-mode-line
      :config
      (setq sml/theme 'dark)
      (sml/setup))

## Ag<a id="orgheadline61"></a>

Ag is a super-fast alternative to grep.

    (use-package ag
      :config
      (define-key ag-mode-map (kbd "k") nil)) ;; stop conflicts with evil

## ws-butler<a id="orgheadline62"></a>

Used to remove whitespace.

    (use-package ws-butler
      :config
      (add-hook 'prog-mode-hook 'ws-butler-mode))

## YASnippet<a id="orgheadline63"></a>

Snippet for Emacs.

    (use-package yasnippet
      :config
      (yas-global-mode 1)
      (a-mode ".snip" "snippet")
      (define-key yas-minor-mode-map (kbd "C-c n") 'yas-next-field)
      (define-key yas-minor-mode-map (kbd "C-c p") 'yas-prev-field)
      (define-key yas-minor-mode-map (kbd "<tab>") nil)
      (define-key yas-minor-mode-map (kbd "TAB") nil)
      (define-key evil-insert-state-map (kbd "C-c RET") 'yas-expand))

(require 'ctags)
(setq ctags-command "/usr/bin/ctags-exuberant -e -R ")
(setq vc-follow-symlinks t)

## Email<a id="orgheadline65"></a>

    (add-hook 'mail-mode-hook 'auto-fill-mode) ;; hard-wrap text when emailing

### Wanderlust<a id="orgheadline64"></a>

Not used much, but might switch to Wanderlust one day.

    (autoload 'wl "wl" "Wanderlust" t)
    (a-mode ".wl" "emacs-lisp")
    (add-to-list 'auto-mode-alist
                 '("mutt-" . mail-mode)) ;; mutt temporary files
    (defun foo-wl ()
      (when evil-mode (evil-change-state 'emacs)))
    
    (add-hook 'wl-hook 'foo-wl)
    (add-hook 'wl-folder-mode-hook 'foo-wl)
    (add-hook 'wl-summary-mode-hook 'foo-wl)
    (add-hook 'wl-message-mode-hook 'foo-wl)
    (add-hook 'mime-view-mode-hook 'foo-wl)

## undotree<a id="orgheadline66"></a>

    (use-package undo-tree
      :config
      (setq undo-tree-auto-save-history 1)
      (setq undo-tree-history-directory-alist (quote (("." . "~/.emacs.d/undo/"))))
      (setq undo-tree-visualizer-diff t))

## Rainbows<a id="orgheadline67"></a>

    (use-package highlight-numbers
      :config
      (add-hook 'prog-mode-hook 'highlight-numbers-mode))
    (use-package rainbow-identifiers
      :config
      (add-hook 'prog-mode-hook 'rainbow-identifiers-mode))
    (use-package rainbow-delimiters
      :config
      (add-hook 'markdown-mode-hook 'rainbow-delimiters-mode)
      (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

## imenu<a id="orgheadline68"></a>

Useful for navigating around my config.

    (add-to-list 'imenu-generic-expression
                 '("Used Packages"
                   "\\(^\\s-*(use-package +\\)\\(\\_<.+\\_>\\)" 2))

## Misc<a id="orgheadline70"></a>

    (setq org-export-htmlize-output-type 'css)
    (set-face-attribute 'default nil :height 85)
    (require 'tramp) ;; edit files on remote locations
    (require 'whitespace) ;; whitespace monitor
    (column-number-mode 1) ;; enable column number in modeline
    (menu-bar-mode -1) ;; disabe menubar
    (tool-bar-mode -1) ;; disable toolbar
    (when (fboundp 'scroll-bar-mode)
      (scroll-bar-mode -1)) ;; disable scrollbar
    (electric-pair-mode 1) ;; match parens automatically
    (setq-default indent-tabs-mode nil) ;; no tabs.
    (setq-default tab-width 2) ;; Ruby-style indentation
    (require 'saveplace)
    (setq-default save-place t)
    (a-mode ".md" "markdown")
    (a-mode ".markdown" "markdown")
    (a-mode ".mw" "mediawiki")
    (a-mode "Gemfile" "ruby")
    (a-mode "Guardfile" "ruby")
    (a-mode "Rakefile" "ruby")
    (require 'htmlize)
    (electric-indent-mode 1) ;; automatically indent on RET or others
    (show-paren-mode 1) ;; highlight matching parens
    (mouse-avoidance-mode 'banish) ;; be gone cursor!
    (setq initial-scratch-message ;; I know it's a scratch buffer by now!
          (format ";; Emacs was started at %s"
                  (format-time-string "%Y-%m-%dT%T")))
    (setq package-menu-async nil) ;; disable this, I hate it when I press "U" just when I run `list-packages' to result in it claiming there's no updates avaialble so I have to run it again.

### Backups<a id="orgheadline69"></a>

I don't commit on every change I make, that'd be silly, so put numbered backups in here to not pollute commit history.

    (setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
    (setq delete-old-versions -1)
    (setq version-control t)
    (setq vc-make-backup-files t)
    (setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

## Utility functions<a id="orgheadline71"></a>

## Sort sexps<a id="orgheadline77"></a>

From Sacha Chua.

    (defun my/sort-sexps-in-region (beg end)
      "Can be handy for sorting out duplicates.
    Sorts the sexps from BEG to END. Leaves the point at where it
    couldn't figure things out (ex: syntax errors)."
      (interactive "r")
      (let ((input (buffer-substring beg end))
            list last-point form result)
        (save-restriction
          (save-excursion
            (narrow-to-region beg end)
            (goto-char (point-min))
            (setq last-point (point-min))
            (setq form t)
            (while (and form (not (eobp)))
              (setq form (ignore-errors (read (current-buffer))))
              (when form
                (add-to-list 'list
                             (cons
                              (prin1-to-string form)
                              (buffer-substring last-point (point))))
                (setq last-point (point))))
            (setq list (sort list (lambda (a b) (string< (car a) (car b)))))
            (delete-region (point-min) (point))
            (insert (mapconcat 'cdr list "\n"))))))

### Get a random item from a list<a id="orgheadline72"></a>

    (defun get-rnd-list (lst)
      "Get a random item from a list."
      (nth (random* (length lst)) lst))

### Get a random color<a id="orgheadline74"></a>

    (defun random-color ()
      "Get a random color."
      (get-rnd-list '("blue" "red" "yellow" "pink")))

1.  TODO <code>[0/1]</code>

    -   [ ] Add more colors

### Increment the number at point, like VIM's `C-a`<a id="orgheadline75"></a>

    (defun increment-number-at-point ()
      (interactive)
      (skip-chars-backward "0123456789")
      (or (looking-at "[0123456789]+")
          (error "No number at point"))
      (replace-match (number-to-string (1+ (string-to-number (match-string 0))))))
    (global-set-key (kbd "C-c +") 'increment-number-at-point)

### Decrement the number at point, like VIM's `C-x`<a id="orgheadline76"></a>

    (defun decrement-number-at-point ()
      (interactive)
      (skip-chars-backward "0123456789")
      (or (looking-at "[0123456789]+")
          (error "No number at point"))
      (replace-match (number-to-string (1- (string-to-number (match-string 0))))))
    
    (global-set-key (kbd "C-c -") 'decrement-number-at-point)

## Insert shell command<a id="orgheadline78"></a>

Insert the output of a shell command into the buffer at cursor's position.

    (defun insert-shell-command (command)
      (interactive "scommand: ")
      (insert (shell-command-to-string command)))
    
    (define-key global-map (kbd "C-c C-g") 'insert-shell-command)

## History<a id="orgheadline79"></a>

    (setq list-command-history-max 500) ;; save an insane amount of previously-used commands
    (setq savehist-file "~/.emacs.d/savehist")
    (savehist-mode 1)
    (setq history-length t)
    (setq history-delete-duplicates t)
    (setq savehist-save-minibuffer-history 1)
    (setq savehist-additional-variables
          '(kill-ring
            search-ring
            regexp-search-ring))

## Eldoc<a id="orgheadline80"></a>

    (add-hook 'python-mode-hook 'turn-on-eldoc-mode)
    (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)

## The end<a id="orgheadline81"></a>

    (provide 'init) ;; that's a wrap folks!
    ;;; init.el ends here

## Cascade startup system<a id="orgheadline84"></a>

### `waitforemacs`<a id="orgheadline82"></a>

Hangs until a certain other Emacs server starts.

    #!/bin/bash
    
    # This is neccesary to stop init file locking (config.el gets locked when tangling my config.org).
    
    emacstmp="/tmp/emacs$UID/"
    pid_location="$emacstmp/ready/"
    waiting_location="$emacstmp/waiting/"
    
    if [ ! $PROJECT_NAME ]; then
        if [ "$(basename $HOME)" == "$(basename $(projectroot))" ]; then
            PROJECT_NAME="server"
        else
            PROJECT_NAME="$(basename $(projectroot))"
        fi
    fi
    echo "export PROJECT_NAME="$PROJECT_NAME >| $pid_location/$PROJECT_NAME-env # parent process will inherit this env
    chmod +x $pid_location/$PROJECT_NAME-env
    
    
    # main server needs to kick things off
    if [ $PROJECT_NAME == server ]; then
        exit
    fi
    
    echo $PROJECT_NAME
    
    function determine_file() {
        case $PROJECT_NAME in
            x11)
                printf damon
                ;;
            damon)
                printf fb-mapper
                ;;
            fb-mapper)
                printf documents
                ;;
            documents)
                printf dotfiles
                ;;
            dotfiles)
                printf server
                ;;
            *)
        esac
    }
    
    # Process redirection is used here to store the `tail` PID, and then terminate it when finished.
    file_to_watch=$(determine_file)
    if [ -e $pid_location/$file_to_watch ]; then
        exit
    fi
    exec 3< <(tail -n 0 -f $emacstmp/inotify.log)
    log_pid=$$ # PID stored here
    cat <&3 | {
        read inotify_pid
        while read file; do
            if [ $(echo -n "$file" | grep -q "$file_to_watch$" ; echo $?) == 0 ]; then
                echo continuing...
                kill $log_pid # we're done, terminate and let other things take over
            else
                echo $file # debugging, really
            fi
        done
    }

### `emacsinotify`<a id="orgheadline83"></a>

    #!/bin/bash
    
    # This program runs a daemonized inotify to avoid using multiple inotifywait processes.
    # It saves output to `inotify.log` which is read by some `tail` processes in `waitforemacs`
    
    emacstmp="/tmp/emacs$UID/"
    pid_location="$emacstmp/ready/"
    waiting_location="$emacstmp/waiting/"
    if [ ! -e $emacstmp ]; then
        mkdir $emacstmp
    
        # make directory safe
        chmod o-wrx $emacstmp
        chmod g-wrx $emacstmp
    fi
    if [ ! -e $waiting_location ]; then
        mkdir $waiting_location
    fi
    if [ ! -e $pid_location ]; then
        mkdir $pid_location
    fi
    
    inotifywait -m -e create --format '%f' $pid_location -d -o $emacstmp/inotify.log

# Window manager (Awesome)<a id="orgheadline120"></a>

I use Awesome (and no desktop environment) for managing windows.

This configuration was a sample configuration taken elsewhere, I have unfortuently forgotten where it came from.

This configuration is heavily modified from the original one.

## Requires<a id="orgheadline86"></a>

    -- Standard awesome library
    local gears = require("gears")
    local awful = require("awful")
    awful.rules = require("awful.rules") -- used to make Chromium (and others) to go specific workspaces (and more!)
    require("awful.autofocus")
    -- Widget and layout library
    local wibox = require("wibox") -- text boxes, buttons, and other widgets
    -- Theme handling library
    local beautiful = require("beautiful") -- theming engine
    local naughty = require("naughty") -- notification library
    local menubar = require("menubar") -- menubars
    local vicious = require("vicious")
    local alttab = require('alttab')
    require("obvious.volume_alsa") -- volume control
    require("obvious.mem")
    require("obvious.battery") -- battery indicator

## Utility functions<a id="orgheadline89"></a>

### `does_monitor_exist()`<a id="orgheadline88"></a>

Determine whether a monitor exists.   

    function does_monitor_exist(m)
       if vicious.widgets.os()[4] == 'linux.site' then
          if screen.count() == 2 then
             return m
          else
             return 1
          end
       else
          return 1
       end
    end

1.  TODO 

    -   [ ] Make more robust <code>[0/1]</code>
        -   [ ] Use `xrandr` or something instead of using a host.

## Error handling<a id="orgheadline92"></a>

### Handle startup errors<a id="orgheadline90"></a>

    -- Check if awesome encountered an error during startup and fell back to
    -- another config (This code will only ever execute for the fallback config)
    if awesome.startup_errors then
       naughty.notify({ preset = naughty.config.presets.critical,
                        title = "Oops, there were errors during startup!",
                        text = awesome.startup_errors })
       right_layout:add(obvious.battery())
    end

### Handle runtime errors<a id="orgheadline91"></a>

    do
       local in_error = false
       awesome.connect_signal("debug::error", function (err)
                                 -- Make sure we don't go into an endless error loop
                                 if in_error then return end
                                 in_error = true
    
                                 naughty.notify({ preset = naughty.config.presets.critical,
                                                  title = "Oops, an error happened!",
                                                  text = err })
                                 in_error = false
       end)
    end

## Theme<a id="orgheadline93"></a>

    -- Themes define colours, icons, and wallpapers
    beautiful.init(os.getenv("HOME").."/.config/awesome/themes/zenburn/theme.lua")

## Adjust screens<a id="orgheadline94"></a>

    if os.getenv("HOST") == "linux-nyit" then
       awful.util.spawn("xrandr --output VGA-1 --right-of HDMI-1 --output HDMI-1 --mode 1680x1050")
    end
    if os.getenv("HOST") == "xieshaij" then
       awful.util.spawn("xvfbsetupz")
    end

## Miscellaneous variables<a id="orgheadline95"></a>

## Layouts<a id="orgheadline97"></a>

    local layouts =
       {
          awful.layout.suit.tile,
          awful.layout.suit.tile.left,
          awful.layout.suit.tile.bottom,
          awful.layout.suit.tile.top,
          awful.layout.suit.fair,
          awful.layout.suit.fair.horizontal,
          awful.layout.suit.max,
          awful.layout.suit.max.fullscreen,
          awful.layout.suit.magnifier
       }

### TODO <code>[0/1]</code><a id="orgheadline96"></a>

-   [ ] Learn how to make my own layout.

## Wallpaper<a id="orgheadline98"></a>

    if beautiful.wallpaper then
       for s = 1, screen.count() do
          gears.wallpaper.maximized(beautiful.wallpaper, s, true)
       end
    end

## Tags<a id="orgheadline99"></a>

    -- Define a tag table which hold all screen tags.
    tags = {}
    for s = 1, screen.count() do
       -- Each screen has its own tag table.
       tags[s] = awful.tag({"➊", "➋", "➌", "➍", "➎", "➏", "➐", "➑", "➒", "➓"}, s, layouts[1])
    end

## Widgets<a id="orgheadline113"></a>

### Menu<a id="orgheadline100"></a>

    -- Create a laucher widget and a main menu
    myawesomemenu = {
       { "restart", awesome.restart }, -- restart button
       { "quit", awesome.quit } -- quit button
    }
    
    mymainmenu = awful.menu({ items = { { "awesome", myawesomemenu, beautiful.awesome_icon }}})
    
    mylauncher = awful.widget.launcher({ image = beautiful.awesome_icon, menu = mymainmenu }) -- top left button for the icon
    
    -- Menubar configuration
    menubar.utils.terminal = terminal -- Set the terminal for applications that require it

### Clock<a id="orgheadline101"></a>

    -- Create a textclock widget
    mytextclock = awful.widget.textclock()

### CPU<a id="orgheadline102"></a>

    local blingbling = require("blingbling")
    cpu_cores_conf = {height = 18, width = 8, rounded_size = 0.3}
    cpu_cores = {}
    
    function determine_core_count()
       if os.getenv("HOST") == "xieshaij" then
          return 4
       else
          return 1
       end
    end
    for i=1,determine_core_count() do
       cpu_cores[i] = blingbling.progress_graph(cpu_cores_conf)
       vicious.register(cpu_cores[i], vicious.widgets.cpu, "$"..(i+1).."",1)
    end

### Initial variables<a id="orgheadline103"></a>

    mywibox = {} -- entire top bar
    mypromptbox = {}
    mylayoutbox = {}
    mytaglist = {}

### Tag buttons<a id="orgheadline104"></a>

    mytaglist[s]=blingbling.tagslist(s,  awful.widget.taglist.filter.all, mytaglist.buttons)
    mytaglist.buttons = awful.util.table.join(
       awful.button({ }, 1, awful.tag.viewonly),
       awful.button({ modkey }, 1, awful.client.movetotag),
       awful.button({ }, 3, awful.tag.viewtoggle),
       awful.button({ modkey }, 3, awful.client.toggletag),
       awful.button({ }, 4, function(t) awful.tag.viewnext(awful.tag.getscreen(t)) end),
       awful.button({ }, 5, function(t) awful.tag.viewprev(awful.tag.getscreen(t)) end)
    )

### Task bar buttons<a id="orgheadline105"></a>

    mytasklist = {}
    mytasklist.buttons = awful.util.table.join(
       awful.button({ }, 1, function (c)
             if c == client.focus then
                c.minimized = true
             else
                -- Without this, the following
                -- :isvisible() makes no sense
                c.minimized = false
                if not c:isvisible() then
                   awful.tag.viewonly(c:tags()[1])
                end
                -- This will also un-minimize
                -- the client, if needed
                client.focus = c
                c:raise()
             end
       end),
       awful.button({ }, 3, function ()
             if instance then
                instance:hide()
                instance = nil
             else
                instance = awful.menu.clients({ width=250 })
             end
       end),
       awful.button({ }, 4, function ()
             awful.client.focus.byidx(1)
             if client.focus then client.focus:raise() end
       end),
       awful.button({ }, 5, function ()
             awful.client.focus.byidx(-1)
             if client.focus then client.focus:raise() end
    end))
    
    -- Create a tasklist widget

    for s = 1, screen.count() do -- Create a promptbox for each screen

### Task bar buttons (continued)<a id="orgheadline106"></a>

    mytasklist[s] = awful.widget.tasklist(s, awful.widget.tasklist.filter.currenttags, mytasklist.buttons)

### Volume<a id="orgheadline107"></a>

    if vicious.widgets.os()[4] == "linux-nyit.site" then
       speaker_name = "Master"
    else
       speaker_name = "Speaker"
    end
    if os.getenv('HOST') ~= "xieshaij" then
       right_layout:add(obvious.volume_alsa(0, speaker_name))
       right_layout:add(obvious.battery())
    end
    local right_layout = wibox.layout.fixed.horizontal()
    if s == 1 then right_layout:add(wibox.widget.systray()) end
    right_layout:add(mylayoutbox[s])
    right_layout:add(mytextclock)

### Layout<a id="orgheadline108"></a>

Image widget to contain an icon indicating which  layout we're using.

    -- Create an imagebox widget which will contains an icon indicating which layout we`re using.
    -- We need one layoutbox per screen.
    mylayoutbox[s] = awful.widget.layoutbox(s)
    mylayoutbox[s]:buttons(awful.util.table.join(awful.button({ }, 1, function () awful.layout.inc(1, s, layouts) end),
                                                 awful.button({ }, 3, function () awful.layout.inc(-1, s, layouts) end),
                                                 awful.button({ }, 4, function () awful.layout.inc(1, s, layouts) end),
                                                 awful.button({ }, 5, function () awful.layout.inc(-1, s, layouts) end)))

### Wibox<a id="orgheadline109"></a>

    -- Create the wibox
    mywibox[s] = awful.wibox({ position = "top", screen = s })

### Left aligned<a id="orgheadline110"></a>

    -- Widgets that are aligned to the left
    local left_layout = wibox.layout.fixed.horizontal()
    left_layout:add(mylauncher)
    left_layout:add(mytaglist[s])
    left_layout:add(mypromptbox[s])
    for i=1,determine_core_count() do
       left_layout:add(cpu_cores[i])
    end

### Bring it all together<a id="orgheadline111"></a>

    -- Now bring it all together (with the tasklist in the middle)
    local layout = wibox.layout.align.horizontal()
    layout:set_left(left_layout)
    layout:set_middle(mytasklist[s])
    layout:set_right(right_layout)
    
    mywibox[s]:set_widget(layout)
    end

### Next/prev tag<a id="orgheadline112"></a>

    root.buttons(awful.util.table.join(
                    awful.button({ }, 3, function () mymainmenu:toggle() end),
                    awful.button({ }, 4, awful.tag.viewnext),
                    awful.button({ }, 5, awful.tag.viewprev)
    ))

## Keys<a id="orgheadline116"></a>

### Initial table<a id="orgheadline114"></a>

    globalkeys = awful.util.table.join(

    awful.key({ modkey }, "g", function() alttab.switch(1, "Alt_L", "g", "g") end),
    awful.key({ modkey, "Control" }, "g", function() alttab.switch(-1, "Alt_L", "g", "g") end),
    awful.key({ modkey }, "s", function() awful.util.spawn("rofi -show window") end),
    awful.key({ modkey, "Shift" }, "`", function () awful.util.spawn("lock") end),
    awful.key({ modkey, }, "¬", function () awful.util.spawn("lock") end),
    awful.key({modkey, "Shift"}, "x", xrandr),
    awful.key({ modkey, "Shift" }, "p", function () awful.util.spawn("passmenu") end), -- Spawn the pass dmenu script.
    
    awful.key({ modkey,           }, "Escape", awful.tag.history.restore),
    
    awful.key({ modkey,           }, "j",
       function ()
          awful.client.focus.byidx(1)
          if client.focus then client.focus:raise() end
    end),
    awful.key({ modkey,           }, "k",
       function ()
          awful.client.focus.byidx(-1)
          if client.focus then client.focus:raise() end
    end),
    awful.key({ modkey,           }, "w", function () mymainmenu:show() end),
    
    -- Layout manipulation
    awful.key({ modkey, "Shift"   }, "j", function () awful.client.swap.byidx(1)    end),
    awful.key({ modkey, "Shift"   }, "k", function () awful.client.swap.byidx(-1)    end),
    awful.key({ modkey, "Control" }, "j", function () awful.screen.focus_relative(1) end),
    awful.key({ modkey, "Control" }, "k", function () awful.screen.focus_relative(-1) end),
    awful.key({ modkey,           }, "u", awful.client.urgent.jumpto),
    awful.key({ modkey,           }, "Tab",
       function ()
          awful.client.focus.history.previous()
          if client.focus then
             client.focus:raise()
          end
    end),
    
    -- Standard program
    awful.key({ modkey,           }, "Return", function () awful.util.spawn(terminal) end),
    awful.key({ modkey, "Control" }, "r", awesome.restart),
    awful.key({ modkey, "Shift"   }, "q", awesome.quit),
    
    awful.key({ modkey,           }, "l",     function () awful.tag.incmwfact(0.05)    end),
    awful.key({ modkey,           }, "h",     function () awful.tag.incmwfact(-0.05)    end),
    awful.key({ modkey, "Shift"   }, "h",     function () awful.tag.incnmaster(1)      end),
    awful.key({ modkey, "Shift"   }, "l",     function () awful.tag.incnmaster(-1)      end),
    awful.key({ modkey, "Control" }, "h",     function () awful.tag.incncol(1)         end),
    awful.key({ modkey, "Control" }, "l",     function () awful.tag.incncol(-1)         end),
    awful.key({ modkey, "Control" }, "n", awful.client.restore),
    
    awful.key({ modkey }, "p", function() awful.util.spawn("rofi -show run") end) -- (dmenu2)
    )
    
    clientkeys = awful.util.table.join(
       awful.key({ modkey }, "Next",  function () awful.client.moveresize(20,  20, -40, -40) end),
       awful.key({ modkey }, "Prior", function () awful.client.moveresize(-20, -20,  40,  40) end),
       awful.key({ modkey }, "Down",  function () awful.client.moveresize(0,  20,   0,   0) end),
       awful.key({ modkey }, "Up",    function () awful.client.moveresize(0, -20,   0,   0) end),
       awful.key({ modkey }, "Left",  function () awful.client.moveresize(-20,   0,   0,   0) end),
       awful.key({ modkey }, "Right", function () awful.client.moveresize(20,   0,   0,   0) end),
       awful.key({ modkey,           }, "f",      function (c) c.fullscreen = not c.fullscreen  end),
       awful.key({ modkey, "Shift"   }, "c",      function (c) c:kill()                         end),
       awful.key({ modkey, "Control" }, "space",  awful.client.floating.toggle                     ),
       awful.key({ modkey, "Control" }, "Return", function (c) c:swap(awful.client.getmaster()) end),
       awful.key({ modkey,           }, "o",      awful.client.movetoscreen                        ),
       awful.key({ modkey,           }, "t",      function (c) c.ontop = not c.ontop            end),
       awful.key({ modkey,           }, "n",
          function (c)
             -- The client currently has the input focus, so it cannot be
             -- minimized, since minimized clients can`t have the focus.
             c.minimized = true
       end),
       awful.key({ modkey,           }, "m",
          function (c)
             c.maximized_horizontal = not c.maximized_horizontal
             c.maximized_vertical   = not c.maximized_vertical
       end)
    )
    
    -- Bind all key numbers to tags.
    -- Be careful: we use keycodes to make it works on any keyboard layout.
    -- This should map on the top row of your keyboard, usually 1 to 9.
    for i = 1, 9 do
       globalkeys = awful.util.table.join(globalkeys,
                                          awful.key({ modkey }, "#" .. i + 9,
                                             function ()
                                                local screen = mouse.screen
                                                local tag = awful.tag.gettags(screen)[i]
                                                if tag then
                                                   awful.tag.viewonly(tag)
                                                end
                                          end),
                                          awful.key({ modkey, "Control" }, "#" .. i + 9,
                                             function ()
                                                local screen = mouse.screen
                                                local tag = awful.tag.gettags(screen)[i]
                                                if tag then
                                                   awful.tag.viewtoggle(tag)
                                                end
                                          end),
                                          awful.key({ modkey, "Shift" }, "#" .. i + 9,
                                             function ()
                                                if client.focus then
                                                   local tag = awful.tag.gettags(client.focus.screen)[i]
                                                   if tag then
                                                      awful.client.movetotag(tag)
                                                   end
                                                end
                                          end),
                                          awful.key({ modkey, "Control", "Shift" }, "#" .. i + 9,
                                             function ()
                                                if client.focus then
                                                   local tag = awful.tag.gettags(client.focus.screen)[i]
                                                   if tag then
                                                      awful.client.toggletag(tag)
                                                   end
                                                end
       end))
    end
    
    clientbuttons = awful.util.table.join(
       awful.button({ }, 1, function (c) client.focus = c; c:raise() end),
       awful.button({ modkey }, 1, awful.mouse.client.move),
       awful.button({ modkey }, 3, awful.mouse.client.resize))

### Pull it all together<a id="orgheadline115"></a>

    root.keys(globalkeys)

## Rules<a id="orgheadline117"></a>

    awful.rules.rules = {
       -- All clients will match this rule.
       { rule = { },
         properties = { border_width = beautiful.border_width,
                        border_color = beautiful.border_normal,
                        focus = awful.client.focus.filter,
                        keys = clientkeys,
                        buttons = clientbuttons } },
       { rule = { instance = "Chrome" },
         properties = {tag = tags[1][2]}}
    }

## Signals<a id="orgheadline119"></a>

Signal function to execute when a new client appears.

    client.connect_signal("manage", function (c, startup)

### Hover focus<a id="orgheadline118"></a>

This is one of my favorite things in a window manager, I **love** it when I hover over a window and it focuses.

    c:connect_signal("mouse::enter", function(c)
                        if awful.layout.get(c.screen) ~= awful.layout.suit.magnifier
                        and awful.client.focus.filter(c) then
                           client.focus = c
                        end
    end)

    if not startup then
       -- Set the windows at the slave,
       -- i.e. put it at the end of others instead of setting it master.
       -- awful.client.setslave(c)
    
       -- Put windows in a smart way, only if they does not set an initial position.
       if not c.size_hints.user_position and not c.size_hints.program_position then
          awful.placement.no_overlap(c)
          awful.placement.no_offscreen(c)
       end
    end
    
    local titlebars_enabled = false
    if titlebars_enabled and (c.type == "normal" or c.type == "dialog") then
       -- buttons for the titlebar
       local buttons = awful.util.table.join(
          awful.button({ }, 1, function()
                client.focus = c
                c:raise()
                awful.mouse.client.move(c)
          end),
          awful.button({ }, 3, function()
                client.focus = c
                c:raise()
                awful.mouse.client.resize(c)
       end))
    
       -- Widgets that are aligned to the left
       local left_layout = wibox.layout.fixed.horizontal()
       left_layout:add(awful.titlebar.widget.iconwidget(c))
       left_layout:buttons(buttons)
    
       -- Widgets that are aligned to the right
       local right_layout = wibox.layout.fixed.horizontal()
       right_layout:add(awful.titlebar.widget.floatingbutton(c))
       right_layout:add(awful.titlebar.widget.maximizedbutton(c))
       right_layout:add(awful.titlebar.widget.stickybutton(c))
       right_layout:add(awful.titlebar.widget.ontopbutton(c))
       right_layout:add(awful.titlebar.widget.closebutton(c))
    
       -- The title goes in the middle
       local middle_layout = wibox.layout.flex.horizontal()
       local title = awful.titlebar.widget.titlewidget(c)
       title:set_align("center")
       middle_layout:add(title)
       middle_layout:buttons(buttons)
    
       -- Now bring it all together
       local layout = wibox.layout.align.horizontal()
       layout:set_left(left_layout)
       layout:set_right(right_layout)
       layout:set_middle(middle_layout)
    
       awful.titlebar(c):set_widget(layout)
    end
    end)
    
    client.connect_signal("focus", function(c) c.border_color = beautiful.border_focus end)
    client.connect_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)

# zsh<a id="orgheadline146"></a>

## Misc<a id="orgheadline121"></a>

    setopt AUTO_PUSHD

## PATH<a id="orgheadline122"></a>

    export PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting
    export PATH=$HOME/bin:$PATH # weird how this isn't a default...
    export PATH=$PATH:/usr/local/bin
    export PATH=$PATH:$HOME/.local/bin
    export GOPATH=$HOME/gostuff

## Variables<a id="orgheadline123"></a>

    export EDITOR=e
    HISTSIZE=99999999
    SAVEHIST=99999999
    HISTFILE=~/.zsh_history

## Functions<a id="orgheadline133"></a>

### Detect command<a id="orgheadline124"></a>

This function returns exit code `1` if the command does **not** exist.

    function detect_command() {
        command -v $1 >/dev/null 2>&1 || return 1
        return 0
    }

### Run command<a id="orgheadline125"></a>

This function runs the command if the command exists.

    function run_command() {
        if [ $($(detect_command $1); echo $?) -eq 0 ]; then
            $@
        fi
    }

### `source_if_exists`<a id="orgheadline126"></a>

Source the file if `file` exists.

    function source_if_exists() {
        [ ! -e "$1" ] || source "$1"
    }

### Emacs projects<a id="orgheadline130"></a>

1.  `ep`

        function ep {
            emacsclient -c -s $(basename $(projectroot)) $@
        }

2.  `ep-start`

        function ep-start {
            sleep 1 # wait for shells to initialise, THEN start
            waitforemacs $(basename $(projectroot))
            if [ $(basename $(projectroot)) == $(basename $HOME) ]; then
                . /tmp/emacs$UID/ready/server-env
            else
                . /tmp/emacs$UID/ready/$(basename $(projectroot))-env
            fi
            emacs
            rm /tmp/emacs$UID/ready/$PROJECT_NAME
        }

3.  `ep-stop`

        function ep-stop {
            if [ $(basename $(projectroot)) == $(basename $HOME) ]; then
                emacsclient -t -s server -e '(safe-buffers-kill-emacs)'
                rm /tmp/emacs$UID/ready/server
                rm /tmp/emacs$UID/waiting/server
                rm /tmp/emacs$UID/server
            else
                emacsclient -t -s $(basename $(projectroot)) -e '(safe-buffers-kill-emacs)'
                rm /tmp/emacs$UID/ready/$(basename (projectroot))
                rm /tmp/emacs$UID/waiting/$(basename (projectroot))
                rm /tmp/emacs$UID/$(basename (projectroot))
            fi
        }

### <http://gitignore.io><a id="orgheadline131"></a>

    function gi() { curl -L -s https://www.gitignore.io/api/$@ }

### Load Minetest mods into config<a id="orgheadline132"></a>

    mt_load_mods() {
        for i in $(echo *) ; do echo $i | sed 's/^/load_mod_/g' | sed 's/$/ = true/g' ; done >> $1
    }

## `gpg-agent`<a id="orgheadline134"></a>

Runs `gpg-agent` if it isn't already running.

    if [ -f "${HOME}/.gpg-agent-info" ]; then
        . "${HOME}/.gpg-agent-info"
        export GPG_AGENT_INFO
    else
        if [[ $HOST == "linux-nyit" ]]; then
            gpg-agent --enable-ssh-support --daemon --write-env-file "${HOME}/.gpg-agent-info"
        else
            gpg-agent --daemon --write-env-file "${HOME}/.gpg-agent-info"
        fi
    fi

## zkbd<a id="orgheadline135"></a>

    # From the ZSH wiki
    # create a zkbd compatible hash;
    # to add other keys to this hash, see: man 5 terminfo
    
    typeset -A key
    
    key[Home]=${terminfo[khome]}
    key[End]=${terminfo[kend]}
    key[Insert]=${terminfo[kich1]}
    key[Delete]=${terminfo[kdch1]}
    key[Up]=${terminfo[kcuu1]}
    key[Down]=${terminfo[kcud1]}
    key[Left]=${terminfo[kcub1]}
    key[Right]=${terminfo[kcuf1]}
    key[PageUp]=${terminfo[kpp]}
    key[PageDown]=${terminfo[knp]}
    
    # setup key accordingly
    [[ -n "${key[Home]}"    ]]  && bindkey  "${key[Home]}"    beginning-of-line
    [[ -n "${key[End]}"     ]]  && bindkey  "${key[End]}"     end-of-line
    [[ -n "${key[Insert]}"  ]]  && bindkey  "${key[Insert]}"  overwrite-mode
    [[ -n "${key[Delete]}"  ]]  && bindkey  "${key[Delete]}"  delete-char
    [[ -n "${key[Up]}"      ]]  && bindkey  "${key[Up]}"      up-line-or-history
    [[ -n "${key[Down]}"    ]]  && bindkey  "${key[Down]}"    down-line-or-history
    [[ -n "${key[Left]}"    ]]  && bindkey  "${key[Left]}"    backward-char
    [[ -n "${key[Right]}"   ]]  && bindkey  "${key[Right]}"   forward-char
    
    # Finally, make sure the terminal is in application mode, when zle is
    # active. Only then are the values from $terminfo valid.
    function zle-line-init () {
        echoti smkx
    }
    function zle-line-finish () {
        echoti rmkx
    }
    
    zle -N zle-line-init
    zle -N zle-line-finish

## Source plugins<a id="orgheadline136"></a>

    source_if_exists ~/.zsh/plugins/opp.zsh/opp.zsh
    source_if_exists ~/.zsh/plugins/opp.zsh/opp/*.zsh
    source_if_exists ~/.zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
    source_if_exists ~/.zsh/plugins/zsh-history-substring-search/zsh-history-substring-search.zsh
    source_if_exists $HOME/.homesick/repos/homeshick/homeshick.sh
    source_if_exists ~/.autojump/etc/profile.d/autojump.sh
    source_if_exists ~/.zsh/plugins/zsh-vcs-prompt/zshrc.sh
    source_if_exists ~/.fzf.zsh
    source_if_exists ~/.locals.sh # host specific things *not* to be checked into version control.

## `pyenv`<a id="orgheadline137"></a>

Initial `pyenv` if found.

    if [[ ! "$(which pyenv)" =~ "not found" ]] ; then
        eval "$(pyenv init -)"
    fi

## Completion<a id="orgheadline138"></a>

    setopt prompt_subst
    autoload -U colors && colors
    setopt histignorealldups sharehistory
    autoload -Uz compinit
    compinit
    zstyle ':completion:*' menu select
    zstyle ':completion:*' auto-description 'specify: %d'
    zstyle ':completion:*' completer _expand _complete _correct _approximate
    zstyle ':completion:*' format 'Completing %d'
    zstyle ':completion:*' group-name ''
    zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
    zstyle ':completion:*' list-colors ''
    zstyle ':completion:*' matcher-list '' 'm:{a-z}={A-Z}' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=* l:|=*'
    zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
    zstyle ':completion:*' use-compctl true
    zstyle ':completion:*' verbose true
    zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;31'
    zstyle ':completion:*:kill:*' command 'ps -u $USER -o pid,%cpu,tty,cputime,cmd'
    setopt interactivecomments
    eval "$(dircolors -b)"

    if [[ -z "$STY" && -z "$TMUX" ]] && [[ "$TERM" == (xterm|rxvt|konsole)* || -n "$COLORTERM" ]] && [[ "$TERM" != "dumb"  ]]; then
        export TERM='xterm-256color'
    fi

## key bindings<a id="orgheadline139"></a>

    bindkey -v
    bindkey -s '^O' '^qcd\n'
    bindkey '^f' vi-forward-blank-word
    export KEYTIMEOUT=1
    ZSH_VCS_PROMPT_ENABLE_CACHING='true'
    fpath=(~/.zsh/plugins/zsh-completions/src ~/.zsh/completion $fpath)
    export rvmsudo_secure_path=1
    
    VIM_PROMPT="%F{yellow}%F{blue}[%f%F{yellow}N%f%F{blue}]%k%f"
    function zle-line-init zle-keymap-select {
        RPS1="${${KEYMAP/vicmd/$VIM_PROMPT}/(main|viins)/}"
        zle reset-prompt
    }

## Prompt<a id="orgheadline140"></a>

    zle -N zle-line-init
    zle -N zle-keymap-select
    bindkey -M vicmd 'k' history-substring-search-up
    bindkey -M vicmd 'j' history-substring-search-down
    bindkey '^[[A' history-substring-search-up
    bindkey '^[[B' history-substring-search-down
    fancy-ctrl-z () {
        if [[ $#BUFFER -eq 0 ]]; then
            fg
            zle redisplay
        else
            zle push-input
            zle clear-screen
        fi
    }
    zle -N fancy-ctrl-z
    bindkey '^Z' fancy-ctrl-z
    autoload -Uz compinit
    compinit

## FZF<a id="orgheadline141"></a>

Taken from the FZF README.

    # fe [FUZZY PATTERN] - Open the selected file with the default editor
    #   - Bypass fuzzy finder if there's only one match (--select-1)
    #   - Exit if there's no match (--exit-0)
    fe() {
        local file
        file=$(fzf --query="$1" --select-1 --exit-0)
        [ -n "$file" ] && ${EDITOR:-vim} "$file"
    }
    
    # fd - cd to selected directory
    fd() {
        local dir
        dir=$(find ${1:-*} -path '*/\.*' -prune \
                   -o -type d -print 2> /dev/null | fzf +m) &&
            cd "$dir"
    }
    
    # fda - including hidden directories
    fda() {
        local dir
        dir=$(find ${1:-.} -type d 2> /dev/null | fzf +m) && cd "$dir"
    }
    
    # fh - repeat history
    fh() {
        eval $(([ -n "$ZSH_NAME" ] && fc -l 1 || history) | fzf +s | sed 's/ *[0-9]* *//')
    }
    
    # fkill - kill process
    fkill() {
        ps -ef | sed 1d | fzf -m | awk '{print $2}' | xargs kill -${1:-9}
    }
    
    # fbr - checkout git branch
    fbr() {
        local branches branch
        branches=$(git branch) &&
            branch=$(echo "$branches" | fzf +s +m) &&
            git checkout $(echo "$branch" | sed "s/.* //")
    }
    
    # fstage - stage uncommited file
    
    fstage() {
        local files to_stage
        files="$(git status --porcelain)"
        to_stage=$(echo $files | fzf -m | rev | cut -d' ' -f1 | rev)
        git add $(echo $to_stage | tr '\n' ' ')
    }
    
    # fco - checkout git commit
    fco() {
        local commits commit
        commits=$(git log --pretty=oneline --abbrev-commit --reverse) &&
            commit=$(echo "$commits" | fzf +s +m -e) &&
            git checkout $(echo "$commit" | sed "s/ .*//")
    }
    
    # ftags - search ctags
    ftags() {
        local line
        [ -e tags ] &&
            line=$(
                awk 'BEGIN { FS="\t" } !/^!/ {print toupper($4)"\t"$1"\t"$2"\t"$3}' tags |
                    cut -c1-80 | fzf --nth=1,2
                ) && $EDITOR $(cut -f3 <<< "$line") -c "set nocst" \
                             -c "silent tag $(cut -f2 <<< "$line")"
    }

## pprompt<a id="orgheadline142"></a>

    # Colors
    _p_color_date=cyan
    _p_color_pwd=cyan
    _p_color_pwd_fg=red
    _p_color_user=white
    _p_color_user_fg=black
    _p_color_host=white
    _p_color_host_fg=black
    
    p_module_privsymbol() {
        if [[ $(print -P "%#") == "#" ]] ; then
            _p_color_user_privsymbol=red
        else
            _p_color_user_privsymbol=blue
        fi
        echo "%F{$_p_color_user_privsymbol}%#%f"
    }
    
    p_module_host() {
        echo "%F{$_p_color_host_fg}%K{$_p_color_host}%m%k%f"
    }
    
    p_module_user() {
        echo "%F{$_p_color_user_fg}%K{$_p_color_user}%n%F{red}⍟%f%k%f"
    }
    
    p_module_pwd() {
        echo "%F{$_p_color_pwd_fg}%K{$_p_color_pwd}%~%k%f"
    }
    
    p_module_time() {
        echo '%F{$_p_color_date}%D{%H:%M:%S}%f'
    }
    
    p_load() {
        export PS1=$(p_module_pwd)" "
        export PS1=$PS1$(p_module_time)" "
        export PS1=$PS1$(p_module_user)
        export PS1=$PS1$(p_module_host)" "
        export PS1=$PS1$(p_module_privsymbol)" "
        export PS1=$PS1'$(vcs_super_info)'
        export PS1=$PS1$'\n'
        export PS1=$PS1"$ "
    }
    
    p_load
    
    alias gpull='git pull'
    alias gstatus='git status'
    alias ..='cd ..'
    alias cab='cabal install'
    alias :q='exit'
    
    # `$TERM' is set to "dumb" when using TRAMP to connect to the host, my custom prompt
    # doesn't work well with TRAMP (i.e: makes TRAMP wait forever for a prompt), so simply
    # set `PS1' to a very simple prompt.
    [[ $TERM == "dumb" ]] && unsetopt zle && PS1='$ '
    
    [[ $DISPLAY == ":2" ]] && unset TMUX
    
    # When in tmux I leave some panes to idle, but when I'm not in tmux I don't need `$TMOUT'
    # especially when forwarding ports, so determine if within `TMUX' and set `TMOUT'.
    [[ $TMUX ]] && export TMOUT=3600
    
    
    
    task

## Aliases<a id="orgheadline145"></a>

### `gcem`<a id="orgheadline143"></a>

Bad habits&#x2026;

    alias gcem="git commit -am '' --allow-empty-message"

### `grmv`<a id="orgheadline144"></a>

    alias grmv="git remote -v"

# tmux<a id="orgheadline156"></a>

tmux is AWESOME!

## Powerline<a id="orgheadline147"></a>

    run-shell "powerline-daemon -q"

## Vi keys<a id="orgheadline148"></a>

    set -g mode-keys vi
    set -g status-keys vi

## Bindings<a id="orgheadline149"></a>

    set -g prefix C-b # use this prefix key, this is the default but just in case it goes weird or something I guess
    bind-key m split-window mutt # email
    bind-key q run-shell -b tmux-charsel # find regex
    bind-key t run-shell -b tmux-task # open taskwarrior
    bind-key 0 select-window -t :10
    bind-key C-d detach-client -P
    bind-key "#" last-window
    bind-key @ switch-client -l

## Settings<a id="orgheadline150"></a>

    set -g base-index 1
    
    set -g display-time 2000
    
    set -s escape-time 0
    
    set -g aggressive-resize on
    
    set -g message-bg colour235
    set -g message-fg white
    
    set -g pane-border-fg '#333333'
    set -g pane-active-border-fg cyan
    
    set -g default-terminal 'screen-256color' # otherwise colors go weird
    set -g update-environment "REMOTE_HOSTNAME SSH_ASKPASS SSH_AUTH_SOCK SSH_AGENT_PID SSH_CONNECTION WINDOWID XAUTHORITY"
    
    set-window-option -g pane-base-index 1 # 0 on any keyboard is in a weird placed IMO
    
    set -g history-limit 200000 # insane scroll back size

## Plugins<a id="orgheadline151"></a>

    run-shell ~/bin/zsetuppowerline
    run-shell ~/.tmux/tmux-pain-control/pain_control.tmux
    run-shell ~/.tmux/tmux-copycat/copycat.tmux

## tmuxinator<a id="orgheadline155"></a>

### Emacs server<a id="orgheadline152"></a>

    # Initialize my Emacs servers.
    
    name: emacs
    root: ~/
    
    windows:
      - main:
          panes:
            - 'cd $HOME && emacsinotify && ep-start'
      - documents:
          panes:
            - 'cd $HOME/documents && ep-start'
      - dotfiles:
          panes:
            - 'cd $HOME/.homesick/repos/dotfiles && ep-start'
      - fbmapper:
          panes:
            - 'cd $HOME/fb-mapper && ep-start'
      - damon:
          panes:
            - 'cd $HOME/damon && ep-start'
      # - x11:
      #     panes:
      #       - 'cd $HOME && PROJECT_NAME=x11 ep-start'

### School<a id="orgheadline153"></a>

    # ~/.tmuxinator/school.yml
    
    name: school
    root: ~/documents
    
    
    windows:
      - main:
          layout: even-horizontal
          panes:
              - emacs:
                  - 'e'
              - pane2:
                  - 'bundle exec guard'

### X11<a id="orgheadline154"></a>

    name: x11
    
    windows:
      - main:
          panes:
            - xvfb:
                - "Xvfb :2 -auth $HOME/.Xauthority -nolisten tcp +extension RANDR -screen 0 $(screenres)x16" 
            - x11vnc:
                - "sleep 2; x11vnc -auth $HOME/.Xauthority -forever -usepw -localhost -display :2"
            - awesome:
                - "sleep 5; DISPLAY=:2 awesome"
