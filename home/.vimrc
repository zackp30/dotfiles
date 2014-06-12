" NeoBundle {{{
  set rtp+=~/.vim/neobundle.vim/
  set rtp+=~/.fzf
  call neobundle#rc(expand('~/.vim/bundle/'))
  " Bundles {{{
  " Didn't like it being renamed.
  command! -nargs=* Bundle NeoBundle <args>
  command! -nargs=1 BundleFetch NeoBundleFetch <args>
  NeoBundleFetch 'Shougo/neobundle.vim'
  autocmd FileType *.rkt NeoBundleDisable slimv
  " Languages {{{
    " Web development {{{
    Bundle 'kchmck/vim-coffee-script'
    Bundle 'digitaltoad/vim-jade'
    " }}}
    " LaTeX {{{
    Bundle 'LaTeX-Box-Team/LaTeX-Box'
    " }}}
    " Haskell {{{
    Bundle 'lukerandall/haskellmode-vim'
    Bundle 'bitc/vim-hdevtools'
    Bundle 'dag/vim2hs'
    Bundle 'eagletmt/neco-ghc'
    " }}}
    " Clojure {{{
      Bundle 'guns/vim-clojure-static'
      Bundle 'tpope/vim-fireplace'
      Bundle 'jpalardy/vim-slime'
      Bundle 'tpope/vim-classpath'
    " }}}
    " Go {{{
    Bundle 'https://github.com/yosssi/vim-gold'
    Bundle 'fatih/vim-go'
    " }}}
    " Ruby {{{
    Bundle 'vim-ruby/vim-ruby'
    Bundle 'kana/vim-textobj-user'
    Bundle 'nelstrom/vim-textobj-rubyblock'
    Bundle 'vim-scripts/rubycomplete.vim'
    " }}}
    " Python {{{
    Bundle 'davidhalter/jedi-vim'
    " Hy {{{
    Bundle 'hylang/vim-hy'
    " }}}
    " }}}
    " Racket {{{
    Bundle 'wlangstroth/vim-racket'
    " }}}
    " RimL {{{
    Bundle 'luke-gru/vim-riml'
    " }}}
  " }}}
  Bundle 'bling/vim-airline'
  Bundle 'kien/ctrlp.vim'
  Bundle 'Shougo/neocomplete.vim'
  Bundle 'mbbill/undotree'
  Bundle 'scrooloose/syntastic'
  Bundle 'bling/vim-bufferline'
  Bundle 'tomtom/tcomment_vim'
  Bundle 'Shougo/unite.vim'
  Bundle 'vim-pandoc/vim-pantondoc'
  Bundle 'vim-pandoc/vim-pandoc-syntax'
  Bundle 'osyo-manga/vim-over'
  Bundle 'Raimondi/delimitMate'
  Bundle 'szw/vim-dict'
  Bundle 'kshenoy/vim-signature'
  Bundle 'mhinz/vim-startify'
  Bundle 'justinmk/vim-gtfo'
  Bundle 'rking/ag.vim'
  Bundle 'edkolev/tmuxline.vim'
  Bundle 'groenewege/vim-less'
  Bundle 'rainbow_parentheses.vim'
  Bundle 'Shougo/vimproc', {
              \ 'build': {
              \ 'unix' : 'make -f make_unix.mak'}, }
  Bundle 'Shougo/neosnippet.vim'
  Bundle 'tpope/vim-surround'
  Bundle 'justinmk/vim-sneak'
  NeoBundle 'ZFileTypes'
  Bundle 'Twinside/vim-haskellConceal'
  Bundle 't9md/vim-choosewin'
  Bundle 'Shougo/vimfiler.vim'
  Bundle 'Shougo/neosnippet-snippets'
  Bundle 'tpope/vim-fugitive'
  Bundle 'vim-scripts/SyntaxRange'
  "Bundle 'http://www.vim.org/scripts/download_script.php?src_id=21479', 
              "\ { 'type__filename' : 'nav.vim', 'script_type' : 'plugin'}
  Bundle 'gregsexton/VimCalc'
  Bundle 'hrsh7th/vim-neco-calc'
  Bundle 'wting/rust.vim'
  Bundle 'slim-template/vim-slim'
  Bundle 'ntpeters/vim-better-whitespace'
  Bundle 'tpope/vim-jdaddy'
  Bundle 'gregsexton/gitv'
  Bundle 'nathanaelkane/vim-indent-guides'
  Bundle 'tpope/timl'
  Bundle 'maxmeyer/vim-taskjuggler'
  Bundle 'tpope/vim-dispatch'
  Bundle 'dyng/ctrlsf.vim'
  Bundle 'edsono/vim-matchit'
  Bundle 'naquad/ctrlp-digraphs.vim'
  Bundle 'vim-scripts/DrawIt'
  Bundle 'dhruvasagar/vim-table-mode'
  Bundle 'chrisbra/csv.vim'
  Bundle 'paradigm/SkyBison'
  Bundle 'wellle/targets.vim'
  Bundle 'airblade/vim-gitgutter'
  " Code navigation {{{
  Bundle 'majutsushi/tagbar'
  " }}}

  " Unite {{{
    Bundle 'majkinetor/unite-cmdmatch'
    Bundle 'kopischke/unite-spell-suggest'
  " }}
  NeoBundleCheck
  " }}}
  " }}}
" }}}
" Abbreviations {{{
    iabbrev @cc@ Copyright 2013 Zack Piper.
" }}}
" autocmds {{{
" Both below are from learnvimscriptthehardway.stevelosh.com.
    " onoremap in@ :execute "normal! /\\(.com\\|.net\\|.tk\\)\r"
    augroup Tmux
    au!
        autocmd VimEnter,BufNewFile,BufReadPost * call system('tmux rename-window "vim - ' . split(substitute(getcwd(), $HOME, '~', ''), '/')[-1] . '"')
        autocmd VimLeave * call system('tmux rename-window ' . split(substitute(getcwd(), $HOME, '~', ''), '/')[-1])
    augroup END
    augroup syntax
        au!
        autocmd Syntax set syntax=on
    augroup END
    augroup miscfts
      au!
      autocmd VimEnter,BufNewFile,BufReadPost *.page set ft=pandoc
      autocmd VimEnter,BufNewFile,BufReadPost *.plt set ft=gnuplot
    augroup end
" }}}
" Learn Vim script the hard way. {{{
    "nnoremap <silent> <leader>w :match /\v   
" }}}
" Mappings {{{
  let mapleader = "\<Space>"
  " http://www.reddit.com/r/vim/comments/275mng/what_have_you_recently_removed_from_your_vim/chxwtro
  nnoremap <silent> <Left> :bp<cr>
  nnoremap <silent> <Right> :bn<cr>
  nnoremap <silent> <Leader><Left> :tabp<cr>
  nnoremap <silent> <Leader><Right> :tabn<cr>
  noremap <leader>- <ESC> dd^p
  noremap <leader>_ <ESC> ddkP
  noremap <leader><leader>u :UndotreeToggle<cr>
  noremap <leader>; r,a 
  inoremap <leader><c-d> <esc> dd i
  inoremap <leader><c-u> <esc> bvwU<esc>i
  inoremap <Left> <nop>
  inoremap <Right> <nop>
  inoremap <Down> <nop>
  inoremap <Up> <nop>
  nnoremap <leader>" bi"<esc>wwa"<esc>
  nnoremap <leader>' bi'<esc>wwa'<esc>
  nnoremap H ^
  nnoremap L $
  nnoremap <leader>ev :vsplit $MYVIMRC<cr>
  nnoremap <leader>sv :source $MYVIMRC<cr>
  nnoremap <silent> <C-l> :nohl<CR><C-l>
  nnoremap <leader>yl Vy
  nnoremap <leader>/ :OverCommandLine<cr>
  nnoremap ]t :tabn<cr>
  nnoremap [t :tabp<cr>
  nnoremap ]b :bn<cr>
  nnoremap [b :bp<cr>
  onoremap p i(
  onoremap il( :<c-u>normal! F)vi(<cr>
  onoremap nl( :<c-u>normal! f)vi(<cr>
  onoremap ll( :<c-u>normal! $vi(<cr>
  onoremap in@ :<c-u>execute "normal! ^/\\a*.\\(com\\\|net\\\|tk\\\|ar\\\)\rBvE"<cr>
  vnoremap <leader>" <esc>`<<esc>i"<esc>`>i"<esc>
  " I don't use :ws.
  cnoremap ws w
" Plugs {{{
    " Neosnippet {{{
    imap <C-k>     <Plug>(neosnippet_expand_or_jump)
    smap <C-k>     <Plug>(neosnippet_expand_or_jump)
    xmap <C-k>     <Plug>(neosnippet_expand_target)

    " For snippet_complete marker.
    if has('conceal')
      set conceallevel=2 concealcursor=i
    endif
    " }}}
    " GoldenView {{{
      nmap <silent> <C-u> <Plug>GoldenViewSplit
      nmap <silent> <F8>   <Plug>GoldenViewSwitchMain
      nmap <silent> <S-F8> <Plug>GoldenViewSwitchToggle
      nmap <silent> <C-g>  <Plug>GoldenViewNext
      nmap <silent> <C-f>  <Plug>GoldenViewPrevious
    " }}}
    " CtrlP {{{
      " Yes, I know this is default, but something broke this...
      nmap <C-p> :CtrlP<cr>
    " }}}
    " vim-sneak {{{
      nmap }} <Plug>SneakForward
      nmap {{ <Plug>SneakBackward
    " }}}
    " Misc {{{
      nmap _ <Plug>(choosewin)
    " }}}
    " pt {{{
    " nnoremap <silent> ,g :<C-u>Unite grep:. -buffer-name=search-buffer<CR>
    if executable('pt')
      let g:unite_source_grep_command = 'pt'
      let g:unite_source_grep_default_opts = '--nogroup --nocolor'
      let g:unite_source_grep_recursive_opt = ''
    endif
    " }}}
" }}}
" }}}
" Misc {{{
  set nocompatible
  filetype off
  set shell=/bin/sh
  " Colorscheme.
  colorscheme zm5
  let g:colors_name="zm5"
  set laststatus=2
  set encoding=utf-8
  set t_Co=256
  syntax enable
  let g:syntastic_python_checkers=['pylint']
  " Tab {{{
      set expandtab
  " }}}
  set foldmethod=marker
  set number
  set fillchars+=vert:\|
  set backspace=indent,eol,start
  set ttimeoutlen=50
  set fcs+=vert:\ 
  set cursorline
  noremap <F1> <ESC>:set relativenumber! <ENTER>
  set numberwidth=1
  " Undofile {{{
      set undofile
      set undodir=~/.vimundo
  " }}}
  let g:netrw_liststyle = 3
  " Dictionary {{{
      let g:dict_hosts = [
                  \["127.0.0.1", ["gcide", "vera", "fd-eng-fra", "fd-fra-eng", "moby-thesaurus"]],
                  \]
  " }}}
  set maxfuncdepth=9001
  augroup scribble
      autocmd BufRead,BufNewFile *.scrbl set filetype=scribble
  augroup END
  " Change default binding for YankRing (<c-p) because of CtrlP.
  let g:yankring_replace_n_pkey="<C-H>"
  set history=500
  " Enables GVim like behaviour of showing the ``cached''
  " motions.
  set showcmd
  " Disables the ``echom'' of the current mode, because I have a
  " status line.
  set noshowmode
  " Wild menu {{{
    set wildmode=longest,list,full
    set wildmenu
  " }}}
  " Searching.
  set hls
  set is
  set ic
  " Indentation.
  set autoindent
  filetype indent on
  filetype plugin on
  set sw=2
  " Spell checking function, not even sure I need this anymore...
  function! Spellchecking()
      set spell
      highlight SpellBad ctermfg=green ctermbg=red
  endfunction
  nnoremap <leader><leader>s :call Spellchecking()<cr>
  let g:startify_custom_header =
    \ map(split(system('fortune | cowsay'), '\n'), '"   ". v:val') + ['','']
    \ "Yes, hello."
  " Whitespace management {{{
      set list
      "set listchars=trail:◉,tab:->
      "set listchars=trail:◦,tab:>-
      set listchars=tab:>-
      autocmd ColorScheme * highlight ExtraWhitespace ctermfg=red guifg=red
      highlight ExtraWhitespace ctermfg=red guifg=red
      match ExtraWhitespace /\s\+$/
  " }}}
  " Tagbar {{{
    autocmd FileType *.tex nested Tagbar
  " }}}
  " Autosave (Bram please) From: " http://stackoverflow.com/questions/6991638/how-to-auto-save-a-file-every-1-second-in-vi {{{
  let g:save_time = localtime()
  au BufRead,BufNewFile * let g:save_time = localtime()
  let g:autosave_time = 20
  function! AutoSave()
      if bufname("%") ==# 'ControlP'
      else
          if((localtime() - g:save_time) >= g:autosave_time)
              update
              let g:save_time = localtime()
          endif
      endif
  endfunction
  au CursorMoved * call AutoSave()
  au BufWritePre * let g:save_time = localtime()

  " }}}
  autocmd BufNew,BufRead Godfile set ft=ruby
  let g:indent_guides_auto_colors = 0
  let g:indent_guides_start_level = 2
  let g:indent_guides_guide_size = 1
  autocmd VimEnter,Colorscheme * :hi IndentGuidesOdd  guibg=red  ctermbg=grey
  autocmd VimEnter,Colorscheme * :hi IndentGuidesEven guibg=green ctermbg=darkgrey
  autocmd VimEnter * :IndentGuidesEnable
" }}}
" Plugins {{{
  " vim-airline {{{
    let g:airline_enable_syntastic=1
    let g:airline_enable_branch=1
    let g:airline#extensions#bufferline#enabled = 1
    let g:airline#extensions#tabline#enabled = 1
    let g:airline#extensions#eclim#enabled = 1
    let g:airline_enable_branch = 1
    let g:airline_mode_map = {'c': 'C', '^S': 'S-BLOCK', 'R': 'R', 's': 'S', 'V': 'V-L', '^V': 'V-B', 'i': 'I', '__': '------', 'S': 'S -LINE', 'v': 'V', 'n': 'N'}
  " }}}
  " Neo Complete {{{
    let g:neocomplete#enable_at_startup = 1
    let g:neocomplete#enable_smart_case = 1
    let g:acp_enableAtStartup = 0
    autocmd FileType css setlocal omnifunc=csscomplete#CompleteCSS
    autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
    autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
    autocmd FileType python setlocal omnifunc=pythoncomplete#Complete
    autocmd FileType xml setlocal omnifunc=xmlcomplete#CompleteTags
    autocmd FileType tex setlocal omnifunc=texcomplete#Complete
    "let g:neocomplete#sources#dictionary#dictionaries="/usr/share/dict/words"
  " }}}
  " vim-sunflower {{{
    let g:sunflower_lat=51
    let g:sunflower_long=0
    let g:sunflower_colorscheme_day='zm3_vim'
    let g:sunflower_colorscheme_night=''
  " }}}
  " delimitMate {{{
  " }}}
  " CSV.vim {{{
    let g:csv_delim=','
    let g:airline#extensions#csv#enabled = 1
  " }}}
  " DBExt {{{
    let g:dbext_default_profile_testuser = 'type=DBI:driver=mysql:user=testuser:passwd=yeshellotestuser:conn_parms=database=testdb;host=localhost'
  " }}}
  " CTRLP {{{
    let g:ctrlp_extensions = []
    let g:ctrlp_extensions += ['undo', 'smarttabs', 'test']
  " }}}
  " LaTeX-suite {{{
    let g:tex_flavor='latex'
    let g:Tex_CompileRule_pdf = 'pdflatex $*'
  " imo this was broken. vvv
    let g:tex_indent_items = 0
  " }}}
" ctags {{{
  let g:Tlist_WinWidth = 20
  let tlist_tex_settings = 'latex;l:labels;s:sections;t:subsections;u:subsubsections'
" }}}
" Bufferline {{{
  let g:bufferline_echo = 0
" }}}
" vim-sneak {{{
" I use S a lot...
    let g:sneak#streak = 1
" }}}
" vim-choosewin {{{
  let g:choosewin_overlay_enable = 1
" }}}
" vim-filer {{{
  let g:vimfiler_as_default_explorer = 1
" }}}
" NeoSnippet {{{
  let g:neosnippet#snippets_directory = "~/.vim/snippets/"
" }}}
" vim-EtherPad {{{
  " To connect to the pad at URI http://localhost:9001/p/test per default:
  let g:epad_host = "localhost" " Hostname to connect to
  let g:epad_port = 9011      " Port to connect to
  let g:epad_path = "p/"        " URL Path to the pad
  let g:epad_pad = "test"       " Name of the pad to connect to

" GUI feel
  let g:epad_updatetime = 1000  " lower this for more realtime, higher this for less load

" GUI look
  let g:epad_attributes = 0     " set to 1 to display attributes (works only with a font that)
  let g:epad_authors = 0        " set to 1 to display authors (works only in gui mode)

" Enable verbosity
  let g:epad_verbose = 2        " set to 1 for INFO level, 2 for DEBUG level
" }}}
" Syntastic {{{
  let g:syntastic_ruby_checkers=['mri', 'rubycop']
  let g:syntastic_javascript_checkers=['jshint']  
" }}}
" Slime {{{
    let g:slime_target = "tmux"
" }}}
" TComment {{{
        call tcomment#DefineType('scribble', "@; %s")
" }}}
" slimv {{{
let g:swank_port = 45126
" }}}
" Rainbow parenthesis {{{
" I prefer it kept on for all files.
au VimEnter * RainbowParenthesesToggle
au Syntax * RainbowParenthesesLoadRound
au Syntax * RainbowParenthesesLoadSquare
au Syntax * RainbowParenthesesLoadBraces
" }}}
" vim-ruby {{{
let g:rubycomplete_buffer_loading = 1
let g:rubycomplete_rails = 1
" }}}
" Eclim {{{
let g:EclimCompletionMethod = "omnifunc"
" }}}
" Tagbar {{{
nnoremap <Leader><Leader><Leader> <C-w><right><C-w><right><C-w><right><C-w><right>
" }}}
" }}}
