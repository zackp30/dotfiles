if version < 600
  syntax clear
elseif exists('b:current_syntax')
  finish
endif

runtime! syntax/sh.vim
unlet b:current_syntax

syn region ppmData
 \ matchgroup=ppmKeyword
 \ keepend
 \ start=/^data\>/
 \ end=/$/
 \ contains=ppmBlack,ppmDarkblue,ppmDarkgreen,ppmDarkcyan,ppmDarkred,ppmDarkmagenta,
    \ppmBrown,ppmLightgrey,ppmDarkgrey,ppmBlue,ppmGreen,ppmCyan,ppmRed,ppmMagenta,
    \ppmYellow,ppmWhite

syn match ppmBlack       /\C k/ contained
syn match ppmDarkblue    /\C b/ contained
syn match ppmDarkgreen   /\C g/ contained
syn match ppmDarkcyan    /\C c/ contained
syn match ppmDarkred     /\C r/ contained
syn match ppmDarkmagenta /\C m/ contained
syn match ppmBrown       /\C y/ contained
syn match ppmLightgrey   /\C w/ contained
syn match ppmDarkgrey    /\C K/ contained
syn match ppmBlue        /\C B/ contained
syn match ppmGreen       /\C G/ contained
syn match ppmRed         /\C R/ contained
syn match ppmMagenta     /\C M/ contained
syn match ppmYellow      /\C Y/ contained
syn match ppmWhite       /\C W/ contained

if version >= 508 || !exists('did_ppm_syntax_inits')
 if version < 508
  let did_ppm_syntax_inits = 1
  command -nargs=+ HiSet hi <args>
 else
  command -nargs=+ HiSet hi def <args>
 endif
 HiSet ppmBlack       ctermbg=Black       ctermfg=LightGrey     guibg=black       guifg=white
 HiSet ppmDarkblue    ctermbg=DarkBlue    ctermfg=LightGrey     guibg=darkblue    guifg=white
 HiSet ppmDarkGreen   ctermbg=DarkGreen   ctermfg=LightGrey     guibg=darkgreen   guifg=white
 HiSet ppmDarkCyan    ctermbg=DarkCyan    ctermfg=LightGrey     guibg=darkcyan    guifg=white
 HiSet ppmDarkRed     ctermbg=DarkRed     ctermfg=LightGrey     guibg=darkred     guifg=white
 HiSet ppmDarkmagenta ctermbg=DarkMagenta ctermfg=LightGrey     guibg=darkmagenta guifg=white
 HiSet ppmBrown       ctermbg=Brown       ctermfg=LightGrey     guibg=brown       guifg=white
 HiSet ppmLightgrey   ctermbg=LightGrey   ctermfg=Black         guibg=lightgrey   guifg=black
 HiSet ppmDarkgrey    ctermbg=DarkGrey    ctermfg=White         guibg=darkgrey    guifg=white
 HiSet ppmBlue        ctermbg=Blue        ctermfg=White         guibg=blue        guifg=white
 HiSet ppmCyan        ctermbg=Cyan        ctermfg=DarkGrey      guibg=cyan        guifg=white
 HiSet ppmRed         ctermbg=Red         ctermfg=White         guibg=red         guifg=black
 HiSet ppmMagenta     ctermbg=Magenta     ctermfg=White         guibg=magenta     guifg=black
 HiSet ppmYellow      ctermbg=Yellow      ctermfg=DarkGrey      guibg=yellow      guifg=black
 HiSet ppmWhite       ctermbg=White       ctermfg=DarkGrey      guibg=white       guifg=black

 HiSet link ppmKeyword Keyword
 delcommand HiSet
endif

let b:current_syntax = "ppm"
