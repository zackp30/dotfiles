compiler scribble
setlocal softtabstop=4
nnoremap <buffer> <leader>ll :make<cr>
RainbowParenthesesToggleAll
hi SpecialComment guifg=#BD9800 guibg=NONE guisp=NONE gui=NONE ctermfg=68 ctermbg=NONE cterm=NONE
call SyntaxRange#Include('@exact{', '@;END', 'tex', 'NonText')
