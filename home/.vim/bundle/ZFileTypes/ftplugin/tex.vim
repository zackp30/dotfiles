setlocal sw=4 st=4
set smartindent
set softtabstop=4
RainbowParenthesesToggleAll
set foldmethod=marker
call SyntaxRange#Include('{python}', '\\end{minted}', 'python', 'NonText')
call SyntaxRange#Include('{rust}', '\\end{minted}', 'rust', 'NonText')
call SyntaxRange#Include('{ruby}', '\\end{minted}', 'ruby', 'NonText')
