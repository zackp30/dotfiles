setlocal sw=4 st=4
setlocal smartindent
setlocal softtabstop=4
RainbowParenthesesToggleAll
setlocal foldmethod=marker
call SyntaxRange#Include('{python}', '\\end{minted}', 'python', 'NonText')
call SyntaxRange#Include('{rust}', '\\end{minted}', 'rust', 'NonText')
call SyntaxRange#Include('{ruby}', '\\end{minted}', 'ruby', 'NonText')
