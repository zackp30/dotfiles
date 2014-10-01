setlocal spell
setlocal tabstop=4
let g:table_mode_corner_corner = "+"
" syntax match PandocCheckbox "\makebox[0pt][l]{$\square$}\raisebox{.15ex}{\hspace{0.1em}$\checkmark$}" conceal cchar=☑
syntax match PandocCheckbox "e" contained conceal cchar=☑
