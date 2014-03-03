" Basic colors from $VIMRUNTIME/rgb.txt
let colors = {
  \ 'k' : '0 0 0',
  \ 'b' : '0 0 139',
  \ 'g' : '0 100 0',
  \ 'c' : '0 139 139',
  \ 'r' : '139 0 0',
  \ 'm' : '139 0 139',
  \ 'y' : '165 42 42',
  \ 'w' : '211 211 211',
  \ 'K' : '169 169 169',
  \ 'B' : '0 0 255',
  \ 'G' : '0 255 0',
  \ 'C' : '0 255 255',
  \ 'R' : '255 0 0',
  \ 'M' : '255 0 255',
  \ 'Y' : '255 255 0',
  \ 'W' : '255 255 255'}

" Append lines to current buffer consisting of header (three lines):
"   'P3' = PPM (full color; numbers in ASCII decimal)
"   width height (two numbers)
"   color_depth  (one number)
" then lines from translating each input letter to its color triple.
" We translate only the letters that follow "data" at the start of a line.
" Width = number of letters in first data item; height = number data items.
function! AppendColors(data_file) abort
  let data = readfile(a:data_file)
  call filter(data, 'v:val=~#''^data\s''')
  call map(data, 'split(v:val)[1:]')
  let width_height = printf('%d %d', len(data[0]), len(data))
  call append(line('$'), ['P3', width_height, '255'])
  for columns in data
    call map(columns, 'g:colors[v:val]')
    call append(line('$'), columns)
  endfor
endfunction
command! -nargs=1 -complete=file AppendColors call AppendColors('<args>')
