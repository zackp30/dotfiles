if exists("current_compiler")
    finish
endif
let current_compiler = "scribble"
if exists(":CompilerSet") != 2
    command -nargs=* CompilerSet setlocal <args>
endif
CompilerSet errorformat&
CompilerSet makeprg=scribble\ --prefix\ ~/gitlab/tex/texdocuments/ZackScribLib.tex\ --pdf\ %
