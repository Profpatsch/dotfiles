setlocal tabstop=8
setlocal softtabstop=2
setlocal shiftwidth=2
setlocal expandtab
setlocal autoindent

setlocal textwidth=80
setlocal wrap
setlocal formatoptions=tcqn
setlocal formatlistpat=^\\s*\\d\\+\\.\\s\\+\\\\|^\\s*<\\d\\+>\\s\\+\\\\|^\\s*[a-zA-Z.]\\.\\s\\+\\\\|^\\s*[ivxIVX]\\+\\.\\s\\+
setlocal comments=s1:/*,ex:*/,://,b:#,:%,:XCOMM,fb:-,fb:*,fb:+,fb:.,fb:>

nnoremap <F2> gq}
nnoremap <F1> gqq<down>

"Theme, one of - (none), flask, volnitsky
let s:AsciidocHtmlTheme = "volnitsky"
let s:AsciidocRenderHtml= "silent !asciidoc -b html5 -a icons -n --theme "
let s:AsciidocOpenBrowser = ' "%" && chromium "%:r".html & '

if !exists("g:AsciidocAutoSave")
  let g:AsciidocAutoSave = 0
endif

fun! AsciidocRefresh()
  if g:AsciidocAutoSave == 1
    if exists("b:AsciidocGenerated")
      execute 'write'
      execute s:AsciidocRenderHtml.s:AsciidocHtmlTheme.' "%"'
    endif
  endif
endf

fun! AsciidocOpen()
  execute 'write'
  execute s:AsciidocRenderHtml.s:AsciidocHtmlTheme.s:AsciidocOpenBrowser
  let b:AsciidocGenerated = 1
endf

nnoremap <leader>h :call AsciidocOpen()<CR>
autocmd FileType asciidoc :autocmd! FocusLost <buffer> call AsciidocRefresh()
