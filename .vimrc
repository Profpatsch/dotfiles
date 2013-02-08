set nocompatible

"" Pathogen (plugin management)
call pathogen#runtime_append_all_bundles()
call pathogen#helptags()
call pathogen#infect()
"" Enable filetype plugins and indention
filetype plugin indent on

"" Mappings
"----------------------------------------
" Der Fuhrer
let mapleader = ","
" split edit vimrc
nnoremap <leader>vrc <C-w><C-v><C-w><C-l>:e $MYVIMRC<CR>
" Insert blank line w/o entering insert mode
nnoremap <CR> o<Esc>0D
" Make Y act like D or C
nnoremap Y y$
" Quick Window switching
nnoremap <leader>i <C-w>h
nnoremap <leader>a <C-w>j
nnoremap <leader>l <C-w>k
nnoremap <leader>e <C-w>l
" Quit window on <leader>q
nnoremap <leader>q :q<CR>
" Map plugins
nnoremap <leader>o :CommandT<CR>
nnoremap <leader>td <Plug>TaskList
nnoremap <leader>tg :TlistOpen<CR>
nnoremap <leader>tt :TlistToggle<CR>
nnoremap <leader>rh :GundoToggle<CR>
let g:pep8_map='<leader>pep'
nnoremap <leader>t :NERDTreeToggle<CR>
nnoremap <leader>j :call RopeGotoDefinition()
nnoremap <leader>r :RopeRename<CR>
nnoremap <leader>ck <Esc>:Ack!
" Filetype mappings (should be made file-specific if too many)
" markdown headers
nnoremap <leader>m1 VypVr=
"----------------------------------------

" Doesn’t overwrite old files :(
" Move Backup Files to ~/.vim/sessions
" set backupdir=~/.vim/sessions//
" set dair=~/.vim/sessions//

" Enable switching buffer even if unsaved (should be standard, imho)
set hidden

"" make backspace work like most other apps
set backspace=2 
"" Add $ at the end of the thing to change
set cpoptions+=$

"" Searching
" Show search results immediately.
set incsearch
set hlsearch
" Search smartly (small&capital, only capital if explicitly used)
set ignorecase
set smartcase

"" Tab Settings
set smarttab
set tabstop=8

"" Kill everyone who doesn’t want this.
set enc=utf-8

"" prefer unix over windows over os9 formats
set fileformats=unix,dos,mac

"" Autocompletion
"----------------------------------------
autocmd FileType python set omnifunc=python3complete#Complete
autocmd FileType javascript set omnifunc=javascriptcomplete#CompleteJS
autocmd FileType html set omnifunc=htmlcomplete#CompleteTags
autocmd FileType css set omnifunc=csscomplete#CompleteCSS
autocmd FileType xml set omnifunc=xmlcomplete#CompleteTags
autocmd FileType php set omnifunc=phpcomplete#CompletePHP
autocmd FileType c set omnifunc=ccomplete#Complete

" If you prefer the Omni-Completion tip window to close when a selection is
" made, these lines close it on movement in insert mode or when leaving
" insert mode
autocmd CursorMovedI * if pumvisible() == 0|pclose|endif
"autocmd InsertLeave * if pumvisible() == 0|pclose|endif
" With supertab option (doesn’t work as of 2012-11)
"let g:SuperTabClosePreviewOnPopupClose = 1

" Commandline autocompletion
set wildmenu
set wildignore=*.dll,*.o,*.pyc,*.bak,*.exe,*.jpg,*.jpeg,*.png,*.gif,*$py.class,*.class
set wildmode=list:full

" Autocompletion in edit-mode
" context-sensitive
let g:SuperTabDefaultCompletionType = "context"
" change list while you type
set completeopt=preview,longest,menuone
let g:SuperTabLongestEnhanced = 1
let g:SuperTabLongestHighlight = 1
"----------------------------------------

"" Tags
set tags+=~/.vim/tags/opencv.tags
set tags+=~/.vim/tags/opencv2.tags

"" Save files
setglobal fileencoding=utf-8 "Everything else should be dead or running…"

"" Set filetypes for extensions
autocmd BufNewFile,BufRead *.muttrc setlocal filetype=muttrc

"" Language support (should be in .vim/after/ftplugin/)
"" ----------------------------------------------------

" ruby support
" ------------
autocmd FileType ruby setlocal expandtab shiftwidth=2 tabstop=2 softtabstop=2

" go support
" ----------
autocmd BufNewFile,BufRead *.go setlocal ft=go
autocmd FileType go setlocal expandtab shiftwidth=4 tabstop=8 softtabstop=4

" php support
" -----------
autocmd FileType php setlocal shiftwidth=4 tabstop=8 softtabstop=4 expandtab

" template language support (SGML / XML too)
" ------------------------------------------
" and disable taht stupid html rendering (like making stuff bold etc)

fun! s:SelectHTML()
let n = 1
while n < 50 && n < line("$")
  " check for jinja
  if getline(n) =~ '{%\s*\(extends\|block\|macro\|set\|if\|for\|include\|trans\)\>'
    set ft=htmljinja
    return
  endif
  " check for mako
    if getline(n) =~ '<%\(def\|inherit\)'
      set ft=mako
      return
    endif
    " check for genshi
    if getline(n) =~ 'xmlns:py\|py:\(match\|for\|if\|def\|strip\|xmlns\)'
      set ft=genshi
      return
    endif
    let n = n + 1
  endwhile
  " go with html
  set ft=html
endfun

autocmd FileType html,xhtml,xml,htmldjango,htmljinja,eruby,mako setlocal expandtab shiftwidth=2 tabstop=2 softtabstop=2
autocmd bufnewfile,bufread *.rhtml setlocal ft=eruby
autocmd BufNewFile,BufRead *.mako setlocal ft=mako
autocmd BufNewFile,BufRead *.tmpl setlocal ft=htmljinja
autocmd BufNewFile,BufRead *.py_tmpl setlocal ft=python
autocmd BufNewFile,BufRead *.html,*.htm  call s:SelectHTML()
let html_no_rendering=1

let g:closetag_default_xml=1
let g:sparkupNextMapping='<c-l>'
autocmd FileType html,htmldjango,htmljinja,eruby,mako let b:closetag_html_style=1
autocmd FileType html,xhtml,xml,htmldjango,htmljinja,eruby,mako source ~/.vim/scripts/closetag.vim

" GLSL
" ----
autocmd bufnewfile,bufread *.frag,*.fragment,*.vert,*.vertex,*.shader,*.glsl setlocal ft=glsl
autocmd FileType glsl setlocal expandtab shiftwidth=4 tabstop=8 softtabstop=4

" Verilog
" -------
autocmd FileType verilog setlocal expandtab shiftwidth=2 tabstop=8 softtabstop=2

" CSS
" ---
autocmd FileType css setlocal expandtab shiftwidth=4 tabstop=4 softtabstop=4

" Java
" ----
autocmd FileType java setlocal shiftwidth=2 tabstop=8 softtabstop=2 expandtab

" rst
" ---
autocmd BufNewFile,BufRead *.txt setlocal ft=rst
autocmd FileType rst setlocal expandtab shiftwidth=4 tabstop=4 softtabstop=4
\ formatoptions+=nqt textwidth=74

" C#
autocmd FileType cs setlocal tabstop=8 softtabstop=4 shiftwidth=4 expandtab

" /Obj-C/C++
autocmd FileType cpp setlocal tabstop=4 softtabstop=4 shiftwidth=4 expandtab
autocmd FileType objc setlocal tabstop=4 softtabstop=4 shiftwidth=4 expandtab
let c_no_curly_error=1

" Octave/Matlab
autocmd FileType matlab setlocal tabstop=8 softtabstop=2 shiftwidth=2 expandtab

" vim
" ---
autocmd FileType vim setlocal expandtab shiftwidth=2 tabstop=8 softtabstop=2

" Javascript
" ----------
autocmd FileType javascript setlocal expandtab shiftwidth=2 tabstop=2 softtabstop=2
autocmd BufNewFile,BufRead *.json setlocal ft=javascript
let javascript_enable_domhtmlcss=1

" CoffeeScript
" ------------
autocmd FileType coffee setlocal expandtab shiftwidth=2 tabstop=2 softtabstop=2

" D
" -
autocmd FileType d setlocal expandtab shiftwidth=4 tabstop=8 softtabstop=4

" cmake support
" -------------
autocmd BufNewFile,BufRead CMakeLists.txt setlocal ft=cmake

" Erlang support
" --------------
autocmd FileType erlang setlocal expandtab shiftwidth=2 tabstop=8 softtabstop=2
autocmd BufNewFile,BufRead rebar.config setlocal ft=erlang

" YAML support
" ------------
autocmd FileType yaml setlocal expandtab shiftwidth=2 tabstop=8 softtabstop=2
autocmd BufNewFile,BufRead *.sls setlocal ft=yaml

" Lua support
" -----------
autocmd FileType lua setlocal shiftwidth=2 tabstop=2 softtabstop=2

" rust
" ----
autocmd FileType rust setlocal expandtab shiftwidth=4 tabstop=8 softtabstop=4
" ---------------------------------------------------t


"" GUI

" GVim
:set guioptions-=m  "remove menu bar
:set guioptions-=T  "remove toolbar
:set guioptions-=r  "remove right-hand scroll bar
" Make the command line two lines high and change the statusline display to
" something that looks useful.
set cmdheight=2
set laststatus=2
set statusline=[%l,%v\ %P%M]\ %f\ %r%h%w\ (%{&ff})\ %{fugitive#statusline()}
set showcmd
if has('gui_running')
    set background=dark
    colorscheme solarized
    set guifont=Inconsolata\ 10
endif

" Disable blinking cursor
set gcr=a:blinkon0

syntax on

" For solarized colors
if $COLORTERM == 'gnome-terminal'
  set t_Co=256
endif
syntax enable



"" Plugins

" Pydoc
let g:pydoc_cmd = 'python -m pydoc' 

let g:pyflakes_use_quickfix = 0

" cscope

if has('cscope')
  set cscopetag cscopeverbose

  if has('quickfix')
    set cscopequickfix=s-,c-,d-,i-,t-,e-
  endif

  cnoreabbrev <expr> csa
        \ ((getcmdtype() == ':' && getcmdpos() <= 4)? 'cs add'  : 'csa')
  cnoreabbrev <expr> csf
        \ ((getcmdtype() == ':' && getcmdpos() <= 4)? 'cs find' : 'csf')
  cnoreabbrev <expr> csk
        \ ((getcmdtype() == ':' && getcmdpos() <= 4)? 'cs kill' : 'csk')
  cnoreabbrev <expr> csr
        \ ((getcmdtype() == ':' && getcmdpos() <= 4)? 'cs reset' : 'csr')
  cnoreabbrev <expr> css
        \ ((getcmdtype() == ':' && getcmdpos() <= 4)? 'cs show' : 'css')
  cnoreabbrev <expr> csh
        \ ((getcmdtype() == ':' && getcmdpos() <= 4)? 'cs help' : 'csh')

  command -nargs=0 Cscope cs add $VIMSRC/src/cscope.out $VIMSRC/src
endif

" TagList
let Tlist_Process_File_Always=1 
let Tlist_Show_One_File=1
"let Tlist_Use_Right_Window=1
let Tlist_WinWidth=40

"" Functions

" Automatically create a new dir when saving to a non-existing path
function s:MkNonExDir(file, buf)
    if empty(getbufvar(a:buf, '&buftype')) && a:file!~#'\v^\w+\:\/'
        let dir=fnamemodify(a:file, ':h')
        if !isdirectory(dir)
            call mkdir(dir, 'p')
        endif
    endif
endfunction
augroup BWCCreateDir
    autocmd!
    autocmd BufWritePre * :call s:MkNonExDir(expand('<afile>'), +expand('<abuf>'))
augroup END
