" vim:foldmethod=marker
set nocompatible " don’t be compatible to vi

" Vundle (plugin management) {{{
"----------------------------
" General {{{
filetype off                   " required!
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

" let Vundle manage Vundle
" required! 
Bundle 'gmarik/vundle'
" }}}
" Bundles {{{
" Editor Extensions {{{
" Awesome status line
Bundle 'Lokaltog/powerline'
" Well, it’s the NERD tree. No explanation needed.
Bundle 'vim-scripts/The-NERD-tree.git'
" small buffer list
Bundle 'sontek/minibufexpl.vim.git'
" Look at that undo tree!
Bundle 'sjl/gundo.vim.git'
" Quickly open files
Bundle 'kien/ctrlp.vim'
" Generate tags for open files, display functions etc in list
Bundle 'vim-scripts/taglist.vim'
" Highlight TODO, FIXME and XXX and display them in a handy list 
Bundle 'vim-scripts/TaskList.vim.git'
" }}}
" Behavior {{{
" use cs to replace surroundings
Bundle 'tpope/vim-surround'
" Enable repeating of plugin commands (some)
Bundle 'tpope/vim-repeat'
" complete everything by tab
Bundle 'ervandew/supertab.git'
" Autoclose brackets (like Eclipse does)
Bundle 'spf13/vim-autoclose'
" Comment lines out quickly
Bundle 'scrooloose/nerdcommenter'
" Snippets! So many snippets!
Bundle 'MarcWeber/vim-addon-mw-utils.git'
Bundle 'tomtom/tlib_vim.git'
Bundle 'spf13/snipmate.vim.git'
Bundle 'spf13/snipmate-snippets'
" Handling of URLs
Bundle 'vim-scripts/utl.vim.git'
" }}}
" GUI {{{
" Make it solarized, baby
Bundle 'altercation/vim-colors-solarized'
" Show relative line numbers only when not in insert mode
Bundle 'jeffkreeftmeijer/vim-numbertoggle.git'
" }}}
" External Programs {{{
" Git integration
Bundle 'tpope/vim-fugitive.git'
" Use :Ack, a grep replacement for programmers
Bundle 'mileszs/ack.vim.git'
" }}}
" Languages {{{
" Vim-Orgmode"{{{
Bundle 'jceb/vim-orgmode'
"}}}
" C++"{{{
Bundle 'Rip-Rip/clang_complete'
"}}}
" python"{{{
" Rope refactoring
Bundle 'sontek/rope-vim.git'
" Check for pep8 compatibility
Bundle 'vim-scripts/pep8.git'
" Show documentation in preview window
Bundle 'fs111/pydoc.vim.git'
" Bundle 'alfredodeza/pytest.vim.git' " Run pytest tests within vim"}}}
" *ML"{{{
" Crazy cool CSS-like syntax for html
Bundle 'rstacruz/sparkup', {'rtp': 'vim/'}
"}}}
" pandoc"{{{
" Syntax, snippets etc for pandoc’s markdown
Bundle 'vim-pandoc/vim-pandoc.git'
" Bundle 'vim-pandoc/…' " There’s some more, in case of need"
" }}}
" }}}
" }}}
" Enable filetype plugins and indention
filetype plugin indent on
"-------------------------------------}}}

" Constants {{{
" Font Options
let g:font = "Inconsolata"
let g:fontsize = "11.5"
let g:fontsizestep = "1.5" " Controls how much the size changes when set to small, big or huge
" ------------------------------------}}}

" General behaviour {{{
"----------------------------------------
set hidden " Enable switching buffer even if unsaved (should be standard, imho)
set backspace=indent,eol,start " make backspace work like most other apps
set cpoptions+=$ " Add $ at the end of the thing to change
set scrolloff=3 " always keep three lines before or after the cursor

" Persistent undo by file saved to cache. HECK YEAH!
set undofile
set undodir=~/.cache/vim/undo

"" Searching
" Show search results immediately.
set incsearch
set hlsearch
" Search smartly (small&capital, only capital if explicitly used)
set ignorecase
set smartcase

" Substitution
set gdefault " replace all occurences on a line, only one with …/g

"" Tab Settings
set smarttab
set tabstop=8

"" prefer unix over windows over os9 formats
set fileformats=unix,dos,mac

"" Kill everyone who doesn’t want this.
set enc=utf-8
"" Save files
setglobal fileencoding=utf-8 "Everything else should be dead or running…"
au FocusLost * :wa " Save everything when using focus (persistent undo on ;)
"-------------------------------------}}}

" Mappings {{{
"----------------------------------------
" General {{{
" Der Fuhrer
let mapleader = ","
" split edit vimrc (vrc=VimRC)
nnoremap <leader>vrc <C-w><C-v><C-w><C-l>:e $MYVIMRC<CR>
" }}}
" New/changed meanings of keys {{{
" Insert blank line w/o entering insert mode with <Return>
nnoremap <CR> o<Esc>0D
" Make Y act like D or C
nnoremap Y y$
" Use <tab> to jump between brackets
nnoremap <tab> %
vnoremap <tab> %
" Toggle Folds with <space>
nnoremap <space> za
" }}}
" Mappings for the leader {{{
"" Files
" Open todo list
nnoremap <leader>todo :e ~/.todo<CR>
" Save session to ~
nnoremap <leader>sessg :mks! ~/.sess
" Save session to .
nnoremap <leader>sessl :mks! .sess
"" Windows
" Make every window command availble with <leader>w
nnoremap <leader>w <C-w>
" New vertical split with focus change (Window Vertical)
nnoremap <leader>wv <C-w>v<C-w>l
" Resize the windows to be equally large (Window Equalize)
nnoremap <leader>we <C-w>=
" Quick Window switching (same keys as arrow keys on Neo-Layout)
nnoremap <leader>i <C-w>h
nnoremap <leader>a <C-w>j
nnoremap <leader>l <C-w>k
nnoremap <leader>e <C-w>l
" Quit window on <leader>q
nnoremap <leader>q :q<CR>
" Clear search marks
nnoremap <leader><space> :noh<cr>
" Toggle display of non-text characters (Toggle List)
nnoremap <leader>tl :set list!<CR>
" Spelling (Set ENglish, Set DEeutsch)
nnoremap <leader>sen :set spell spelllang=en_us<CR>
nnoremap <leader>sde :set spell spelllang=de_de<CR>
" }}}
" GUI {{{
function! EditorSetFontSize(size)
  :let &guifont = g:font . " " . a:size
endfunction
" Dynamically set font size (small, normal, big, huge).
" Set values in the Constants section.
nnoremap <leader>fs :call EditorSetFontSize(float2nr(str2float(fontsize)-str2float(fontsizestep)))<CR>
nnoremap <leader>fn :call EditorSetFontSize(fontsize)<CR>
nnoremap <leader>fb :call EditorSetFontSize(float2nr(str2float(fontsize)+str2float(fontsizestep)))<CR>
nnoremap <leader>fh :call EditorSetFontSize(float2nr(fontsize+3*str2float(fontsizestep)))<CR>
" }}}
" External commands {{{
" Generate tags for current folder (mostly C++ I guess, kind of redundant with
" the clang plugin
nnoremap <leader>gtcpp :!ctags -R --c++-kinds=+p --fields=+iaS --extra=+q .<CR>
" }}}
" Map plugins {{{
let g:ctrlp_map = '<c-p>'
nnoremap <leader>td <Plug>TaskList
nnoremap <leader>tg :TlistOpen<CR>
nnoremap <leader>tt :TlistToggle<CR>
nnoremap <leader>tu :GundoToggle<CR>
let g:pep8_map='<leader>pep'
nnoremap <leader>nt :NERDTreeToggle<CR>
nnoremap <leader>j :call RopeGotoDefinition()
nnoremap <leader>r :RopeRename<CR>
nnoremap <leader>ck <Esc>:Ack!
let g:sparkupExecuteMapping='<C-e>'
let g:sparkupNextMapping='<C-n>'
nnoremap <leader>u :Utl<CR>
" }}}
" Filetype mappings (should be made file-specific if too many) {{{
" markdown headers
nnoremap <leader>m1 VypVr=
" }}}
"-------------------------------------}}}

" GUI {{{
"----------------------------------------
" GVim
set guioptions-=m  "remove menu bar
set guioptions-=T  "remove toolbar
set guioptions-=r "remove right-hand scroll bar
set guioptions-=R "remove right-hand scroll bar
set guioptions-=l "remove left-hand scroll bar
set guioptions-=L "remove left-hand scroll bar
call EditorSetFontSize(fontsize)

" Make the command line two lines high and change the statusline display to
" something that looks useful.

set ttyfast " enable smoother screen redraw
set cmdheight=2
set laststatus=2
set listchars=tab:▸\ ,eol:¬ " Use the same symbols as TextMate for tabstops and EOLs
set statusline=[%l,%v\ %P%M]\ %f\ %r%h%w\ (%{&ff})\ %{fugitive#statusline()}
set showcmd
if has('gui_running')
    set background=dark
    colorscheme solarized
endif

" Disable blinking cursor
set gcr=a:blinkon0

syntax on
" For solarized colors
if $COLORTERM == 'gnome-terminal'
  set t_Co=256
endif
syntax enable
"-------------------------------------}}}

" Autocompletion {{{
"----------------------------------------
autocmd FileType python set omnifunc=python3complete#Complete
autocmd FileType javascript set omnifunc=javascriptcomplete#CompleteJS
autocmd FileType html set omnifunc=htmlcomplete#CompleteTags
autocmd FileType css set omnifunc=csscomplete#CompleteCSS
autocmd FileType xml set omnifunc=xmlcomplete#CompleteTags
autocmd FileType php set omnifunc=phpcomplete#CompletePHP
autocmd FileType c set omnifunc=ccomplete#Complete

" Limit popup menu height
set pumheight=15

" If you prefer the Omni-Completion tip window to close when a selection is
" made, these lines close it on movement in insert mode or when leaving
" insert mode
" autocmd CursorMovedI * if pumvisible() == 0|pclose|endif
"autocmd InsertLeave * if pumvisible() == 0|pclose|endif

" Commandline autocompletion
set wildmenu
set wildignore=*.dll,*.o,*.pyc,*.bak,*.exe,*.jpg,*.jpeg,*.png,*.gif,*$py.class,*.class,*/tmp/*,*.so,*.swp,*.zip
set wildmode=list:longest

" Autocompletion in edit-mode
" context-sensitive
let g:SuperTabDefaultCompletionType = "context"
" change list while you type
set completeopt=preview,longest,menuone
let g:SuperTabLongestEnhanced = 1
let g:SuperTabLongestHighlight = 1

" clang completion
let g:clang_use_library = 1 " Use the library directly (faster)
let g:clang_complete_auto = 0 " Disable auto popup
let g:clang_complete_copen = 1 " Show clang error in quickfix window

"-------------------------------------}}}

" Tags {{{
set tags+=~/.vim/tags/cpplib
set tags+=~/.vim/tags/includes
set tags+=~/.vim/tags/opencv2
autocmd BufNewFile,BufRead * setlocal tags+=./tags " Set tags of the current folder"}}}

" Filetypes for extensions {{{
autocmd BufNewFile,BufRead *.muttrc setlocal filetype=muttrc
autocmd BufNewFile,BufRead *.mail setlocal filetype=mail"}}}

" Language support (should be in .vim/after/ftplugin/) {{{
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
" ------------------------------------}}}

" Plugins {{{
" ---------------------------------------
" Powerline {{{
set rtp+=~/.vim/bundle/powerline/powerline/bindings/vim
" Pydoc {{{
let g:pydoc_cmd = 'python -m pydoc' 

let g:pyflakes_use_quickfix = 0
" }}}
" cscope {{{
" 
" if has('cscope')
"   set cscopetag cscopeverbose
" 
"   if has('quickfix')
"     set cscopequickfix=s-,c-,d-,i-,t-,e-
"   endif
" 
"   cnoreabbrev <expr> csa
"         \ ((getcmdtype() == ':' && getcmdpos() <= 4)? 'cs add'  : 'csa')
"   cnoreabbrev <expr> csf
"         \ ((getcmdtype() == ':' && getcmdpos() <= 4)? 'cs find' : 'csf')
"   cnoreabbrev <expr> csk
"         \ ((getcmdtype() == ':' && getcmdpos() <= 4)? 'cs kill' : 'csk')
"   cnoreabbrev <expr> csr
"         \ ((getcmdtype() == ':' && getcmdpos() <= 4)? 'cs reset' : 'csr')
"   cnoreabbrev <expr> css
"         \ ((getcmdtype() == ':' && getcmdpos() <= 4)? 'cs show' : 'css')
"   cnoreabbrev <expr> csh
"         \ ((getcmdtype() == ':' && getcmdpos() <= 4)? 'cs help' : 'csh')
" 
"   command -nargs=0 Cscope cs add $VIMSRC/src/cscope.out $VIMSRC/src
" endif
" }}}
" TagList {{{
let Tlist_Process_File_Always=1 
let Tlist_Show_One_File=1
"let Tlist_Use_Right_Window=1
let Tlist_WinWidth=40
" }}}
" CtrlP {{{
let g:ctrlp_cmd = 'CtrlP'
let g:ctrlp_working_path_mode = 'ra' " Set CtrlP’s working directory, see help
let g:ctrlp_custom_ignore = '\v[\/]\.(git|hg|svn)$' " Ignore version control
let g:ctrlp_show_hidden = 1
" }}}
" AutoClose {{{
" Don’t double `"` in vim files (used as comments)
let g:autoclose_vim_commentmode = 1
"}}}
" ------------------------------------}}}

" Functions {{{
" ---------------------------------------
" Automatically create a new dir when saving to a non-existing path
function! s:MkNonExDir(file, buf)
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
" ------------------------------------}}}
