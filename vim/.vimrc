" vim:foldmethod=marker

set nocompatible " don’t be compatible to vi

" Vundle (plugin management) {{{
"----------------------------
" General {{{
filetype off                   " required!
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()
let g:vundle_default_git_proto = 'git'

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
Bundle 'fholgado/minibufexpl.vim'
" Bundle 'sontek/minibufexpl.vim.git' alternative version
" Look at that undo tree!
Bundle 'sjl/gundo.vim.git'
" Quickly open files
Bundle 'kien/ctrlp.vim'
" Generate tags for open files, display functions etc in list
Bundle 'vim-scripts/taglist.vim'
" Highlight TODO, FIXME and XXX and display them in a handy list 
Bundle 'vim-scripts/TaskList.vim.git'
" Vimdiff Awesomeness Mode
Bundle 'sjl/splice.vim'
" }}}
" Behavior {{{
" Don’t kill my window structure when deleting a buffer
Bundle 'vim-scripts/bufkill.vim'
" use cs to replace surroundings
Bundle 'tpope/vim-surround'
" Enable repeating of plugin commands (some)
Bundle 'tpope/vim-repeat'
" Match more than single letters with %, e.g. XML tags
Bundle 'Spaceghost/vim-matchit'
" complete everything by tab
Bundle 'ervandew/supertab.git'
" Autoclose brackets (like Eclipse does)
Bundle 'Townk/vim-autoclose'
" Comment lines out quickly
Bundle 'scrooloose/nerdcommenter'
" Snippets! So many snippets!
"Bundle 'MarcWeber/ultisnips'
"Bundle 'ALX-Liu-Xiao/ultisnips'
"Bundle 'honza/vim-snippets'
Bundle 'SirVer/ultisnips'
Bundle 'MarcWeber/vim-addon-mw-utils.git'
Bundle 'tomtom/tlib_vim.git'
" Handling of URLs
Bundle 'vim-scripts/utl.vim.git'
" Wisely add “end” in ruby, vim, bash, VB, C/C++ preprop, Lua
Bundle 'tpope/vim-endwise'
" Adds indent text object
Bundle 'michaeljsmith/vim-indent-object'
" }}}
" GUI {{{
" Make it solarized, baby
Bundle 'altercation/vim-colors-solarized'
" Show relative line numbers only when not in insert mode
Bundle 'jeffkreeftmeijer/vim-numbertoggle.git'
" RAINBOWS!
Bundle 'kien/rainbow_parentheses.vim'
" }}}
" External Programs {{{
" Integrated syntax checking for lots of languages
Bundle 'scrooloose/syntastic'
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
" Uber plugin
Bundle 'klen/python-mode'
" Show documentation in preview window
Bundle 'fs111/pydoc.vim.git'
" Bundle 'alfredodeza/pytest.vim.git' " Run pytest tests within vim"}}}
" Clojure {{{
" syntax, indent, filetype settings
Bundle 'guns/vim-clojure-static'
" nREPL awesomeness (interactive REPL buffers)
Bundle 'tpope/vim-fireplace'
" Parediting for vim
Bundle 'vim-scripts/paredit.vim'
" }}}
" *ML"{{{
" Crazy cool CSS-like syntax for html
Bundle 'rstacruz/sparkup', {'rtp': 'vim/'}
"}}}
" pandoc"{{{
" Syntax, snippets etc for pandoc’s markdown
Bundle 'vim-pandoc/vim-pandoc.git'
" Bundle 'vim-pandoc/…' " There’s some more, in case of need"
" }}}
" Ascidoc {{{
"Bundle 'dagwieers/asciidoc-vim'
" }}}
" Mediawiki {{{
Bundle 'vim-scripts/mediawiki.vim'
" }}}
" }}}
" }}}
" Enable filetype plugins and indention
filetype plugin indent on
"-------------------------------------}}}

" Constants {{{
" ---------------------------------------
" Font Options
let g:font = "Inconsolata"
let g:fontsize = "11.5"
let g:fontsizestep = "1.5" " Controls how much the size changes when set to small, big or huge
" ------------------------------------}}}

" Abbreviations {{{
" ---------------------------------------
function! EatChar(pat)
    let c = nr2char(getchar(0))
    return (c =~ a:pat) ? '' : c
endfunction

function! MakeSpacelessIabbrev(from, to)
    execute "iabbrev <silent> ".a:from." ".a:to."<C-R>=EatChar('\\s')<CR>"
endfunction
function! MakeSpacelessBufferIabbrev(from, to)
    execute "iabbrev <silent> <buffer> ".a:from." ".a:to."<C-R>=EatChar('\\s')<CR>"
endfunction

call MakeSpacelessIabbrev('@p', 'mail@profpatsch.de')
call MakeSpacelessIabbrev('~p', '~Profpatsch')
" ------------------------------------}}}

" General behaviour {{{
"----------------------------------------
set hidden " Enable switching buffer even if unsaved (should be standard, imho)
set backspace=indent,eol,start " make backspace work like most other apps
set cpoptions+=$ " Add $ at the end of the thing to change
set scrolloff=3 " always keep three lines before or after the cursor

" Backups {{{
set backup                        " enable backups
set noswapfile                    " It's 2012, Vim.
set undofile                      " Yep, DAT persistent history

set undodir=~/.cache/vim/undo//     " undo files
set backupdir=~/.cache/vim/backup// " backups
set directory=~/.cache/vim/swap//   " swap files

" Make those folders automatically if they don't already exist.
if !isdirectory(expand(&undodir))
    call mkdir(expand(&undodir), "p")
endif
if !isdirectory(expand(&backupdir))
    call mkdir(expand(&backupdir), "p")
endif
if !isdirectory(expand(&directory))
    call mkdir(expand(&directory), "p")
endif
" }}}

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
" Select (charwise) the contents of the current line, excluding indentation.
" Great for pasting Python lines into REPLs.
nnoremap vv ^vg_
" Easier linewise reselection
nnoremap <leader>V V`]
" Use <tab> to jump between brackets
nnoremap <tab> %
vnoremap <tab> %
" Folds {{{
" Toggle Folds with <space>
nnoremap <space> za
" "Focus" the current line.  Basically:
" 1. Close all folds.
" 2. Open just the folds containing the current line.
" 3. Move the line to a little bit (15 lines) above the center of the screen.
" 4. Pulse the cursor line.  My eyes are bad.
nnoremap <leader>z mzzMzvzz10<c-e>`z:Pulse<cr>
" }}}
" }}}
" Mappings for the leader {{{
" Files {{{
" Open todo list
nnoremap <leader>todo :e ~/.todo<CR>
" Save session to ~
nnoremap <leader>ssg :mks! ~/.sess<CR>
nnoremap <leader>slg :so ~/.sess<CR>
" Save session to .
nnoremap <leader>ssl :mks! ./.sess<CR>
nnoremap <leader>sll :so ./.sess<CR>
" }}}
" Change global working dir to current file
nnoremap <leader>cd :cd %:p:h<CR>
" Quickly set filetype
nnoremap <leader>ft :set ft=
" Write & source current file
nnoremap <leader>so :w<CR>:so %<CR>
" Clear search marks
nnoremap <leader><space> :noh<cr>
" Toggle display of non-text characters (Toggle Non Text)
nnoremap <leader>tnt :set list!<CR>
" Buffers {{{
" Next and previous buffer, i and e like left and right arrow on Neo-Layout
nnoremap <leader>be  :bn<CR>
nnoremap <leader>bi  :bp<CR>
" Bufferdelete sits conveniently in the middle of bn and bp (use bufkill.vim)
nnoremap <leader>ba  :BD<CR>
" }}}
" Windows {{{
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
" Write and quit window on <leader>q
nnoremap <leader>q :wq<CR>
" }}}
" Spelling (Set ENglish, Set DEeutsch) {{{
nnoremap <leader>sen :set spell spelllang=en_us<CR>
nnoremap <leader>sde :set spell spelllang=de_de<CR>
" }}}
" }}}
" Change filetype {{{
nnoremap _md :set ft=markdown<CR>
nnoremap _p :set ft=pandoc<CR>
nnoremap _h  :set ft=html<CR>
nnoremap _ma :set ft=mail<CR>
nnoremap _v  :set ft=vim<CR>
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
nnoremap <leader>fh :call EditorSetFontSize(float2nr(fontsize+4*str2float(fontsizestep)))<CR>
" }}}
" External commands {{{
" Generate tags for current folder (mostly C++ I guess, kind of redundant with
" the clang plugin
nnoremap <leader>gtcpp :!ctags -R --c++-kinds=+p --fields=+iaS --extra=+q .<CR>
" Opens dir of current file in ranger in urxvt
nnoremap <silent> <leader>rf  :!urxvt -e ranger --selectfile="%:p" &<CR><CR>
" }}}
" Map plugins {{{
let g:ctrlp_map = '<c-p>'
nnoremap <leader>I :call IndentGuides()<CR>
nnoremap <leader>R :RainbowParenthesesToggle<CR>
nnoremap <leader>____ <plug>TaskList
nnoremap <leader>tl :TaskList<CR>
nnoremap <leader>tg :TlistOpen<CR>
nnoremap <leader>tt :TlistToggle<CR>
nnoremap <leader>tu :GundoToggle<CR>
nnoremap <leader>nt :NERDTreeToggle<CR>
" Search current dir with Ack (Search New, Search Previous)
nnoremap <leader>sn :Ack! 
nnoremap <leader>sp :AckFromSearch<CR>
let g:sparkupExecuteMapping='<C-e>'
let g:sparkupNextMapping='<C-n>'
nnoremap <leader>u :Utl<CR>
nnoremap <c-tab> :call UltiSnips_ListSnippets()<CR>
" }}}
" Filetype mappings (should be made file-specific if too many) {{{
" All files
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

set ttyfast " enable smoother screen redraw
set gcr=a:blinkon0 " Disable blinking cursor

" Make the command line two lines high and change the statusline display to
" something that looks useful.
set cmdheight=2
set laststatus=2
set statusline=[%l,%v\ %P%M]\ %f\ %r%h%w\ (%{&ff})\ %{fugitive#statusline()}
set showcmd

set listchars=tab:▸\ ,eol:¬ " Use the same symbols as TextMate for tabstops and EOLs
set showbreak=↪ " Shows a little hint at the beginning of a broken line
set colorcolumn=+1

" Resize splits when the window is resized
au VimResized * :wincmd =

" For solarized colors
syntax on
if has('gui_running')
    set background=dark
    colorscheme solarized
endif
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
"" change list while you type
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
autocmd BufNewFile,BufRead *.rst setlocal ft=rst
autocmd FileType rst setlocal expandtab shiftwidth=4 tabstop=4 softtabstop=4
\ formatoptions+=nqt textwidth=74

" asciidoc
" --------
autocmd BufNewFile,BufRead *.txt,*.asc setlocal ft=asciidoc
let g:AsciidocAutoSave = 1

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
" }}}
" Splice {{{
let g:splice_prefix = "<localleader>"
let g:splice_initial_mode = "compare"
let g:splice_initial_scrollbind_grid = 1
let g:splice_initial_scrollbind_compare = 1

" }}}
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
let g:ctrlp_clear_cache_on_exit = 0
let g:ctrlp_extensions = ['dir']
let g:ctrlp_custom_ignore = '/\.cache/'
let g:ctrlp_follow_symlinks = 1
" }}}
" AutoClose {{{
"}}}
" Pandoc {{{
let g:pandoc_use_hard_wraps = 1
let g:pandoc_auto_format = 1
" }}}
" Highlight Word {{{
"
" This mini-plugin provides a few mappings for highlighting words temporarily.
"
" Sometimes you're looking at a hairy piece of code and would like a certain
" word or two to stand out temporarily.  You can search for it, but that only
" gives you one color of highlighting.  Now you can use <leader>N where N is
" a number from 1-6 to highlight the current word in a specific color.

function! HiInterestingWord(n) " {{{
    " Save our location.
    normal! mz

    " Yank the current word into the z register.
    normal! "zyiw

    " Calculate an arbitrary match ID.  Hopefully nothing else is using it.
    let mid = 86750 + a:n

    " Clear existing matches, but don't worry if they don't exist.
    silent! call matchdelete(mid)

    " Construct a literal pattern that has to match at boundaries.
    let pat = '\V\<' . escape(@z, '\') . '\>'

    " Actually match the words.
    call matchadd("InterestingWord" . a:n, pat, 1, mid)

    " Move back to our original location.
    normal! `z
endfunction " }}}

" Mappings {{{

nnoremap <silent> <leader>1 :call HiInterestingWord(1)<cr>
nnoremap <silent> <leader>2 :call HiInterestingWord(2)<cr>
nnoremap <silent> <leader>3 :call HiInterestingWord(3)<cr>
nnoremap <silent> <leader>4 :call HiInterestingWord(4)<cr>
nnoremap <silent> <leader>5 :call HiInterestingWord(5)<cr>
nnoremap <silent> <leader>6 :call HiInterestingWord(6)<cr>

" }}}
" Default Highlights {{{

hi def InterestingWord1 guifg=#000000 ctermfg=16 guibg=#ffa724 ctermbg=214
hi def InterestingWord2 guifg=#000000 ctermfg=16 guibg=#aeee00 ctermbg=154
hi def InterestingWord3 guifg=#000000 ctermfg=16 guibg=#8cffba ctermbg=121
hi def InterestingWord4 guifg=#000000 ctermfg=16 guibg=#b88853 ctermbg=137
hi def InterestingWord5 guifg=#000000 ctermfg=16 guibg=#ff9eb8 ctermbg=211
hi def InterestingWord6 guifg=#000000 ctermfg=16 guibg=#ff2c4b ctermbg=195

" }}}

" }}}
" Indent Guides {{{
let g:indentguides_state = 0
function! IndentGuides() " {{{
    if g:indentguides_state
        let g:indentguides_state = 0
        2match None
    else
        let g:indentguides_state = 1
        execute '2match IndentGuides /\%(\_^\s*\)\@<=\%(\%'.(0*&sw+1).'v\|\%'.(1*&sw+1).'v\|\%'.(2*&sw+1).'v\|\%'.(3*&sw+1).'v\|\%'.(4*&sw+1).'v\|\%'.(5*&sw+1).'v\|\%'.(6*&sw+1).'v\|\%'.(7*&sw+1).'v\)\s/'
    endif
endfunction " }}}
hi def IndentGuides guibg=#073642

" }}}
" Next and Last {{{
"
" Motion for "next/last object".  "Last" here means "previous", not "final".
" Unfortunately the "p" motion was already taken for paragraphs.
"
" Next acts on the next object of the given type, last acts on the previous
" object of the given type.  These don't necessarily have to be in the current
" line.
"
" Currently works for (, [, {, and their shortcuts b, r, B. 
"
" Next kind of works for ' and " as long as there are no escaped versions of
" them in the string (TODO: fix that).  Last is currently broken for quotes
" (TODO: fix that).
"
" Some examples (C marks cursor positions, V means visually selected):
"
" din'  -> delete in next single quotes                foo = bar('spam')
"                                                      C
"                                                      foo = bar('')
"                                                                C
"
" canb  -> change around next parens                   foo = bar('spam')
"                                                      C
"                                                      foo = bar
"                                                               C
"
" vin"  -> select inside next double quotes            print "hello ", name
"                                                       C
"                                                      print "hello ", name
"                                                             VVVVVV

onoremap an :<c-u>call <SID>NextTextObject('a', '/')<cr>
xnoremap an :<c-u>call <SID>NextTextObject('a', '/')<cr>
onoremap in :<c-u>call <SID>NextTextObject('i', '/')<cr>
xnoremap in :<c-u>call <SID>NextTextObject('i', '/')<cr>

onoremap al :<c-u>call <SID>NextTextObject('a', '?')<cr>
xnoremap al :<c-u>call <SID>NextTextObject('a', '?')<cr>
onoremap il :<c-u>call <SID>NextTextObject('i', '?')<cr>
xnoremap il :<c-u>call <SID>NextTextObject('i', '?')<cr>


function! s:NextTextObject(motion, dir)
    let c = nr2char(getchar())
    let d = ''

    if c ==# "b" || c ==# "(" || c ==# ")"
        let c = "("
    elseif c ==# "B" || c ==# "{" || c ==# "}"
        let c = "{"
    elseif c ==# "r" || c ==# "[" || c ==# "]"
        let c = "["
    elseif c ==# "'"
        let c = "'"
    elseif c ==# '"'
        let c = '"'
    else
        return
    endif

    " Find the next opening-whatever.
    execute "normal! " . a:dir . c . "\<cr>"

    if a:motion ==# 'a'
        " If we're doing an 'around' method, we just need to select around it
        " and we can bail out to Vim.
        execute "normal! va" . c
    else
        " Otherwise we're looking at an 'inside' motion.  Unfortunately these
        " get tricky when you're dealing with an empty set of delimiters because
        " Vim does the wrong thing when you say vi(.

        let open = ''
        let close = ''

        if c ==# "(" 
            let open = "("
            let close = ")"
        elseif c ==# "{"
            let open = "{"
            let close = "}"
        elseif c ==# "["
            let open = "\\["
            let close = "\\]"
        elseif c ==# "'"
            let open = "'"
            let close = "'"
        elseif c ==# '"'
            let open = '"'
            let close = '"'
        endif

        " We'll start at the current delimiter.
        let start_pos = getpos('.')
        let start_l = start_pos[1]
        let start_c = start_pos[2]

        " Then we'll find it's matching end delimiter.
        if c ==# "'" || c ==# '"'
            " searchpairpos() doesn't work for quotes, because fuck me.
            let end_pos = searchpos(open)
        else
            let end_pos = searchpairpos(open, '', close)
        endif

        let end_l = end_pos[0]
        let end_c = end_pos[1]

        call setpos('.', start_pos)

        if start_l == end_l && start_c == (end_c - 1)
            " We're in an empty set of delimiters.  We'll append an "x"
            " character and select that so most Vim commands will do something
            " sane.  v is gonna be weird, and so is y.  Oh well.
            execute "normal! ax\<esc>\<left>"
            execute "normal! vi" . c
        elseif start_l == end_l && start_c == (end_c - 2)
            " We're on a set of delimiters that contain a single, non-newline
            " character.  We can just select that and we're done.
            execute "normal! vi" . c
        else
            " Otherwise these delimiters contain something.  But we're still not
            " sure Vim's gonna work, because if they contain nothing but
            " newlines Vim still does the wrong thing.  So we'll manually select
            " the guts ourselves.
            let whichwrap = &whichwrap
            set whichwrap+=h,l

            execute "normal! va" . c . "hol"

            let &whichwrap = whichwrap
        endif
    endif
endfunction

" }}}
" Ack motions {{{

" Motions to Ack for things.  Works with pretty much everything, including:
"
"   w, W, e, E, b, B, t*, f*, i*, a*, and custom text objects
"
" Awesome.
"
" Note: If the text covered by a motion contains a newline it won't work.  Ack
" searches line-by-line.

nnoremap <silent> <leader>A :set opfunc=<SID>AckMotion<CR>g@
xnoremap <silent> <leader>A :<C-U>call <SID>AckMotion(visualmode())<CR>

" Having Backspace search for stuff is quite confusing. oO
"xnoremap <silent> <bs> :<C-U>call <SID>AckMotion(visualmode())<CR>

function! s:CopyMotionForType(type)
    if a:type ==# 'v'
        silent execute "normal! `<" . a:type . "`>y"
    elseif a:type ==# 'char'
        silent execute "normal! `[v`]y"
    endif
endfunction

function! s:AckMotion(type) abort
    let reg_save = @@

    call s:CopyMotionForType(a:type)

    execute "normal! :Ack! --literal " . shellescape(@@) . "\<cr>"

    let @@ = reg_save
endfunction

" }}}
" Rainbow parentheses {{{
let g:rbpt_colorpairs = [
    \ ['red',    '#dc322f'],
    \ ['green',       '#859900'],
    \ ['cyan', '#2aa198'],
    \ ['blue',     '#268bd2'],
    \ ['magenta',    '#6c71c4'],
    \ ['lightmagenta',   '#d33682'],
    \ ['red',    '#dc322f'],
    \ ['yellow',       '#cb4b16'],
    \ ['lightyellow',    '#b58900'],
    \ ['cyan', '#2aa198'],
    \ ['blue',     '#268bd2'],
    \ ['magenta',    '#6c71c4'],
    \ ['lightmagenta',   '#d33682'],
    \ ['yellow',       '#cb4b16'],
    \ ['lightyellow',    '#b58900'],
    \ ['green',       '#859900'],
    \ ]
let g:rbpt_max = 16
au Syntax * RainbowParenthesesLoadRound
au Syntax * RainbowParenthesesLoadSquare
au Syntax * RainbowParenthesesLoadBraces
" }}}
" UltiSnips {{{
"let g:UltiSnipsSnippetsDir="~/code/vim/vim-snippets-private/"
let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<tab>"
let g:UltiSnipsJumpBackwardTrigger="<s-tab>"
let g:UltiSnipsListSnippets="<leader>mm"
" }}}
" Python-Mode {{{
" }}}
" Paredit {{{
let g:paredit_smartjump = 1
"let g:paredit_leader = "\\"
let g:paredit_shortmaps = 1
" }}}
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

" Bda -  Delete all open buffers. {{{
command! -nargs=0 -bang Bda
    \ :call BufDeleteAll('<bang>')

function! BufDeleteAll(bang)
    let last_buffer = bufnr('$')

    let n = 1
    while n <= last_buffer
        if a:bang == '' && getbufvar(n, '&modified')
            echohl ErrorMsg
            echomsg 'No write since last change for buffer'
                        \ n '(add ! to override)'
            echohl None
            return 0
        endif
        let n = n+1
    endwhile

    let delete_count = 0
    let n = 1
    while n <= last_buffer
        if buflisted(n)
            silent exe 'bdel' . a:bang . ' ' . n
            if ! buflisted(n)
                let delete_count = delete_count+1
            endif
        endif
        let n = n+1
    endwhile

    if delete_count == 1
        echomsg delete_count "buffer deleted"
    elseif delete_count > 1
        echomsg delete_count "buffers deleted"
    endif

endfunction
" }}}
" ------------------------------------}}}
