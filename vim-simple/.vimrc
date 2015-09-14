" vim:foldmethod=marker

set nocompatible " don’t be compatible to vi

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

set ttyfast " enable smoother screen redraw
set gcr=a:blinkon0 " Disable blinking cursor

" Make the command line two lines high and change the statusline display to
" something that looks useful.
set cmdheight=2
set laststatus=2
set showcmd

set listchars=tab:▸\ ,eol:¬ " Use the same symbols as TextMate for tabstops and EOLs
set showbreak=↪ " Shows a little hint at the beginning of a broken line
set colorcolumn=+1

" Syntax hilighting
syntax on

" Resize splits when the window is resized
au VimResized * :wincmd =
"-------------------------------------}}}
