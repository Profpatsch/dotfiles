setlocal textwidth=80
setlocal formatoptions=tcqj

let b:AutoClosePairs = AutoClose#DefaultPairsModified("", "'")

nnoremap <localleader>t :A<CR>

" Midje indenting
setlocal lispwords+=facts
setlocal lispwords+=fact
