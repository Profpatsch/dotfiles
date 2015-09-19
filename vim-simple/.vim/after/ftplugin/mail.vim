setlocal textwidth=72

" Remap G to stop in front of the signature and go to insert mode.
nmap  G     /\n--\ \n<Return>o<ESC>O
nmap  dG    d/\n--\ \n<Return>o<ESC>O

" Some tricks for mutt
" F1 through F3 re-wraps paragraphs in useful ways
nmap  <F1>  gqap
nmap  <F2>  gqqj
nmap  <F3>  kgqj
imap  <F1>  <ESC>gqapi
imap  <F2>  <ESC>gqqji
imap  <F2>  <ESC>kgqji
