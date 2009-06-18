colorscheme railscasts
syntax on           " syntax highlighting, please
set number          " show line numbers
set softtabstop=2   " tabs are 2 spaces
set shiftwidth=2    " auto-indent is 2 spaces
set expandtab       " use spaces instead of tabs
set ai              " always autoindent
set ruler           " always show cursor position
set laststatus=2    " always show the status line
set statusline=%<%f\ %h%m%r%=%-14.(%l,%c%V%)\ %P " various fancy status line stuff
" set cursorline      " highlight the line with the cursor

" taken from jamis buck, tabbing related
set tabstop=2
set smarttab
set backspace=start,indent

" recognize more kinds of ruby files
autocmd BufRead *.rake set filetype=ruby
autocmd BufRead Rakefile set filetype=ruby
autocmd BufRead *.erb set filetype=eruby
autocmd BufRead Capfile set filetype=ruby
" also recognize todo files
autocmd BufRead *.todo set filetype=tasks

" toggle display of scratch
function! ToggleScratch()
  if expand('%') == g:ScratchBufferName
    quit
  else
    Sscratch
  endif
endfunction

" \s will toggle scratch display
map <leader>s :call ToggleScratch()<CR>
" \d will show or refresh nerd tree
map <leader>d :NERDTreeToggle code<CR>



