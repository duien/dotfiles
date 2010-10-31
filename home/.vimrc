set number          " show line numbers
set softtabstop=2   " tabs are 2 spaces
set shiftwidth=2    " auto-indent is 2 spaces
set tabstop=2       " taken from jamis buck
set smarttab        " taken from jamis buck
set expandtab       " use spaces instead of tabs
set ai              " always autoindent
set ruler           " always show cursor position
set laststatus=2    " always show the status line
set statusline=%<%f\ %h%m%r%=%-14.(%l,%c%V%)\ %P " various fancy status line stuff
" set cursorline      " highlight the line with the cursor
set backspace=start,indent,eol
set linebreak       " wrap lines at word boundaries

let ruby_operators=1 " highlight operators in ruby

colorscheme railscasts
syntax on           " syntax highlighting, please

filetype on           " Enable filetype detection
filetype indent on    " Enable filetype-specific indenting
filetype plugin on    " Enable filetype-specific plugins

compiler ruby         " Enable compiler support for ruby


" recognize more kinds of ruby files
autocmd BufRead *.rake set filetype=ruby
autocmd BufRead Rakefile set filetype=ruby
autocmd BufRead *.erb set filetype=eruby
autocmd BufRead Capfile set filetype=ruby
autocmd BufRead config.ru set filetype=ruby
autocmd BufRead Gemfile set filetype=ruby
autocmd Filetype * set formatoptions-=o noeol

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
map <leader>d :NERDTreeToggle<CR>
map <leader>n :NERDTreeToggle<CR>
map <leader>a :Ack 
