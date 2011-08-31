source ~/.vim/bundle/vim-pathogen/autoload/pathogen.vim
call pathogen#infect()

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
set cursorline      " highlight the line with the cursor
set incsearch       " show search as you type

" Search highlighting and and clearning
" Diabled until I find a non-awful way of clearing
" set hlsearch        " highlight all search matches
" nnoremap <esc> :noh<return><esc>
" nnoremap <CR> :noh<return><CR>

let ruby_operators=1 " highlight operators in ruby

" colorscheme railscasts
if has('gui_running')
  set background=light
else
  set background=dark
endif
colorscheme solarized
syntax on           " syntax highlighting, please

filetype on           " Enable filetype detection
filetype indent on    " Enable filetype-specific indenting
filetype plugin on    " Enable filetype-specific plugins


" recognize more kinds of ruby files
autocmd BufRead *.rake set filetype=ruby
autocmd BufRead Rakefile set filetype=ruby
autocmd BufRead *.erb set filetype=eruby
autocmd BufRead Capfile set filetype=ruby
autocmd BufRead config.ru set filetype=ruby
autocmd BufRead Gemfile set filetype=ruby
autocmd BufRead *.zsh-theme set filetype=zsh
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

call togglebg#map("<F5>")

" Show syntax highlighting groups for word under cursor
nmap <C-S-P> :call <SID>SynStack()<CR>
function! <SID>SynStack()
  if !exists("*synstack")
    return
  endif
  echo map(synstack(line('.'), col('.')), 'synIDattr(v:val, "name")')
endfunc

" Source the vimrc file after saving it
if has("autocmd")
  autocmd bufwritepost .vimrc source $MYVIMRC
endif
