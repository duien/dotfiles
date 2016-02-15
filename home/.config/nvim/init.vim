" Vimplug
call plug#begin()
Plug 'tpope/vim-sensible'
Plug 'morhetz/gruvbox'
  let g:gruvbox_contrast_light = 'hard'
  let g:gruvbox_italic = 1
  let g:gruvbox_italicize_comments = 1
  let g:gruvbox_undercurl = 0
  let g:gruvbox_italicize_strings = 1
Plug 'bling/vim-airline'
  let g:airline_powerline_fonts = 1
  let g:airline#extensions#tabline#left_sep = ' '
  let g:airline#extensions#tabline#left_alt_sep = '|'
  let g:airline#extensions#tabline#tab_nr_type = 1
  let g:airline#extensions#tabline#enabled = 1
  let g:airline#extensions#branch#format = 1
  let g:airline#extensions#whitespace#enabled = 0
  " maybe?
  let g:airline#extensions#hunks#enabled = 1
Plug 'vim-ruby/vim-ruby'
" Plug 'bling/vim-bufferline'
Plug 'fugitive.vim'
Plug 'tpope/vim-commentary'
Plug 'ctrlpvim/ctrlp.vim'
  let g:ctrlp_custom_ignore = '\v[\/]\.(git|hg|svn)$|node_modules'
  let g:ctrlp_working_path_mode = 'ra'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-repeat'
Plug 'airblade/vim-gitgutter'
  let g:gitgutter_sign_column_always = 1
  let g:gitgutter_override_sign_column_highlight = 1
  " Customize the symbols used
  let g:gitgutter_sign_added = '+'
  let g:gitgutter_sign_modified = '~'
  let g:gitgutter_sign_removed = '-'
  let g:gitgutter_sign_removed_first_line = '^'
  let g:gitgutter_sign_modified_removed = '≋'
Plug 'ntpeters/vim-better-whitespace'
Plug 'bruno-/vim-ruby-fold'
Plug 'benekastah/neomake'
  autocmd! BufWritePost,BufEnter * Neomake
  let g:neomake_javascript_enabled_makers = ['eslint']
  let g:neomake_open_list = 2
  let g:neomake_ruby_enabled_makers = []
Plug 'scrooloose/nerdtree'
Plug 'Xuyuanp/nerdtree-git-plugin'
Plug 'vim-scripts/TaskList.vim'
Plug 'elixir-lang/vim-elixir'
Plug 'rust-lang/rust.vim'
 " Some since-abandoned alternative markdown thinger -- the
" vim-flavored-markdown plugin makes underscores work like GitHub, but breaks
" the ability to highlight code blocks
"
" Plug 'tpope/vim-markdown'
" Plug 'jtratner/vim-flavored-markdown'

" Plug 'scrooloose/syntastic'
"   let g:syntastic_javascript_checkers = ["eslint"]

call plug#end()

" Experimental -- Use truecolor in iTerm2
" let $NVIM_TUI_ENABLE_TRUE_COLOR=1

" set backspace=2         " backspace in insert mode works like normal editor
" syntax on               " syntax highlighting
" filetype indent on      " activates indenting for files
" set autoindent          " auto indenting
set number                " line numbers
" colorscheme delek         " colorscheme desert
set noshowmode
" set nobackup            " get rid of anoying ~file
set colorcolumn=80,120
set updatetime=500
set shell=/bin/sh
set nofoldenable          " don't automatically collapse folds when loading file
if executable('ag')
  " Use ag in CtrlP for listing files. Lightning fast and respects .gitignore
  let g:ctrlp_user_command = 'ag %s -l --nocolor -m 100 -g ""'
  " ag is fast enough that CtrlP doesn't need to cache
  let g:ctrlp_use_caching = 0
endif

if has('gui_running')
  set background=light
else
  set background=dark
endif
colorscheme gruvbox

" Customize highlighting for git gutter
" (removed) foreground settings for gutter if using solid bg: guifg=#f9f5d7 ctermfg=230
highlight GitGutterAdd          guifg=#b8bb26 ctermfg=142
highlight GitGutterChange       guifg=#83a598 ctermfg=109
highlight GitGutterDelete       guifg=#fb4934 ctermfg=167
highlight GitGutterChangeDelete guifg=#d3869b ctermfg=175
highlight link ExtraWhitespace GitGutterDelete


" Dealing with tabstops
set tabstop=2           " width of a hard tabstop
set softtabstop=2       " keep this the same as tabstop
set shiftwidth=2        " size of an 'indent' with >> or <<
set expandtab

" Line wrapping
set linebreak
noremap <silent> <Up>   gk
noremap <silent> <Down> gj
" inoremap kj <esc>   " this ended up being a bad idea




let mapleader="\<space>"

" Easy dotfile editing
nmap <leader>ve :tabedit $MYVIMRC<CR>
nmap <leader>vv :source $MYVIMRC<CR>
nmap <leader>vp :PlugInstall<CR>
nmap <leader><Tab> gt
nmap <leader>w :w<CR>
nmap <leader>ff :CtrlP<CR>
nmap <leader>fs :w<CR>
nmap <leader>fr :CtrlPMRU<CR>
nmap <leader>pt :NERDTreeToggle<CR>
nmap <leader>qq :quitall<CR>
nmap <leader>bb :CtrlPBuffer<CR>
nmap <leader>bd :bdelete<CR>

" Make a nice little cheatsheet
" | Leader | Default | Command
" +--------+---------+---------
" | ve     |         | edit ~/.vimrc in tab
" | vv     |         | source ~/.vimrc
" | vp     |         | install plugins
" | tab    | gt      | next tab
" | w      |         | write file
" | ff     |         | file : search
" | fs     |         | file : save
" | fr     |         | file : recent
" | pt     |         | project : toggle tree
" | qq     |         | quit all
" | bb     |         | search open buffers
" | bd     |         | buffer : delete


" Better markdown support
let g:markdown_fenced_languages = ['javascript', 'js=javascript', 'ruby', 'sh', 'bash=sh']
augroup markdown
  au!
  au BufNewFile,BufRead *.md,*.markdown setlocal filetype=markdown
augroup END

" Source the vimrc file after saving it
" augroup reload_vimrc " {
"   autocmd!
"   autocmd BufWritePost .vimrc source $MYVIMRC
" augroup END " }
