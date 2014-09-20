" Standards {{{
augroup vimrc
	au BufReadPre vimrc,.vimrc setlocal foldmethod=marker
augroup END

set nocompatible
set ruler
set laststatus=2
set showcmd
set showmode
set number
set relativenumber

set incsearch
set ignorecase
set smartcase
set hlsearch

set autoindent
set smarttab
set tabstop=8
set shiftwidth=8
set softtabstop=8

set wildmenu
set wildmode=list:longest

set backspace=indent,eol,start

set hidden

set history=1000

set mouse=a

set colorcolumn=81
highlight ColorColumn ctermbg=lightgrey

syntax on
filetype plugin on
filetype plugin indent on
" }}}

" Pathogen {{{
runtime bundle/vim-pathogen/autoload/pathogen.vim
let g:pathogen_disabled=[ '' ]
execute pathogen#infect()
" }}}

" Space as leader {{{
map <Space> <Nop>
let mapleader = "\<Space>"
" }}}

" Pretty linebreak
let &showbreak = '↪ '

nnoremap <F9> :set relativenumber!<CR>

" Copypasting {{{
" vnoremap <silent> <F1> "zy:new \| pu z \| exe '2,.!pbcopy' \| bw!<CR>
" nnoremap <silent> <F1> :.w !pbcopy<CR>
" inoremap <silent> <F2> <C-O>:set paste \| let @z=system('pbpaste')<CR><C-R>z<C-O>:set nopaste<CR>
" nnoremap <Leader>v :let @z=system('pbpaste')<CR>
nnoremap <F1> "+y
nnoremap <F2> "+p
set pastetoggle=<F5>
" }}}

" It's syntastic!
map <F4> :SyntasticCheck<CR>

vnoremap il 0$

set tags=.tags,./.tags;,tags,./tags;

" Better? windows
nmap <silent> <Up> :wincmd k<CR>
nmap <silent> <Down> :wincmd j<CR>
nmap <silent> <Left> :wincmd h<CR>
nmap <silent> <Right> :wincmd l<CR>

nnoremap <C-e> <C-u>
nnoremap <C-u> 5<C-e>
nnoremap <C-y> 5<C-y>

map \ :NERDTreeToggle
map <Leader>s :w<CR>
map <silent> <Leader>z :NERDTreeToggle<CR>

" Swap mark jumping
nnoremap ' `
nnoremap ` '

" Use <C-L> to clear the highlighting of :set hlsearch.
if maparg('<C-L>', 'n') ==# ''
nnoremap <silent> <C-L> :nohlsearch<CR><C-L>
endif

let g:syntastic_scala_checkers = [ 'scalac' ]
let g:syntastic_mode_map = { 'mode': 'active',
                           \ 'active_filetypes': [],
                           \ 'passive_filetypes': [ 'scala' ] }

" solarized scheme
set background=dark
colorscheme solarized

" airline
set ttimeoutlen=10
set timeoutlen=1000
set noshowmode
let g:airline_powerline_fonts=1
let g:airline_theme='solarized'

" ctrlp
let g:ctrlp_extensions = ['tag', 'bookmarkdir']
