set nocompatible
set ruler
set laststatus=2
set showcmd
set showmode
set number

set incsearch
set ignorecase
set smartcase
set hlsearch

nmap <F3> :w<CR>
imap <F3> <Esc>:w<CR>a
map <F4> :SyntasticCheck<CR>

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

let &showbreak = '↪ '

set tags=.tags,./.tags;,tags,./tags;

nmap <silent> <Up> :wincmd k<CR>
nmap <silent> <Down> :wincmd j<CR>
nmap <silent> <Left> :wincmd h<CR>
nmap <silent> <Right> :wincmd l<CR>

nnoremap <C-e> <C-u>
nnoremap <C-u> 5<C-e>
nnoremap <C-y> 5<C-y>

" map <Space> <leader>
map <Space> <Nop>
map \ :NERDTreeToggle
let mapleader = "\<Space>"
map <Leader>s :w<CR>
map <silent> <Leader>z :NERDTreeToggle<CR>

nnoremap ' `
nnoremap ` '

" Use <C-L> to clear the highlighting of :set hlsearch.
if maparg('<C-L>', 'n') ==# ''
nnoremap <silent> <C-L> :nohlsearch<CR><C-L>
endif

" set textwidth=80
set colorcolumn=81
highlight ColorColumn ctermbg=lightgrey

" prettify
syntax on
filetype plugin on
filetype plugin indent on

" pathogen
runtime bundle/vim-pathogen/autoload/pathogen.vim
let g:pathogen_disabled=[ '' ]
execute pathogen#infect()

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
