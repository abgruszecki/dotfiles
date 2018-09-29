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
set smartindent
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
filetype plugin indent on
" }}}

" Pathogen {{{
runtime bundle/vim-pathogen/autoload/pathogen.vim
let g:pathogen_disabled=[ 'vim-pandoc', 'vim-pandoc-syntax' ]
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

" Old BIT syntax {{{
function Setbitxmlopts()
	set filetype=xml
	set expandtab
	set shiftwidth=8
	set tabstop=4
	set softtabstop=4
endfunction
augroup bitOpts
	au!
	au BufNewFile,BufRead datacapture.cfg call Setbitxmlopts()
	au BufNewFile,BufRead *.page call Setbitxmlopts()
augroup END
augroup counterPandoc
	au! BufNewFile,BufRead *.md setfiletype markdown
augroup END

augroup bitOpts
	au!
	au BufNewFile,BufRead *.sc set ft=scala
augroup END

function Setbitymlopts()
	set filetype=yaml
	set expandtab
	set shiftwidth=8
	set tabstop=2
	set softtabstop=2
endfunction

function SetTabStop(len)
	set expandtab
	echom a:len
	let &shiftwidth = a:len
	let &softtabstop = a:len
endfunction
" }}}

nnoremap <Leader>2 :call SetTabStop(2)<CR>
nnoremap <Leader>4 :call SetTabStop(4)<CR>
nnoremap <Leader>8 :call SetTabStop(8)<CR>

nnoremap <Leader><F2> :set ft=ansible<CR>

" It's syntastic!
map <F4> :SyntasticCheck<CR>

vnoremap il 0$

" Add around-all motion
onoremap aa :<C-U>normal! ggVG<CR>

noremap <Leader>y "+y
noremap <Leader>p "+p

set backupdir=~/.cache/vim/backup/
set directory=~/.cache/vim/swap/
set undodir=~/.cache/vim/undo/

set tags=.tags,./.tags;,tags,./tags;

" Better? windows
nnoremap <silent> <Up> :wincmd k<CR>
nnoremap <silent> <Down> :wincmd j<CR>
nnoremap <silent> <Left> :wincmd h<CR>
nnoremap <silent> <Right> :wincmd l<CR>

nnoremap <C-e> <C-u>
nnoremap <C-u> 5<C-e>
nnoremap <C-y> 5<C-y>

map \ :NERDTreeToggle
nnoremap <Leader>s :w<CR>
map <silent> <Leader>z :NERDTreeToggle<CR>

nnoremap [g :set nohls<CR>/<<<<<<<<CR>:set hls<CR>
nnoremap ]g :set nohls<CR>?<<<<<<<<CR>:set hls<CR>
nnoremap [= :set nohls<CR>/=======<CR>:set hls<CR>
nnoremap ]= :set nohls<CR>/=======<CR>:set hls<CR>
nnoremap [G :set nohls<CR>?>>>>>>><CR>:set hls<CR>
nnoremap ]G :set nohls<CR>?>>>>>>><CR>:set hls<CR>

" Swap mark jumping
nnoremap ' `
nnoremap ` '

" Commands without shift
" nnoremap ; :
" nnoremap : ;

" Use <C-L> to clear the highlighting of :set hlsearch.
if maparg('<C-L>', 'n') ==# ''
nnoremap <silent> <C-L> :nohlsearch<CR><C-L>
endif

let g:syntastic_scala_checkers = [ 'scalac' ]
let g:syntastic_javascript_checkers = [ 'eslint' ]
let g:syntastic_mode_map = { 'mode': 'active',
                           \ 'active_filetypes': [],
                           \ 'passive_filetypes': [ 'scala', 'java' ] }
let g:syntastic_cpp_compiler_options = "-std=c++11 -Wall -Wextra -Wpedantic"

" solarized scheme
set background=dark
colorscheme solarized

" airline
let g:airline_theme_patch_func = 'AirlineThemePatch'
function! AirlineThemePatch(palette)
  if g:airline_theme == 'solarized'

    let g:airline#themes#solarized#palette.normal.airline_error =
          \ g:airline#themes#solarized#palette.normal.airline_warning

    let g:airline#themes#solarized#palette.normal_modified.airline_error =
          \ g:airline#themes#solarized#palette.normal.airline_warning

    let g:airline#themes#solarized#palette.insert.airline_error =
          \ g:airline#themes#solarized#palette.normal.airline_warning

    let g:airline#themes#solarized#palette.insert_modified.airline_error =
          \ g:airline#themes#solarized#palette.normal.airline_warning

    let g:airline#themes#solarized#palette.visual.airline_error =
          \ g:airline#themes#solarized#palette.normal.airline_warning

    let g:airline#themes#solarized#palette.visual_modified.airline_error =
          \ g:airline#themes#solarized#palette.normal.airline_warning

    let g:airline#themes#solarized#palette.replace.airline_error =
          \ g:airline#themes#solarized#palette.normal.airline_warning

    let g:airline#themes#solarized#palette.replace_modified.airline_error =
          \ g:airline#themes#solarized#palette.normal.airline_warning

  endif
endfunction
set ttimeoutlen=10
set timeoutlen=1000
set noshowmode
let g:airline_powerline_fonts=1
let g:airline_theme='solarized'

let g:vimwiki_list = [{'path': '~/vimwiki'}, {'path': '~/epflwiki'}]
