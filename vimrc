" My vim config
" stuff taken from https://github.com/derekwyatt/vim-config
" and many many others
filetype off 
" call pathogen#infect()
" call pathogen#helptags()

set nocompatible

set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

Bundle 'gmarik/vundle'

Bundle 'plasticboy/vim-markdown'
Bundle 'Shougo/neocomplcache'
Bundle 'SirVer/ultisnips'
Bundle 'sjl/gundo.vim'
Bundle 'tpope/vim-commentary'
Bundle 'tpope/vim-fugitive'
Bundle 'tpope/vim-repeat'
Bundle 'tpope/vim-surround'
Bundle 'tpope/vim-unimpaired'
Bundle 'wincent/Command-T'

Bundle 'matchit.zip'

" Language / Filetype Support
Bundle 'pangloss/vim-javascript'
Bundle 'lukerandall/haskellmode-vim'
Bundle 'digitaltoad/vim-jade'
Bundle 'wavded/vim-stylus'
Bundle 'tpope/vim-rails'
Bundle 'jnwhiteh/vim-golang'

Bundle 'VimClojure'

filetype on
" Make backspace behave in a sane manner.
set backspace=indent,eol,start

" Enable file type detection and do language-dependent indenting.
filetype plugin indent on
syntax on

set ch=2
set vb
set backspace=2
set hidden
set cpoptions=B$

set number
set laststatus=2
set lazyredraw
set showmode
set showcmd
set showmatch
set mousehide
set mouse=a
set timeoutlen=500
set history=500
set scrolloff=5
set virtualedit=onemore
set nrformats=

set statusline=
set statusline+=%f\                         " file name
set statusline+=%y\                         " file type
set statusline+=%h%m%r%w                    " flags
set statusline+=%{fugitive#statusline()}    " git status
set statusline+=%=                          " right align
set statusline+=Line:%l/%L[%p%%]\ Col:%v\   " cursor location
set statusline+=Buf:#%n\                    " file name

set hlsearch
set incsearch
set ignorecase
set smartcase

set wildmenu
set wildignorecase
set showfulltag

set diffopt+=iwhite
set clipboard+=unnamed
set grepprg=grep\ -nH\ $*
set formatprg=fmt
set spelllang=en_us
set listchars=tab:→\ ,eol:↓,trail:⊥


autocmd FileType * set ai ts=4 sw=4 sts=4 sta et "autoindent tabstop shiftwidth softtabstop smarttab expandtab
autocmd FileType javascript setlocal ai ts=2 sw=2 sts=2 sta et
autocmd FileType jade setlocal ai ts=2 sw=2 sts=2 sta et
autocmd FileType make setlocal noexpandtab
autocmd FileType mail set spell tw=78 formatprg="fmt -w 78"

" Folding
set foldignore= " don't ignore anything when folding
set foldlevelstart=99 " no folds closed on open
set foldmethod=marker " collapse code using markers

"-----------------------------------------------------------------------------
" Mappings
"-----------------------------------------------------------------------------

let mapleader = ","

" Switch colon and semi-colon
nnoremap ; :
nnoremap : ;
vnoremap ; :
vnoremap : ;

" Exit insert mode
inoremap jj <esc>

" make Y like C, D
nnoremap Y y$

" auto magic mode
nnoremap / /\v
vnoremap / /\v

" Search for trailing whitespace
nnoremap <leader>w /\s\+$<CR>

" Toggle last active buffer
nnoremap <leader><Tab> :b#<CR>
nnoremap <leader>b :ls<CR>:buffer<space>

" Toggling settings
noremap <silent> <leader>sp :set spell!<CR>
noremap <silent> <leader>l :set list!<CR>
nnoremap <silent> <leader>n :nohlsearch<CR>
nnoremap <leader>fm :call ToggleFoldMethod()<CR>
nnoremap <leader>nm :call ToggleNumberMethod()<CR>

" Open file in the same directory as current file without changing CWD
noremap <leader>e :e <C-R>=expand("%:p:h") . "/" <CR>

" Cd to the directory containing the file in the buffer
noremap <leader>cd :lcd %:h<CR>
noremap <leader>md :!mkdir -p %:p:h<CR>
" autocmd BufEnter * silent! lcd %:p:h:gs/ /\\ /

" Edit the vimrc file
noremap <silent> <leader>ev :e $MYVIMRC<CR>
noremap <silent> <leader>sv :so $MYVIMRC<CR>

" Enable sudo saving
ca w!! w !sudo tee "%"
" cmap w!! %!sudo tee "%"

" Insert current timestamp
nnoremap <leader>tl a<C-R>=strftime("%b %d %Y %I:%M%p %Z")<CR><Esc>
nnoremap <leader>ts a<C-R>=strftime("%I:%M")<CR><Esc>

" emacs keymap in the commandline {{{
cnoremap <c-a> <home>
cnoremap <c-e> <end>
cnoremap <c-b> <left>
cnoremap <c-d> <del>
cnoremap <c-f> <right>
cnoremap <c-n> <down>
cnoremap <c-p> <up>
cnoremap <M-b> <S-Left>
cnoremap <M-f> <S-Right>

set shellslash
if has("unix")
    set shell=zsh
else
    set shell=ksh.exe
endif

"-----------------------------------------------------------------------------
" Plugin Settings
"-----------------------------------------------------------------------------
nnoremap <leader>u :GundoToggle<CR>

let g:haddock_browser = "firefox"

"-----------------------------------------------------------------------------
" Functions
"-----------------------------------------------------------------------------
function! ToggleFoldMethod()
    if &foldmethod == 'indent'
        set foldmethod=marker
        echo "foldmethod=marker"
    else
        set foldmethod=indent
        echo "foldmethod=indent"
    endif
endfunction

function! ToggleNumberMethod()
    if &relativenumber
        set number
    else
        set relativenumber
    endif
endfunction

"-----------------------------------------------------------------------------
" Set up the window colors and size
"-----------------------------------------------------------------------------

set background=dark
"colorscheme mango

if has("gui_running")
    let g:main_font = "Consolas\\ 11"
    let g:small_font = "Consolas\\ 4"

    exe "set guifont=" . g:main_font
    set background=dark
    if !exists("g:vimrcloaded")
        winpos 0 0
        if !&diff
            winsize 130 120
        else
            winsize 227 120
        endif
        let g:vimrcloaded = 1
    endif
endif
