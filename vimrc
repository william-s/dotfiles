filetype off 
call pathogen#runtime_append_all_bundles()
call pathogen#helptags()

set nocompatible

filetype on
" Make backspace behave in a sane manner.
set backspace=indent,eol,start

" Enable file type detection and do language-dependent indenting.
filetype plugin indent on
syntax on

set shellslash
if has("unix")
    set shell=zsh
else
    set shell=ksh.exe
endif

set ch=2
set vb
set backspace=2
set hidden
set cpoptions=B$

" Set the status line the way i like it
" adapted from https://github.com/derekwyatt/vim-config
set stl=%f\ %m\ %r%{fugitive#statusline()}\ Line:%l/%L[%p%%]\ Col:%v\ Buf:#%n\ [%b][0x%B]
set laststatus=2
set lazyredraw
set showmode
set mousehide
set guicursor=n-v-c:block-Cursor-blinkon0,ve:ver35-Cursor,o:hor50-Cursor,i-ci:ver25-Cursor,r-cr:hor20-Cursor,sm:block-Cursor-blinkwait175-blinkoff150-blinkon175
set timeoutlen=500
set history=500
set scrolloff=5
set virtualedit=onemore

set ignorecase
set smartcase

set wildmenu
set wildignorecase
set showfulltag

set diffopt+=iwhite
set hlsearch
set incsearch
set clipboard+=unnamed
set autoread
set grepprg=grep\ -nH\ $*
set formatprg=fmt
set spelllang=en_us

set relativenumber
let mapleader = ","

let g:haddock_browser = "firefox"

" enable sudo saving
ca w!! w !sudo tee "%"
" cmap w!! %!sudo tee "%"

autocmd FileType * set ai ts=4 sw=4 sts=4 sta et "autoindent tabstop shiftwidth softtabstop smarttab expandtab
autocmd FileType javascript set ai ts=2 sw=2 sts=4 sta et
au FileType mail set spell tw=78 formatprg="fmt -w 78"

nmap <silent> <leader>sp :set spell!<CR>

"autocmd BufEnter * silent! lcd %:p:h:gs/ /\\ /

"open file in the same directory as current file without changing CWD
map <leader>e :e <C-R>=expand("%:p:h") . "/" <CR>

" cd to the directory containing the file in the buffer
nmap <silent> ,cd :lcd %:h<CR>
nmap <silent> ,md :!mkdir -p %:p:h<CR>

" Edit the vimrc file
nmap <silent> ,ev :e $MYVIMRC<CR>
nmap <silent> ,sv :so $MYVIMRC<CR>

" Turn off that stupid highlight search
nmap <silent> ,n :nohls<CR>

" The following beast is something i didn't write... it will return the 
" syntax highlighting group that the current "thing" under the cursor
" belongs to -- very useful for figuring out what to change as far as 
" syntax highlighting goes.
nmap <silent> ,qq :echo "hi<" . synIDattr(synID(line("."),col("."),1),"name") . '> trans<' . synIDattr(synID(line("."),col("."),0),"name") . "> lo<" . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">"<CR>


" Digraphs
" Alpha
imap <c-l><c-a> <c-k>a*
" Beta
imap <c-l><c-b> <c-k>b*
" Gamma
imap <c-l><c-g> <c-k>g*
" Delta
imap <c-l><c-d> <c-k>d*
" Epslion
imap <c-l><c-e> <c-k>e*
" Lambda
imap <c-l><c-l> <c-k>l*
" Eta
imap <c-l><c-y> <c-k>y*
" Theta
imap <c-l><c-h> <c-k>h*
" Mu
imap <c-l><c-m> <c-k>m*
" Rho
imap <c-l><c-r> <c-k>r*
" Pi
imap <c-l><c-p> <c-k>p*
" Phi
imap <c-l><c-f> <c-k>f*

let g:main_font = "Consolas\\ 11"
let g:small_font = "Consolas\\ 4"

"-----------------------------------------------------------------------------
" NERD Tree Plugin Settings
"-----------------------------------------------------------------------------
" Toggle the NERD Tree on an off with F7
nmap <F7> :NERDTreeToggle<CR>

" Close the NERD Tree with Shift-F7
nmap <S-F7> :NERDTreeClose<CR>

" Show the bookmarks table on startup
let NERDTreeShowBookmarks=1

" Don't display these kinds of files
let NERDTreeIgnore=[]

"-----------------------------------------------------------------------------
" Set up the window colors and size
"-----------------------------------------------------------------------------

set background=dark
"colorscheme mango

if has("gui_running")
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
