set nocompatible

" Install plugins {{{
call pathogen#infect()

" }}}
" Solarized colorscheme settings {{{
syntax on
set t_Co=16
"colorscheme Tomorrow-Night
set background=dark
let g:solarized_visibility = "high"
colorscheme solarized

" }}}
" General settings {{{
let mapleader=","
" Automatic reloading on external file changes
set autoread
" Automatic writing when using certain commands, e.g. :n, :N
set autowrite
set noswapfile

" Line numbering
set number
set colorcolumn=79
" Wrap lines automatically
set textwidth=79
" Ignore case when searching except if search has uppercase letters
set smartcase
" Find as you type
set incsearch
" Autocompletion
set wildmenu
" Ignore in autocompletion (also ignores in Command-T)
set wildignore=*.o,*.obj,*.pyc,*.class,*.git
" Mouse interactivity
set mouse=a
" Copy/paste with shared clipboard
if has ('unnamedplus')
    set clipboard=unnamedplus
else
    set clipboard=unnamed
endif
" Paste mode shortcut
set pastetoggle=<leader>p
" Start scrolling earlier
set scrolloff=6
set sidescroll=2
set sidescrolloff=2
" Make the new window below the current one
set splitbelow
" Check modelines (like the one at the bottom of this file)
set modelines=1
" Use ack! to grep and always print file name in Quickfix list
set grepprg=ack\ -H\ --nocolor\ --nogroup

" }}}
" Recognize some file extensions {{{
" Filename ending in .pl is a Prolog file and not a Perl one
autocmd BufNewFile,BufRead *.pl set filetype=prolog
" Filename ending in .py3 is a Python3 file
autocmd BufNewFile,BufRead *.py3 set filetype=python

" }}}
" Remove trailing whitespace {{{
" Remove automatically on write when desired (non-binary)
autocmd BufWritePre * call StripTrailingWhitespace()
function! StripTrailingWhitespace()
    if &l:fileencoding ==? "utf-8"
        let l:winview = winsaveview()
        silent! %s/\s\+$//
        call winrestview(l:winview)
    endif
endfunction

" }}}
" Folding {{{
set foldmethod=indent
set foldlevelstart=1
set foldnestmax=2
autocmd FileType html,xhtml,htmldjango setlocal foldnestmax=30
autocmd FileType python setlocal foldlevel=90
set fillchars=fold:.

" }}}
" Indentation {{{

" Number of spaces that a Tab respresents
set tabstop=4
" Number of spaces for an (auto)indent
set shiftwidth=4
" Turn tabs into spaces
set expandtab
" When smarttab is on, a <Tab> in front of a line inserts blanks according to
" 'shiftwidth'.  'tabstop' is used in other places. A <BS> will delete a
" 'shiftwidth' worth of space at the start of the line.
set smarttab
" Copy indent from current line when starting a new line
set autoindent
" Indent after '{' and after keywords (if, for, else, while, do, switch)
set smartindent

" Indentation per filetype
filetype plugin indent on
au FileType html,xhtml,htmldjango setlocal tabstop=2 shiftwidth=2 softtabstop=2
au FileType python setlocal nocindent shiftwidth=4 softtabstop=4 tw=78
au FileType haskell setlocal shiftwidth=2 softtabstop=2
au FileType prolog setlocal shiftwidth=4 softtabstop=4
au FileType ruby setlocal shiftwidth=2 softtabstop=2
au FileType tex setlocal shiftwidth=2 softtabstop=2

" }}}
" Plugin settings {{{

" SuperTab settings {{{
set completeopt=menuone,longest,preview
let g:SuperTabDefaultCompletionType = "context"
let g:SuperTabLongestHighlight = 1
" No tabcompletion for Prolog after certain symbols
au FileType prolog let g:SuperTabNoCompleteAfter = ['^', '\s', ';', '->', '(']
" }}}
" Ropevim settings {{{
au FileType python call RopeSettings()
function! RopeSettings()
    setlocal omnifunc=RopeCompleteFunc
    let ropevim_vim_completion = 1
    let ropevim_extended_complete = 1
    "let g:ropevim_enable_autoimport = 1
    let g:ropevim_guess_project = "1"
    let g:ropevim_autoimport_modules = ["sys","os.*","traceback","django.*","lxml.etree","lxml.*"]
    "inoremap <leader><Space> <C-R>=RopeCodeAssistInsertMode()<CR>
    "let g:ropevim_autoimport_modules = ["base64", "datetime", "gtk", "hashlib", "heapq", "itertools", "locale", "logging", "math", "os", "os.*", "pdb", "pexpect", "pygtk", "random", "re", "sys", "timeit"]
endfunction
" }}}
" Powerline settings {{{
"let g:Powerline_symbols = "fancy"
"set laststatus=2
" }}}
" Minibufexplorer settings {{{
"" Switch buffers with Ctrl-Tab en Ctrl-Shift-Tab
"let g:miniBufExplMapCTabSwitchWindows = 1
"" Switch windows with Ctrl-arrows
"let g:miniBufExplMapWindowNavArrows = 1
"" Single click (instead of double click) to switch buffers
"let g:miniBufExplUseSingleClick = 1
" }}}
" DelimitMate settings {{{
" Sets cursor and closing paren/bracket to the right place on <CR>
let delimitMate_expand_cr = 1
" }}}
" NERDTree settings {{{
let NERDTreeIgnore = ['\.pyc$', '\.class$']
" }}}
" Command-T settings {{{
let g:CommandTMatchWindowReverse = 1
" }}}
" LaTeX Suite settings {{{
au FileType tex call TexBindings()
function! TexBindings()
    let g:tex_flavor = "latex"
    let g:Tex_DefaultTargetFormat = "pdf"
    let g:Tex_MultipleCompileFormats = "pdf,dvi"
    let g:Imap_UsePlaceHolders = 0
    TCTarget pdf
endfunction
" }}}
" Eclim settings {{{
" Don't show todo markers in margin
let g:EclimSignLevel = 2
" }}}
" Syntastic settings {{{
let g:syntastic_python_pylint_args = "-d C0103,C0111"
" }}}

" }}}
" General mappings {{{
" jj in insert mode to go to normal mode
inoremap jj <Esc>
" Use backspace to delete character
noremap <BS> X
" Space to (un)fold
nnoremap <silent> <Space> za
vnoremap <silent> <Space> za
" ,= to autoformat
nnoremap <leader>= mzggVG='z
" Use s to change current line
nnoremap s ddko

" }}}
" Plugin mappings {{{
" F7 to open NERDTree {{{
noremap <F7> :NERDTreeToggle<CR>
" }}}
" Python (PEP8, Rope) bindings {{{
au FileType python call PythonBindings()
function! PythonBindings()
    " F8 to open PEP8 quickfix window
    let g:pep8_map='<F8>'
    " Rope shortcuts
    nnoremap <leader>d :RopeGotoDefinition<CR>
    nnoremap <leader>r :RopeRename<CR>
    nnoremap <leader>i :RopeAutoImport<CR>
    nnoremap <leader>o :RopeOrganizeImports<CR>0<CR><CR>
endfunction
" }}}
" Java (Eclim) bindings {{{
au FileType java call EclimBindings()
function! EclimBindings()
    nnoremap <silent> <buffer> <leader>i :JavaImport<CR>
    nnoremap <silent> <buffer> <leader>o :JavaImportOrganize<CR>
    nnoremap <silent> <buffer> <leader>d :JavaDocSearch -x declarations<cr>
    nnoremap <silent> <buffer> <leader><space> :JavaCorrect<cr>
    nnoremap <silent> <buffer> <cr> :JavaSearchContext<cr>
endfunction
" }}}

" }}}
" Window & tab bindings {{{

" Un-vimmy binding D:
" Save by Ctrl-S (has to be allowed by terminal (stty -ixon))
nnoremap <C-S> :w<CR>

" Window movement! Whee!
noremap <leader>h :wincmd h<CR>
noremap <leader>j :wincmd j<CR>
noremap <leader>k :wincmd k<CR>
noremap <leader>l :wincmd l<CR>
" Windows resizing
noremap + <C-W>+
noremap - <C-W>-

" Tab switching
nnoremap <S-Tab> :tabp<CR>
nnoremap <Tab> :tabn<CR>

" }}}

" vim:foldmethod=marker:foldlevel=0
