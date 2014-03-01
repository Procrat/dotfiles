set nocompatible

" Install plugins {{{
call pathogen#infect()

" }}}
" Solarized colorscheme settings {{{
syntax on
set background=dark
set t_Co=16
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
" Mouse interactivity
set mouse=a
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

" }}}
" Recognize some file extensions {{{
" Filename ending in .pl is a Prolog file and not a Perl one
autocmd BufNewFile,BufRead *.pl set filetype=prolog
" Filename ending in .py3 is a Python3 file
autocmd BufNewFile,BufRead *.py3 set filetype=python

" }}}
" Remove trailing whitespace {{{
" Remove trailing whitespace automatically on write when desired (non-binary)
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

set tabstop=4		" Number of spaces that a Tab respresents
set shiftwidth=4	" Number of spaces for an (auto)indent
set expandtab		" Turn tabs into spaces
set smarttab		" When on, a <Tab> in front of a line inserts blanks
                    " according to 'shiftwidth'. 'tabstop' is used in other
					" places. A <BS> will delete a 'shiftwidth' worth of space
					" at the start of the line.
set autoindent smartindent

" Indentation shizzle per filetype
filetype plugin indent on
au FileType html,xhtml,htmldjango setlocal tabstop=2 shiftwidth=2 softtabstop=2
au FileType python setlocal nocindent smartindent shiftwidth=4 softtabstop=4 tw=78
au FileType haskell setlocal shiftwidth=2 softtabstop=2
au FileType prolog setlocal shiftwidth=4 softtabstop=4
au FileType ruby setlocal shiftwidth=2 softtabstop=2

" }}}
" Plugin settings {{{

" SuperTab settings {{{
let g:SuperTabDefaultCompletionType = "context"
" No tabcompletion for Prolog after certain symbols
au FileType prolog let g:SuperTabNoCompleteAfter = ['^', '\s', ';', '->', '(']
set completeopt=menuone,longest,preview
" }}}
" Rope settings {{{
let ropevim_vim_completion = 1
let ropevim_extended_complete = 1
let g:ropevim_enable_autoimport = 1
let g:ropevim_autoimport_modules = ["sys","os.*","traceback","django.*","lxml.etree","lxml.*"]
imap <C-Space> <C-R>=RopeCodeAssistInsertMode()<CR>
"" Set autoguessing rope project
"let g:ropevim_guess_project = "1"
"let g:ropevim_autoimport_modules = ["base64", "datetime", "gtk", "hashlib", "heapq", "itertools", "locale", "logging", "math", "os", "os.*", "pdb", "pexpect", "pygtk", "random", "re", "sys", "timeit"]
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
" LaTeX Suite settings {{{
set grepprg=grep\ -nH\ $*
let g:tex_flavor = "latex"
let g:Tex_DefaultTargetFormat = "pdf"
" }}}
" TODO: For Play framework {{{
let g:syntastic_java_checkers=[]
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
" F7 to open NERDTree
noremap <F7> :NERDTreeToggle<CR>
" F8 to open PEP8 quickfix window
let g:pep8_map='<F8>'
" F12 to to go the definition of a function/class/...
noremap <F12> :RopeGotoDefinition<CR>
" ,r to rename a variable across a file
noremap <leader>r :RopeRename<CR>
" Auto-import for Python
noremap é :RopeAutoImport<CR>
" Rope AutoImport and OrganizeImport
nnoremap <leader>o :RopeOrganizeImports<CR>0<CR><CR>
nnoremap <leader>i :RopeAutoImport<CR>
" Eclim bindings {{{
au FileType java call EclimBindings()
function! EclimBindings()
    nnoremap <silent> <buffer> <leader>i :JavaImport<CR>
    nnoremap <silent> <buffer> <leader>d :JavaDocSearch -x declarations<cr>
    nnoremap <silent> <buffer> <cr> :JavaSearchContext<cr>
endfunction
" }}}
" Java auto-import {{{
noremap <F5> :call JavaInsertImport()<CR>
function! JavaInsertImport()
  exe "normal mz"
  let cur_class = expand("<cword>")
  try
    if search('^\s*import\s.*\.' . cur_class . '\s*;') > 0
      throw getline('.') . ": import already exist!"
    endif
    wincmd }
    wincmd P
    1
    if search('^\s*public.*\s\%(class\|interface\)\s\+' . cur_class) > 0
      1
      if search('^\s*package\s') > 0
        yank y
      else
        throw "Package definition not found!"
      endif
    else
      throw cur_class . ": class not found!"
    endif
    wincmd p
    normal! G
    " insert after last import or in first line
    if search('^\s*import\s', 'b') > 0
      put y
    else
      1
      put! y
    endif
    substitute/^\s*package/import/g
    substitute/\s\+/ /g
    exe "normal! 2ER." . cur_class . ";\<Esc>lD"
  catch /.*/
    echoerr v:exception
  finally
    " wipe preview window (from buffer list)
    silent! wincmd P
    if &previewwindow
      bwipeout
    endif
    exe "normal! `z"
  endtry
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
