" Vundle plugins {{{
set nocompatible
filetype off
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'VundleVim/Vundle.vim'

" -- Small plugins (total load .09s)
Plugin 'alfredodeza/khuno.vim'
" Plugin 'altercation/vim-colors-solarized'
Plugin 'AndrewRadev/splitjoin.vim'
Plugin 'chase/vim-ansible-yaml'
Plugin 'chriskempson/base16-vim'
Plugin 'christoomey/vim-tmux-navigator'
Plugin 'fisadev/vim-isort'
Plugin 'honza/vim-snippets'
Plugin 'jvirtanen/vim-octave'
Plugin 'Glench/Vim-Jinja2-Syntax'
Plugin 'kien/ctrlp.vim'
" Plugin 'klen/python-mode'
Plugin 'LaTeX-Box-Team/LaTeX-Box'
Plugin 'Lokaltog/vim-easymotion'
Plugin 'maksimr/vim-jsbeautify'
Plugin 'mattn/gist-vim'
Plugin 'mattn/webapi-vim'
Plugin 'nelstrom/vim-markdown-folding'
Plugin 'Raimondi/delimitMate'
Plugin 'rust-lang/rust.vim'
Plugin 'scrooloose/nerdcommenter'
Plugin 'tpope/vim-endwise'
Plugin 'tpope/vim-fugitive'
Plugin 'tpope/vim-markdown'
Plugin 'tpope/vim-unimpaired'
Plugin 'Valloric/YouCompleteMe'

" -- Slightly bigger plugins
" Load: .012s
Plugin 'bling/vim-airline'
" Load: .09s
Plugin 'scrooloose/nerdtree'
" Load: .15s
" Plugin 'tpope/vim-rails'
" Load: .11s
Plugin 'SirVer/ultisnips'

" -- Massive load time (something is seriously wrong here, it might be because
"                       of pylint?)
" Load: 4.81s
"Plugin 'scrooloose/syntastic'

call vundle#end()
filetype plugin indent on

" }}}
" Colorscheme settings {{{
syntax on
set t_Co=16
set background=dark

" colorscheme solarized
" let g:solarized_visibility = "high"

let base16colorspace=256  " Access colors present in 256 colorspace
colorscheme base16-default

" }}}
" General settings {{{
let mapleader=','
let maplocalleader=','
" Automatic reloading on external file changes
set autoread
" Automatic writing when using certain commands, e.g. :n, :N
set autowrite
" Hides buffers instead of closing
set hidden
" No swap files
set noswapfile
" Line numbering
set number
" Show vertical column at 79 (maximum line length for Python)
set colorcolumn=79
" Wrap lines automatically
set textwidth=79
" Ignore case when searching except if search has uppercase letters
set smartcase
" Find as you type
set incsearch
" Autocompletion for command-line
set wildmenu
" Always use a menu for autocompletion, not a preview pane
set completeopt=menu
" Ignore in autocompletion (also ignores in CtrlP/Command-T/Unite.vim)
set wildignore=*.o,*.obj,*.pyc,*.class,*.git,*.orig
" Maximum height of the autocompletion popup menu (pum)
set pumheight=8
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
" Always show statusline/powerline/airline
set laststatus=2
" Disable showing the current mode because powerline/airline already shows it
set noshowmode
" Help for Matlab/Octave (with shortcut K)
autocmd FileType matlab,octave setlocal keywordprg=info\ octave\ --vi-keys\ --index-search

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
set foldlevelstart=0
set foldnestmax=1
function! MyFoldText() " {{{
    let line = getline(v:foldstart)
    let nucolwidth = &fdc + &number * &numberwidth
    let windowwidth = winwidth(0) - nucolwidth - 3
    let foldedlinecount = v:foldend - v:foldstart
    " expand tabs into spaces
    let onetab = strpart(' ', 0, &tabstop)
    let line = substitute(line, '\t', onetab, 'g')
    let line = strpart(line, 0, windowwidth - 2 -len(foldedlinecount))
    let fillcharcount = windowwidth - len(line) - len(foldedlinecount)
    return line . ' ' . repeat(" ",fillcharcount) . ' ' . foldedlinecount . ' '
endfunction " }}}
set foldtext=MyFoldText()
autocmd FileType html,xhtml,htmldjango,eruby setlocal foldnestmax=20

" }}}
" Indentation {{{
" Number of spaces that a Tab respresents
set tabstop=8
" Number of spaces for an (auto)indent
set shiftwidth=4
" Number of spaces that a Tab feels like while editing
set softtabstop=4
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
au FileType html,xhtml,htmldjango,eruby setlocal tabstop=2 shiftwidth=2 softtabstop=2
au FileType python setlocal nocindent shiftwidth=4 softtabstop=4 tw=79
au FileType haskell setlocal shiftwidth=2 softtabstop=2
au FileType prolog setlocal shiftwidth=4 softtabstop=4
au FileType ruby setlocal shiftwidth=2 softtabstop=2
au FileType tex setlocal shiftwidth=2 softtabstop=2

" }}}
" Plugin settings {{{
" bling/vim-airline {{{
let g:airline_powerline_fonts = 1
let g:airline_exclude_preview = 1
" Optimization: don't search for all possible extensions
let g:airline_extensions = [
            \'branch',
            \'ctrlp',
            \'netrw',
            \'quickfix',
            \'syntastic',
            \'tabline',
            \'virtualenv']
let g:airline#extensions#tabline#buffer_min_count = 2
" }}}
" klen/python-mode {{{
let g:pymode_trim_whitespaces = 0  " This already happens anyway
let g:pymode_doc = 0  " Doesn't work when I need it
let g:pymode_virtualenv = 0  " Don't need it
let g:pymode_run = 0  " Don't use it (because it's not interactive)
let g:pymode_breakpoint_cmd = 'import pudb; pudb.set_trace()  # XXX breapoint'
let g:pymode_lint = 0  " Use flake8 for syntax checking
" let g:pymode_lint_checker = 'pyflakes'
" let g:pymode_lint_ignore = 'C0103,C0111,E114,E116,E265,E702'
let g:pymode_rope = 0  " Rope just sucks
let g:pymode_syntax = 0  " Already works out-of-the-box
" }}}
" LaTeX-Box-Team/LaTeX-Box {{{
let g:LatexBox_Folding = 1
" }}}
" Lokaltog/vim-easymotion {{{
let g:EasyMotion_smartcase = 1  " Turn on case sensitive feature
let g:EasyMotion_startofline = 0  " Keep cursor column during JK motion
" }}}
" mattn/gist-vim {{{
let g:gist_detect_filetype = 1
let g:gist_open_browser_after_post = 1
" }}}
" Raimondi/delimitMate {{{
let delimitMate_expand_cr = 1
let delimitMate_expand_space = 1
" }}}
" scrooloose/nerdcommenter {{{
let NERDCommentWholeLinesInVMode = 1
let NERDSpaceDelims = 1
" }}}
" scrooloose/nerdtree {{{
let NERDTreeIgnore = ['\~$', '\.pyc$', '\.class$', '\.pid$', '\.o$', '\.pdf$']
let NERDTreeMinimalUI = 1
let NERDTreeChDirMode = 2
" }}}
" scrooloose/syntastic {{{
let g:syntastic_python_pylint_args = "-d C0103,C0111"
let g:syntastic_check_on_open = 1
let g:syntastic_aggregate_errors = 1
let g:syntastic_auto_jump = 2
" }}}
" SirVer/ultisnips {{{
let g:UltiSnipsEditSplit='vertical'  " Let the UltiSnipsEdit split
" }}}
" Valloric/YouCompleteMe {{{
let g:ycm_global_ycm_extra_conf = '~/.vim/ycm_extra_conf.py'
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
" Ex-ex
nnoremap Q <nop>
" ,= to autoformat
nnoremap <leader>= mzggVG='z
" Sort lines
vnoremap <leader>s :!sort<CR>
" Sudo write
cnoremap w!! w !sudo tee % >/dev/null
" Toggle (in)visible characters
nnoremap <leader>i :set list!<CR>
" ,w to write
nnoremap <leader>w :w<CR>
" Use C-O and C-P to shift between edited parts
nnoremap <C-P> <C-I>
" Use (very) magic regexes
nnoremap / /\v
vnoremap / /\v
cnoremap s/ s/\v
nnoremap :g/ :g/\v
nnoremap :g// :g//
" Markdown mappings
au FileType markdown call MardownMappings()
function! MardownMappings()
    nnoremap <buffer> <leader>1 yypVr=:redraw<CR>
    nnoremap <buffer> <leader>2 yypVr-:redraw<CR>
    nnoremap <buffer> <leader>3 mzI###<Space><Esc>`zllll<CR>
endfunction
" Save by Ctrl-S (has to be allowed by terminal (stty -ixon))
nnoremap <C-S> :w<CR>
" Windows resizing
noremap + <C-W>+
noremap - <C-W>-
" Tab management
nnoremap <Tab> :bnext<CR>
nnoremap <S-Tab> :bprev<CR>
nnoremap <leader>e :enew<cr>
nnoremap <leader>d :bd<cr>
nnoremap <leader>D :bd!<cr>

" }}}
" Plugin mappings {{{
" alfredodeza/khuno.vim {{{
"   <leader>x   Show errors in a quickfix-like pane
nnoremap <silent><leader>x <Esc>:Khuno show<CR>
" }}}
" AndrewRadev/splitjoin.vim {{{
"   gS  Split a one-liner into multiple lines
"   gJ  (with the cursor on the first line of a block) to join a block into a
"       single-line statement.
" }}}
" fisadev/vim-isort {{{
let g:vim_isort_map = '<C-i>'
" }}}
" kien/ctrlp.vim {{{
let g:ctrlp_map = '<leader>t'  " Behave like command-t
let g:ctrlp_cmd = 'CtrlPMixed'
" }}}
" klen/python-mode {{{
"   The following evironments are defined:
"     C   Class
"     M   Method or function
"   which makes the following commands possible:
"     [[  Jump to previous class or function (normal, visual, operator modes)
"     ]]  Jump to next class or function  (normal, visual, operator modes)
"     [M  Jump to previous class or method (normal, visual, operator modes)
"     ]M  Jump to next class or method (normal, visual, operator modes)
"     aC  Select a class. Ex: vaC, daC, yaC, caC (normal, operator modes)
"     iC  Select inner class. Ex: viC, diC, yiC, ciC (normal, operator modes)
"     aM  Select a function or method. Ex: vaM, daM, yaM, caM (normal, operator modes)
"     iM  Select inner function or method. Ex: viM, diM, yiM, ciM (normal, operator modes)
"
"   All the following is currently not enabled:
"
"   K          Show pydoc
"   <leader>g  Runs the python code
" let g:pymode_run_bind = '<leader>g'
"   <leader>b  Sets a breakpoint
"
"   Rope:
" let g:pymode_rope_global_prefix = "<leader>r"
"     <C-Space>  Autocompletion
"     <C-c>d     Show internal doc
"     <C-c>g     Go to definition
"     <C-c>rr    Rename object onder cursor
"     <C-c>r1r   Rename current module
"     <C-c>ro    Organize imports (sort + remove unused)
"     <C-c>ra    Auto import object onder cursor
"     <C-c>r1p   Convert module to package
"     <C-c>rm    Extract method
"     <C-c>rl    Extract variable
"     <C-c>ru    Automagically finds places where a function can be used
"     <C-c>rv    Move method/class
"     <C-c>rs    Change method/function signature
" }}}
" LaTeX-Box-Team/LaTeX-Box {{{
"   <leader>ll  Compile with latexmk.
"   <leader>lL  Force compilation with latexmk.
"   <leader>lc  Clean temporary output from LaTeX.
"   <leader>lC  Clean all output from LaTeX.
"   <leader>lk  Stop latexmk if it is running.
"   <leader>lg  Show the running status of latexmk for the current buffer.
"   <leader>lG  Show the running status of latexmk for all buffers with process
"               group ID's.
"   <leader>le  Load the log file for the current document and jump to the
"               first error.
"   <leader>lv  View output file.
"   <leader>lf  Recalculate the folds.
"   <leader>lt  Open a table of contents.
au FileType tex call LaTeXMappings()
function! LaTeXMappings()
"   [[          Start an environment
    imap <buffer> [[ \begin{
"   ]]          Close the environment
    imap <buffer> ]] <Plug>LatexCloseCurEnv
endfunction
" }}}
" Lokaltog/vim-easymotion {{{
"   <leader><leader>  Default prefix
"   s                 Bi-directional find. Jump to anywhere with s{char}{label}
nmap s <Plug>(easymotion-s)
"   <leader>j         Easymotion up. Jump up with <leader>j{label}
map <leader>j <Plug>(easymotion-j)
"   <leader>k         Easymotion down. Jump down with <leader>k{label}
map <leader>k <Plug>(easymotion-k)
"   <leader>l         Easymotion forward. Jump forward with <leader>l{label}
map <leader>l <Plug>(easymotion-lineforward)
"   <leader>h         Easymotion backward. Jump backward with <leader>h{label}
map <leader>h <Plug>(easymotion-linebackward)
" }}}
" Raimondi/delimitMate {{{
"   <BS>     Also removes closing paren/quote/bracket
"   <S-BS>   Only removes closing paren/quote/bracket
"   <S-Tab>  Skips over one the closing parens/quotes/brackets
"   <C-G>g   Skips over all closing parens/quotes/brackets
" }}}
" scrooloose/nerdcommenter {{{
"   <leader>cc  Comment out the current line or text selected in visual mode.
"   <leader>cn  Same as <leader>cc but forces nesting.
"   <leader>c<Space>
"               Toggles the comment state of the selected line(s). If the
"               topmost selected line is commented, all selected lines are
"               uncommented and vice versa.
"   <leader>cm  Comments the given lines using only one set of multipart
"               delimiters.
"   <leader>ci  Toggles the comment state of the selected line(s) individually.
"   <leader>cs  Comments out the selected lines ``sexily''
"   <leader>cy  Same as <leader>cc except that the commented line(s) are yanked
"               first.
"   <leader>c$  Comments the current line from the cursor to the end of line.
"   <leader>cA  Adds comment delimiters to the end of line and goes into insert
"               mode between them.
"   <leader>ca  Switches to the alternative set of delimiters.
"   <leader>cl and <leader>cb
"               Same as |NERDComComment| except that the delimiters are aligned
"               down the left side (<leader>cl) or both sides (<leader>cb).
"   <leader>cu  Uncomments the selected line(s).
" }}}
" scrooloose/nerdtree {{{
"   <leader>n  Open NERD Tree
nnoremap <leader>n :NERDTreeToggle<CR>
" }}}
" SirVer/ultisnips {{{
"   ç        Expand ultisnips
let g:UltiSnipsExpandTrigger='ç'
"   <Tab>    Move to next editable part in the snippet
let g:UltiSnipsJumpForwardTrigger='<Tab>'
"   <S-Tab>  Move the previous editable part in the snippet
let g:UltiSnipsJumpBackwardTrigger='<S-Tab>'
"   <leader>u  Show available snippets
let g:UltiSnipsListSnippets='<leader>u'
" }}}
" tpope/vim-unimpaired {{{
"   A lot of mapping starting with [ and ]. A full list can be found here:
"   https://github.com/tpope/vim-unimpaired/blob/master/doc/unimpaired.txt
" }}}
" Valloric/YouCompleteMe {{{
"   Tab and S-Tab  Scroll between autocomplete options
"   C-Space        Force autocompletion without prefix given
" }}}
"" Java/Eclim mappings {{{
"au FileType java call EclimBindings()
"function! EclimBindings()
"   nnoremap <silent> <buffer> <leader>i :JavaImport<CR>
"   nnoremap <silent> <buffer> <leader>o :JavaImportOrganize<CR>
"   nnoremap <silent> <buffer> <leader>d :JavaDocSearch -x declarations<cr>
"   nnoremap <silent> <buffer> <leader><space> :JavaCorrect<cr>
"   nnoremap <silent> <buffer> <cr> :JavaSearchContext<cr>
"endfunction
"" }}}
" }}}

" vim:foldmethod=marker:foldlevel=0
