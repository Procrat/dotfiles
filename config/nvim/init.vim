" Plugins {{{
call plug#begin()

" -- Small plugins (less than .01s)
Plug 'alfredodeza/khuno.vim'
" Plug 'altercation/vim-colors-solarized'
Plug 'chase/vim-ansible-yaml'
Plug 'chriskempson/base16-vim'
Plug 'christoomey/vim-tmux-navigator'
Plug 'easymotion/vim-easymotion'
Plug 'fisadev/vim-isort'
Plug 'Glench/Vim-Jinja2-Syntax'
Plug 'godlygeek/tabular'
Plug 'honza/vim-snippets'
Plug 'junegunn/vim-journal'
Plug 'jvirtanen/vim-octave'
" Plug 'klen/python-mode'
Plug 'LaTeX-Box-Team/LaTeX-Box'
Plug 'maksimr/vim-jsbeautify'
Plug 'mattn/gist-vim', { 'on': 'Gist' }
Plug 'mattn/webapi-vim', { 'on': 'Gist' }  " Dependency for gist-vim
Plug 'Raimondi/delimitMate'
Plug 'rhysd/committia.vim'
Plug 'rust-lang/rust.vim'
Plug 'scrooloose/nerdcommenter'
Plug 'SirVer/ultisnips', { 'on': [] }  " Defer to insert mode
Plug 'tpope/vim-endwise'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-markdown'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'
Plug 'vim-scripts/JavaDecompiler.vim'

" -- Slightly bigger plugins
" Load: .02s
Plug 'ctrlpvim/ctrlp.vim'
" Load: .10s
Plug 'scrooloose/nerdtree', { 'on': 'NERDTreeToggle' }
" Load: .14s
Plug 'tpope/vim-rails'
" Load: .25s (for Rails)
Plug 'AndrewRadev/splitjoin.vim'
" Load: .30s
Plug 'bling/vim-airline'
" Load: .93s (for Rails)
" Defer to insert mode
Plug 'Valloric/YouCompleteMe', { 'do': 'python2 install.py', 'on': [] }

" -- Massive load time
" Load: .04s when not used; more than 5s for e.g. Python files
" If `let g:syntastic_enable_signs=0` is used, it only takes .07s.
" Plug 'scrooloose/syntastic'

call plug#end()

" Defer loading of ultisnips and YouCompleteMe to insert mode
augroup load_us_ycm
  autocmd!
  autocmd InsertEnter * call plug#load('ultisnips', 'YouCompleteMe')
                     \| call youcompleteme#Enable()
                     \| autocmd! load_us_ycm
augroup END

filetype plugin indent on

" }}}
" Colorscheme settings {{{
syntax on
set background=dark

let base16colorspace=256  " Access colors present in 256 colorspace
colorscheme base16-default

" }}}
" General settings {{{
let mapleader=','
let maplocalleader=','
" Automatic reloading on external file changes (default in NeoVim)
"set autoread
" Automatic writing when using certain commands, e.g. :n, :N
set autowrite
" Hides buffers instead of closing
set hidden
" No swap files
set noswapfile
" Line numbering
set number
" Show vertical column at 79 (maximum line length for Python)
set colorcolumn=80
" Wrap lines automatically
set textwidth=80
" Ignore case when searching except if search has uppercase letters
set ignorecase smartcase
" Find as you type (default in NeoVim)
"set incsearch
" Autocompletion for command-line (default in NeoVim)
"set wildmenu
" Always use a menu for autocompletion, not a preview pane
set completeopt=menu
" Ignore in autocompletion (also ignores in CtrlP/Command-T/Unite.vim)
set wildignore=*.o,*.obj,*.pyc,*.class,*.orig,*/.git/*
" Maximum height of the autocompletion popup menu (pum)
set pumheight=8
" Mouse interactivity (default in NeoVim)
"set mouse=a
" Copy/paste with X11 CLIPBOARD
set clipboard=unnamedplus
" Paste mode shortcut
set pastetoggle=<leader>p
" Start scrolling earlier
set scrolloff=6
set sidescroll=2
set sidescrolloff=2
" Make the new window below or right of the current one
set splitbelow
set splitright
" Check modelines (like the one at the bottom of this file)
set modelines=1
" Use the silver searcher to grep and always print file name in Quickfix list
set grepprg=ag\ --vimgrep\ -w\ -Q\ \"$*\"
set grepformat=%f:%l:%c:%m
" Always show statusline/powerline/airline (default in NeoVim)
"set laststatus=2
" Disable showing the current mode because powerline/airline already shows it
set noshowmode
" Wait less than a second for mapped seauence to complete
set timeoutlen=400
" Keep the cursor on the same column
set nostartofline
" Skip intro
set shortmess+=I
" Allow non-existing blocks in Visual block mode
set virtualedit=block
" French spacing all the way
set nojoinspaces
" Ignore whitespace in diff mode
set diffopt+=iwhite

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
augroup folding
    autocmd!
    au FileType xml,html,xhtml,htmldjango,eruby,xslt setlocal foldnestmax=20
augroup END

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
" Round off indentation to multiple of shiftwidth
set shiftround
" When smarttab is on, a <Tab> in front of a line inserts blanks according to
" 'shiftwidth'.  'tabstop' is used in other places. A <BS> will delete a
" 'shiftwidth' worth of space at the start of the line.
" (default in NeoVim)
"set smarttab
" Copy indent from current line when starting a new line (default in NeoVim)
"set autoindent
" Indent after '{' and after keywords (if, for, else, while, do, switch)
set smartindent


let s:maxoff = 50  " Maximum number of lines to look backwards.
function! GetGooglePythonIndent(lnum)
  " Indent inside parens.
  " Align with the open paren unless it is at the end of the line.
  " E.g.
  "   open_paren_not_at_EOL(100,
  "                         (200,
  "                          300),
  "                         400)
  "   open_paren_at_EOL(
  "       100, 200, 300, 400)
  call cursor(a:lnum, 1)
  let [par_line, par_col] = searchpairpos('(\|{\|\[', '', ')\|}\|\]', 'bW',
        \ "line('.') < " . (a:lnum - s:maxoff) . " ? dummy :"
        \ . " synIDattr(synID(line('.'), col('.'), 1), 'name')"
        \ . " =~ '\\(Comment\\|String\\)$'")
  if par_line > 0
    call cursor(par_line, 1)
    if par_col != col("$") - 1
      return par_col
    endif
  endif

  " Delegate the rest to the original function.
  return GetPythonIndent(a:lnum)
endfunction

let pyindent_nested_paren="&sw*2"
let pyindent_open_paren="&sw*2"

" Indentation per filetype
augroup indentation
    autocmd!

    au FileType xml,html,xhtml,htmldjango,eruby,xslt setlocal tabstop=2 shiftwidth=2 softtabstop=2 textwidth=120 colorcolumn=120
    au FileType js setlocal shiftwidth=2 softtabstop=2 textwidth=120 colorcolumn=120
    au FileType python setlocal nocindent shiftwidth=4 softtabstop=4 textwidth=79 colorcolumn=79 indentexpr=GetGooglePythonIndent(v:lnum)
    au FileType haskell setlocal shiftwidth=2 softtabstop=2
    au FileType prolog setlocal shiftwidth=4 softtabstop=4
    au FileType ruby setlocal shiftwidth=2 softtabstop=2
    au FileType tex setlocal shiftwidth=2 softtabstop=2
augroup END

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
    \'tabline']
    " \'virtualenv']
let g:airline#extensions#tabline#buffer_min_count = 2
" }}}
" easymotion/vim-easymotion {{{
let g:EasyMotion_smartcase = 1  " Turn on case sensitive feature
let g:EasyMotion_startofline = 0  " Keep cursor column during JK motion
" }}}
" kien/ctrlp.vim {{{
" Use the silver searcher in CtrlP for listing files
let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'
" The silver searcher is fast enough so that CtrlP doesn't need to cache
let g:ctrlp_use_caching = 0
" }}}
" klen/python-mode {{{
" let g:pymode_trim_whitespaces = 0  " This already happens anyway
" let g:pymode_doc = 0  " Doesn't work when I need it
" let g:pymode_virtualenv = 0  " Don't need it
" let g:pymode_run = 0  " Don't use it (because it's not interactive)
" let g:pymode_breakpoint_cmd = 'import pudb; pudb.set_trace()  # TODO breakpoint'
" let g:pymode_lint = 0  " Use flake8 for syntax checking
" let g:pymode_lint_checker = 'pyflakes'
" let g:pymode_lint_ignore = 'C0103,C0111,E114,E116,E265,E702'
" let g:pymode_rope = 0  " Rope just sucks
" let g:pymode_syntax = 0  " Already works out-of-the-box
" }}}
" LaTeX-Box-Team/LaTeX-Box {{{
let g:LatexBox_Folding = 1
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
" let g:syntastic_python_pylint_args = "-d C0103,C0111"
" let g:syntastic_check_on_open = 1
" let g:syntastic_aggregate_errors = 1
" let g:syntastic_auto_jump = 2
" }}}
" SirVer/ultisnips {{{
let g:UltiSnipsEditSplit='vertical'  " Let the UltiSnipsEdit split
" }}}
" tpope/vim-markdown {{{
let g:markdown_folding = 1
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
" Make Y behave like other capitals
nnoremap Y y$
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
" Make < and > behave like they should
vnoremap < <gv
vnoremap > >gv
" Markdown mappings
au FileType markdown call MardownMappings()
function! MardownMappings()
    nnoremap <buffer> <leader>1 yypVr=:redraw<CR>
    nnoremap <buffer> <leader>2 yypVr-:redraw<CR>
    nnoremap <buffer> <leader>3 mzI###<Space><Esc>`zllll<CR>
endfunction
" Windows resizing
noremap + <C-W>+
noremap - <C-W>-
" Tab management
nnoremap <Tab> :bnext<CR>
nnoremap <S-Tab> :bprev<CR>
nnoremap <leader>e :enew<cr>
nnoremap <leader>d :bd<cr>
nnoremap <leader>D :bd!<cr>
" Use S to grep (dependent on format of grepprg)
nnoremap S :grep! <C-R><C-W><CR>:cw<CR>
vnoremap S "hy:grep! <C-R>h<CR>:cw<CR>
" Send a blame mail from a git repo
function! Blame()
    execute "!send_blame_mail " . expand('%:p') . " " . line('.')
endfunction
noremap <leader>B :call Blame()<CR>

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
" christoomey/vim-tmux-navigator {{{
"   M-[hjkl]  Move around between vim and tmux panes
"             (This requires additional settings in .tmux.conf)
let g:tmux_navigator_no_mappings = 1
nnoremap <silent> <M-h> :TmuxNavigateLeft<CR>
nnoremap <silent> <M-j> :TmuxNavigateDown<CR>
nnoremap <silent> <M-k> :TmuxNavigateUp<CR>
nnoremap <silent> <M-l> :TmuxNavigateRight<CR>
" }}}
" easy-motion/vim-easymotion {{{
"                     Disable default mappings
let g:EasyMotion_do_mapping = 0
"   s                 Bi-directional find. Jump to anywhere with s{char}{label}
"noremap s <Plug>(easymotion-s)
"   s                 Bi-directional find. Jump to anywhere with s{char}{char}{label}
map s <Plug>(easymotion-s2)
"   <leader>j         Easymotion up. Jump up with <leader>j{label}
map <leader>j <Plug>(easymotion-j)
"   <leader>k         Easymotion down. Jump down with <leader>k{label}
map <leader>k <Plug>(easymotion-k)
"   <leader>l         Easymotion forward. Jump forward with <leader>l{label}
map <leader>l <Plug>(easymotion-lineforward)
"   <leader>h         Easymotion backward. Jump backward with <leader>h{label}
map <leader>h <Plug>(easymotion-linebackward)
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
let g:UltiSnipsExpandTrigger='»'
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
"   gd             Go to definition (or declaration)
"                  (YCM does a better job at this than Vim.)
nnoremap gd :YcmCompleter GoTo<CR>
"   K              Go to documentation
nnoremap K :YcmCompleter GetDoc<CR>
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
" Misc {{{
augroup vimrc_misc
  autocmd!

  " Help for Matlab/Octave (with shortcut K)
  au FileType matlab,octave setlocal keywordprg=info\ octave\ --vi-keys\ --index-search

  " Recognize some file extensions
  "   Filename ending in .pl is a Prolog file and not a Perl one
  au BufNewFile,BufRead *.pl setlocal filetype=prolog
  "   Filename ending in .py3 is a Python3 file
  au BufNewFile,BufRead *.py3 setfiletype python
  "   Filename ending in rc is probably a config file if nothing else
  au BufNewFile,BufRead *rc setfiletype dosini
  "   Highlight Markdown files and todo files with vim-journal
  au BufNewFile,BufRead *.md,*todo*,*TODO* setfiletype journal

  " Turn on spelling for some filetypes
  au FileType tex,mail setlocal spell

  " Reload .vimrc on save
  au BufWritePost .vimrc source %

  " Automatic renaming of tmux window
  if exists('$TMUX')
      au BufEnter * if empty(&buftype) | call system('tmux rename-window '.expand('%:t:S')) | endif
      au VimLeave * call system('tmux set-window automatic-rename on')
  endif

  " Remove trailing whitespace automatically on write when desired (non-binary)
  au BufWritePre * call StripTrailingWhitespace()
  function! StripTrailingWhitespace()
      if &l:fileencoding ==? "utf-8"
          let l:winview = winsaveview()
          silent! %s/\s\+$//
          call winrestview(l:winview)
      endif
  endfunction

augroup END

" }}}

" vim:foldmethod=marker:foldlevel=0