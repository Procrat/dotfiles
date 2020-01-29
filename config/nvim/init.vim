scriptencoding 'utf-8'

" Plugins {{{
call plug#begin()

" -- Fast plugins (< 5ms on boot)
Plug 'AndrewRadev/splitjoin.vim'
Plug 'AndrewRadev/switch.vim'
Plug 'christoomey/vim-tmux-navigator'
Plug 'cohama/lexima.vim'
Plug 'Glench/Vim-Jinja2-Syntax'
Plug 'godlygeek/tabular'
Plug 'honza/vim-snippets'
Plug 'jreybert/vimagit'
Plug '/usr/share/vim/vimfiles/plugin/fzf.vim'
Plug 'junegunn/fzf.vim'
Plug 'junegunn/limelight.vim'
Plug 'junegunn/vim-peekaboo'
Plug 'jvirtanen/vim-octave'
Plug 'Konfekt/FastFold'
Plug 'LaTeX-Box-Team/LaTeX-Box'
Plug 'leafgarland/typescript-vim'
Plug 'ludovicchabant/vim-gutentags'
Plug 'majutsushi/tagbar'
Plug 'maksimr/vim-jsbeautify', { 'for': [
            \ 'javascript', 'javascript.jsx', 'json', 'html', 'css'] }
Plug 'mattn/emmet-vim'
Plug 'mattn/gist-vim', { 'on': 'Gist' }
Plug 'mattn/webapi-vim', { 'on': 'Gist' }  " Dependency for gist-vim
Plug 'neomake/neomake'
Plug 'rhysd/clever-f.vim'
Plug 'rhysd/committia.vim'
" Plug 'a-watson/vim-gdscript'
Plug 'rust-lang/rust.vim'
Plug 'scrooloose/nerdcommenter'
Plug 'scrooloose/nerdtree', { 'on': 'NERDTreeToggle' }
Plug 'shime/vim-livedown'
Plug 'Shougo/echodoc.vim'
Plug 'terryma/vim-multiple-cursors'
Plug 'terryma/vim-smooth-scroll'
Plug 'tmux-plugins/vim-tmux'
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-obsession'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-sleuth'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'
Plug 'vim-pandoc/vim-pandoc'
Plug 'vim-pandoc/vim-pandoc-syntax'
Plug 'vim-scripts/JavaDecompiler.vim'

" -- Slightly slower plugins (5ms -- 50ms)
Plug 'chriskempson/base16-vim'
Plug 'easymotion/vim-easymotion'
Plug 'junegunn/vim-journal'
Plug 'sheerun/vim-polyglot'
Plug 'tpope/vim-rails'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

" -- Slow plugins (> 50ms)
" -- All of these are fixed by loading them on demand
Plug 'SirVer/ultisnips', { 'on': [] }  " Defer to insert mode
Plug 'bitc/vim-hdevtools', { 'on': [
            \ 'HdevtoolsType', 'HdevtoolsClear', 'HdevtoolsInfo'] }
Plug 'Procrat/jedi-vim'

" -- Completion plugins
Plug 'ervandew/supertab'
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'racer-rust/vim-racer'
Plug 'Shougo/neco-vim'
Plug 'zchee/deoplete-clang'
Plug 'zchee/deoplete-jedi'
Plug 'zchee/deoplete-zsh'

" Unmanaged plugins
" Plug '~/.config/nvim/unplugged/eclim', { 'for': 'java', 'frozen': 1 }

call plug#end()

" Defer loading of some plugins to insert mode
augroup load_insert_plugins
    autocmd!
    autocmd InsertEnter * call plug#load('ultisnips', 'jedi-vim')
augroup END

" filetype plugin indent on  " (default in NeoVim)

" }}}
" Colorscheme settings {{{
" syntax on  " (default in NeoVim)

let g:base16colorspace = 256  " Access colors present in 256 colorspace
colorscheme base16-mocha
let g:airline_theme = 'base16'

" }}}
" General settings {{{
let g:mapleader = "\<Space>"
let g:maplocalleader = "\<Space>"
" Automatic reloading on external file changes (default in NeoVim)
"set autoread
" Automatic writing when using certain commands, e.g. :n, :N
set autowrite
" Hides buffers instead of closing
set hidden
" Persistent undo
set undofile
" Line numbering
set number
" Show vertical column at 79 (maximum line length for Python)
set colorcolumn=80
" Highlight current line
set cursorline
" Ignore case when searching except if search has uppercase letters
set ignorecase smartcase
" Find as you type (default in NeoVim)
"set incsearch
" Autocompletion for command-line (default in NeoVim)
"set wildmenu
" Always use a menu for autocompletion, insert longest match, no preview pane
set completeopt=menuone,longest
" Ignore in autocompletion (also ignores in CtrlP/Command-T/Unite.vim)
set wildignore=*.o,*.obj,*.pyc,*.class,*.orig,*/.git/*
" Maximum height of the autocompletion popup menu (pum)
set pumheight=8
" Mouse interactivity
set mouse=a
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
set modelines=2
" Use ripgrep to grep and always print file name in Quickfix list
set grepprg=rg\ --vimgrep\ --word-regexp\ --fixed-strings\ \"$*\"
set grepformat=%f:%l:%c:%m
" Always show statusline/powerline/airline (default in NeoVim)
"set laststatus=2
" Disable showing the current mode because powerline/airline already shows it
set noshowmode
" Wait less than a second for mapped seauence to complete
set timeoutlen=500
" Keep the cursor on the same column
set nostartofline
" Skip intro, skip autocompletion output and use more abbreviations
set shortmess+=Ica
" Allow non-existing blocks in Visual block mode
set virtualedit=block
" French spacing all the way
set nojoinspaces
" Show normally invisible characters (trailing whitespace, tabs, nbsp)
set list
" Ignore whitespace in diff mode
set diffopt+=iwhite
" Set window title
set title
" Pipe cursor in insert & command mode, underline in replace mode
set guicursor=n-v-c:block-Cursor/lCursor-blinkon0,i-ci:ver25-Cursor/lCursor,r-cr:hor20-Cursor/lCursor

" Nvim settings
if has('nvim')
    " Show substitution feedback <3
    set inccommand=split
endif

" }}}
" Folding {{{
set foldmethod=indent
set foldlevelstart=0
set foldnestmax=1
function! MyFoldText() " {{{
    let l:line = getline(v:foldstart)
    let l:nucolwidth = &foldcolumn + &number * &numberwidth
    let l:windowwidth = winwidth(0) - l:nucolwidth - 3
    let l:foldedlinecount = v:foldend - v:foldstart
    " expand tabs into spaces
    let l:onetab = strpart(' ', 0, &tabstop)
    let l:line = substitute(l:line, '\t', l:onetab, 'g')
    let l:line = strpart(l:line, 0, l:windowwidth - 2 -len(l:foldedlinecount))
    let l:fillcharcount = l:windowwidth - len(l:line) - len(l:foldedlinecount)
    return l:line . ' ' . repeat(' ', l:fillcharcount) . ' ' . l:foldedlinecount . ' '
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


" Indentation per filetype
augroup indentation
    autocmd!

    au FileType xml,html,xhtml,htmldjango,eruby,xslt setlocal tabstop=2 shiftwidth=2 softtabstop=2 textwidth=120 colorcolumn=120
    au FileType js setlocal shiftwidth=2 softtabstop=2 textwidth=120 colorcolumn=120
    au FileType css,scss setlocal shiftwidth=2 softtabstop=2
    au FileType python setlocal nocindent shiftwidth=4 softtabstop=4 textwidth=79 colorcolumn=79
    au FileType haskell setlocal shiftwidth=4 softtabstop=4
    au FileType rust setlocal shiftwidth=4 softtabstop=4 textwidth=99 colorcolumn=99
    au FileType prolog setlocal shiftwidth=4 softtabstop=4
    au FileType ruby setlocal shiftwidth=2 softtabstop=2
    au FileType tex setlocal shiftwidth=2 softtabstop=2
augroup END

" }}}
" Plugin settings {{{
" AndrewRadev/splitjoin.vim {{{
" Put closing angle bracket in HTML on a new line
let g:splitjoin_html_attributes_bracket_on_new_line = 1
" }}}
" bling/vim-airline {{{
let g:airline_powerline_fonts = 1
let g:airline_exclude_preview = 1
" Optimization: don't search for all possible extensions
let g:airline_extensions = [
    \'branch',
    \'netrw',
    \'neomake',
    \'quickfix',
    \'tabline',
    \'tagbar',
    \'vimagit']
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#buffer_min_count = 2
let g:airline#extensions#neomake#error_symbol = '✖'
let g:airline#extensions#neomake#warning_symbol = '⚠'
let g:airline_skip_empty_sections = 1
" }}}
" cohama/lexima.vim {{{
" Enable space rules only for certain filetypes
let g:lexima_enable_space_rules = 0
for rule in g:lexima#space_rules
    let rule.filetype = ['rust', 'toml']
    call lexima#add_rule(rule)
endfor
" }}}
" davidhalter/jedi-vim {{{
let g:jedi#completions_enabled = 0
" }}}
" easymotion/vim-easymotion {{{
let g:EasyMotion_smartcase = 1  " Turn on case sensitive feature
let g:EasyMotion_startofline = 0  " Keep cursor column during JK motion
" }}}
" junegunn/fzf {{{
" Remove statusline from FZF popup
augroup fzf
  autocmd!
  au FileType fzf set laststatus=0 noruler
    \| au BufLeave <buffer> set laststatus=2 ruler
augroup END
" Fuzzy file finder with preview window
"   :Files[!]        -> fzf in current dir
"   :Files[!] <dir>  -> fzf in given dir
"   Adding the bang puts fzf across the whole window
command! -bang -nargs=? -complete=dir Files
    \ call fzf#vim#files(<q-args>, fzf#vim#with_preview(), <bang>0)
" Recursive search with fuzzy selector and preview window
"   :Rg[!]                -> Dynamic search using fzf
"   :Rg[!] <search-term>  -> Static search with rg + dynamic selection with fzf
"   Adding the bang puts fzf across the whole window
command! -bang -nargs=* Rg
  \ call fzf#vim#grep(
  \   'rg --column --line-number --no-heading --color=always --smart-case '.shellescape(<q-args>), 1,
  \   fzf#vim#with_preview(), <bang>0)
" }}}
" junegunn/limelight.vim {{{
" Highlight complete top level elements
let g:limelight_bop = '^\s*\n\zs\S'
let g:limelight_eop = '\ze\n\s*\n\S'
" }}}
" LaTeX-Box-Team/LaTeX-Box {{{
let g:LatexBox_Folding = 1
" }}}
" ludovicchabant/vim-gutentags {{{
let g:gutentags_cache_dir = '~/.cache/gutentag'
let g:gutentags_exclude_filetypes = ['gitcommit', 'gitconfig', 'gitrebase', 'gitsendemail', 'git']
" }}}
" majutsushi/tagbar {{{
let g:tagbar_autofocus = 1
let g:tagbar_sort = 0
let g:tagbar_compact = 1
let g:tagbar_iconchars=['▸', '▾']
let g:tagbar_type_make = {
    \ 'kinds': [
        \ 'm:macros',
        \ 't:targets'
    \ ]
\ }
let g:tagbar_type_ansible = {
    \ 'ctagstype': 'ansible',
    \ 'kinds': [
        \ 't:tasks'
    \ ]
\ }
let g:tagbar_type_rust = {
    \ 'ctagstype': 'rust',
    \ 'kinds': [
        \ 'n:modules',
        \ 's:structs',
        \ 'i:interfaces',
        \ 'c:implementations',
        \ 'f:functions',
        \ 'g:enums',
        \ 't:typedefs',
        \ 'v:variables',
        \ 'M:macros',
        \ 'm:fields',
        \ 'e:enumerators',
        \ 'F:methods',
    \ ]
\ }
let g:tagbar_type_typescript = {
    \ 'ctagstype': 'typescript',
    \ 'kinds': [
        \ 'c:classes',
        \ 'n:modules',
        \ 'f:functions',
        \ 'v:variables',
        \ 'v:varlambdas',
        \ 'm:members',
        \ 'i:interfaces',
        \ 'e:enums'
    \ ]
\ }
" }}}
" mattn/gist-vim {{{
let g:gist_detect_filetype = 1
let g:gist_open_browser_after_post = 1
" }}}
" neomake/neomake {{{
let g:neomake_open_list = 2
let g:neomake_haskell_enabled_makers = ['hdevtools', 'hlint']
let g:neomake_python_enabled_makers = []
augroup au_neomake_rust_enabled_makers
    autocmd!
    au FileType rust call s:RustMakers()
    function! s:RustMakers()
        let g:neomake_enabled_makers = ['clippy']
    endfunction
augroup END
" }}}
" racer-rust/vim-racer {{{
let g:racer_experimental_completer = 1
" }}}
" rhysd/clever-f.vim {{{
let g:clever_f_smart_case = 1
" }}}
" scrooloose/nerdcommenter {{{
let g:NERDCommentWholeLinesInVMode = 1
let g:NERDSpaceDelims = 1
" }}}
" scrooloose/nerdtree {{{
let g:NERDTreeIgnore = ['\~$', '\.pyc$', '\.class$', '\.pid$', '\.o$', '\.pdf$']
let g:NERDTreeMinimalUI = 1
let g:NERDTreeChDirMode = 2
let g:NERDTreeMapActivateNode = 'l'
let g:NERDTreeMapJumpParent = 'h'
" }}}
" sheerun/vim-polyglot {{{
" I have better alternative plugins for the following languages
let g:polyglot_disabled = ['latex', 'markdown', 'octave', 'python', 'rust', 'tmux', 'typescript']
" }}}
" Shougo/echodoc.vim {{{
let g:echodoc_enable_at_startup = 1
" }}}
" Shougo/neopairs.vim {{{
let g:neopairs#enable = 1
" }}}
" SirVer/ultisnips {{{
let g:UltiSnipsEditSplit='vertical'  " Let the UltiSnipsEdit split
" }}}
" vim-pandoc/vim-pandoc {{{
let g:pandoc#filetypes#pandoc_markdown = 0
" }}}
" Completion settings {{{
let g:deoplete#enable_at_startup = 1
let g:deoplete#sources#clang#libclang_path = '/usr/lib/libclang.so'
let g:deoplete#sources#clang#clang_header = '/usr/include/clang'
let g:SuperTabDefaultCompletionType = 'context'
" }}}
" }}}
" General mappings {{{
" jj/fd in insert mode to go to normal mode
inoremap jj <Esc>:w<CR>
inoremap fd <Esc>
" Use backspace to delete character
noremap <BS> X
" Make Y behave like other capitals
nnoremap Y y$
" Center search matches to center of screen
nnoremap n nzz
nnoremap N Nzz
nnoremap * *zz
nnoremap # #zz
nnoremap g* g*zz
nnoremap g# g#zz
" Space to (un)fold
nnoremap <leader><leader> za
vnoremap <leader><leader> za
" Ex-ex
nnoremap Q <nop>
" Funny story: before this keybind, I realised I was actually typing the Z
" with my thumb, because typing both Z and Q with my pinky takes up too much
" time.
nnoremap QQ ZQ
" Select all
nnoremap vA ggVG
" Create newlines like o and O, but stay in normal mode
nnoremap zj o<Esc>k
nnoremap zk O<Esc>j
" Autoformat whole file
nnoremap <leader>= mzggVG='z
" Sort lines
vnoremap <leader>s :!sort<CR>
" Sudo write
cnoremap w!! w !sudo tee % >/dev/null
" ,w to write
nnoremap <leader>w :w<CR>
" Use C-O and C-P to shift between edited parts
nnoremap <C-P> <C-I>
" <Enter> to clear the current search highlighting
nnoremap <silent> <CR> <CR>:noh<CR>
" Make < and > behave like they should
vnoremap < <gv
vnoremap > >gv
" Jump to the end after pasting
vnoremap <silent> y y`]
vnoremap <silent> p p`]
nnoremap <silent> p p`]
" Markdown mappings
augroup markdown_mappings
    autocmd!
    au FileType markdown,journal call s:MardownMappings()
    function! s:MardownMappings()
        nnoremap <buffer> <leader>1 yypVr=:redraw<CR>
        nnoremap <buffer> <leader>2 yypVr-:redraw<CR>
        nnoremap <buffer> <leader>3 mzI###<Space><Esc>`zllll<CR>
        nnoremap <buffer> <leader>v :LivedownPreview<CR>
    endfunction
augroup END
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
    execute '!send_blame_mail ' . expand('%:p') . ' ' . line('.')
endfunction
nnoremap <leader>B :call Blame()<CR>

" }}}
" Plugin mappings {{{
" AndrewRadev/splitjoin.vim {{{
"   gS  Split a one-liner into multiple lines
"   gJ  (with the cursor on the first line of a block) to join a block into a
"       single-line statement.
" }}}
" Plug 'bitc/vim-hdevtools' {{{
augroup haskell_mappings
    autocmd!
    au FileType haskell call s:HaskellMappings()
    function! s:HaskellMappings()
"   F1  Show type information. Multiple presses for expansion
        nnoremap <buffer> <leader>ht :HdevtoolsType<CR>
"   F2  Clear the type information
        nnoremap <buffer> <silent> <leader>hc :HdevtoolsClear<CR>
    endfunction
augroup END
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
" davidhalter/jedi-vim {{{
"   gd          Go to Python definition, falls back to assignments
"   K           Show docs
"   <leader>ja  Go to assignment of element under cursor
"   <leader>jn  Show usages of element under cursor. (Could be non-exhaustive)
"   <leader>jr  Rename variable. (Seems dangerous to me tbh)
let g:jedi#goto_command = 'gd'
let g:jedi#documentation_command = 'K'
let g:jedi#completions_command = ''
let g:jedi#goto_assignments_command = '<leader>ja'
let g:jedi#goto_definitions_command = ''
let g:jedi#usages_command = '<leader>jn'
let g:jedi#rename_command = '<leader>jr'
" }}}
" easy-motion/vim-easymotion {{{
"                     Disable default mappings
let g:EasyMotion_do_mapping = 0
"   s                 Bi-directional find. Jump to anywhere with s{char}{label}
"noremap s <Plug>(easymotion-s)
"   s                 Bi-directional find. Jump to anywhere with s{char}{char}{label}
map s <Plug>(easymotion-s2)
"   <leader>j         Easymotion up. Jump up with <leader>j{label}
"map <leader>j <Plug>(easymotion-j)
"   <leader>k         Easymotion down. Jump down with <leader>k{label}
"map <leader>k <Plug>(easymotion-k)
"   <leader>l         Easymotion forward. Jump forward with <leader>l{label}
"map <leader>l <Plug>(easymotion-lineforward)
"   <leader>h         Easymotion backward. Jump backward with <leader>h{label}
"map <leader>h <Plug>(easymotion-linebackward)
" }}}
" Plug junegunn/fzf {{{
nnoremap <leader>o :Files<CR>
nnoremap <leader>g :Rg<space>
vnoremap <leader>g "hy:Rg <C-R>h<CR>
nnoremap <leader>t :Tags<CR>
" }}}
" junegunn/limelight.vim {{{
"   <leader>l          Toggle Limelight
nnoremap <leader>l :Limelight!!<CR>
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
augroup latex_mappings
    autocmd!
    au FileType tex call s:LaTeXMappings()
    function! s:LaTeXMappings()
"   [[          Start an environment
        imap <buffer> [[ \begin{
"   ]]          Close the environment
        imap <buffer> ]] <Plug>LatexCloseCurEnv
    endfunction
augroup END
" }}}
" majutsushi/tagbar {{{
"   <leader>m  View output file.
nnoremap <silent> <leader>m :TagbarToggle<CR>
" }}}
" racer-rust/vim-racer {{{
augroup rust_racer_mappings
    autocmd!
    au FileType rust call s:RacerMappings()
    function! s:RacerMappings()
        nmap <buffer> gd <Plug>(rust-def)
        nmap <buffer>  K <Plug>(rust-doc)
    endfunction
augroup END
" }}}
" Procrat/jedi-vim {{{
let g:jedi#auto_initialization = 0
augroup python_jedi_mappings
    autocmd!
    au FileType python call s:JediMappings()
    function! s:JediMappings()
        nmap <buffer>         gd <Plug>JediGoto
        nmap <buffer> <leader>ja <Plug>JediGotoAssignments
        nmap <buffer> <leader>ju <Plug>JediUsages
        map  <buffer> <leader>jr <Plug>JediRename
        nmap <buffer>          K <Plug>JediDocumentation
        imap <buffer>    <space> <Plug>JediSmartAutoMapping
    endfunction
augroup END
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
" Plug 'shime/vim-livedown' {{{
"   <leader>v  Opens generated markdown in browser. See markdown_mappings.
" }}}
" SirVer/ultisnips {{{
"   ß        Expand ultisnips
let g:UltiSnipsExpandTrigger='ß'
"   <C-j>    Move to next editable part in the snippet
"   <C-k>  Move the previous editable part in the snippet
" }}}
" terryma/vim-smooth-scroll {{{
nnoremap <silent> <C-u> :call smooth_scroll#up(&scroll, 10, 2)<CR>
nnoremap <silent> <C-d> :call smooth_scroll#down(&scroll, 10, 2)<CR>
nnoremap <silent> <C-b> :call smooth_scroll#up(&scroll*2, 10, 4)<CR>
nnoremap <silent> <C-f> :call smooth_scroll#down(&scroll*2, 10, 4)<CR>
" }}}
" tpope/vim-unimpaired {{{
"   A lot of mapping starting with [ and ]. A full list can be found here:
"   https://github.com/tpope/vim-unimpaired/blob/master/doc/unimpaired.txt
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
" Comletion mappings {{{
let g:SuperTabContextDefaultCompletionType = '<C-n>'
" Make other popup menu keybindings like in IDEs
inoremap <expr> <Esc>      pumvisible() ? "\<C-e>" : "\<Esc>"
inoremap <expr> <Down>     pumvisible() ? "\<C-n>" : "\<Down>"
inoremap <expr> <Up>       pumvisible() ? "\<C-p>" : "\<Up>"
inoremap <expr> <PageDown> pumvisible() ? "\<PageDown>\<C-p>\<C-n>" : "\<PageDown>"
inoremap <expr> <PageUp>   pumvisible() ? "\<PageUp>\<C-p>\<C-n>" : "\<PageUp>"
" }}}
" }}}
" Misc {{{

let g:python3_host_prog = '/usr/bin/python3'

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
    "   Highlight todo files and other textfiles with vim-journal
    "   (and override detection of Markdown and text filetypes)
    au BufNewFile,BufRead *.txt,*todo*,*TODO* setlocal filetype=journal

    " Turn on spelling for some filetypes
    au FileType tex,mail,markdown,gitcommit setlocal spell

    " Don't wrap lines in the middle of a word
    au FileType text,journal,markdown setlocal linebreak

    " Set wrapping and markdown folding for Markdown and journal files
    au FileType journal,markdown setlocal textwidth=80 foldmethod=expr foldexpr=FoldexprMarkdown(v:lnum)

    " Autoscale Quickfix window, don't let it appear in buffer lists and close
    " it with `q`
    au FileType qf
        \ call s:AdjustWindowHeight(3, 10) |
        \ setlocal nobuflisted |
        \ nnoremap <buffer> <silent> q :close<CR>

    " Also quit help files with `q`
    au FileType help nnoremap <buffer> <silent> q :close<CR>

    " Reload .vimrc on save
    au BufWritePost .vimrc source %

    " Run all makers (linters etc) on save
    au BufWritePost * Neomake

    " Compile TypeScript and show errors on save
    au BufWritePost *.ts call s:MakeAndCopen()

    " Run stylish-haskell when saving Haskell files
    au BufWritePost *.hs call s:StylishHaskell()

    " Set GHC options when configuring XMonad
    let s:xmonad_lib = expand('~/.xmonad/lib')
    au BufNewFile,BufRead ~/.xmonad/*
        \ let g:hdevtools_options = '-g-i' . s:xmonad_lib |
        \ let b:neomake_haskell_hdevtools_args =
            \ ['--verbosity', 'silent', 'exec', '--',
            \  'hdevtools', 'check', '-g-Wall', '-g-i' . s:xmonad_lib]

    " Automatic renaming of tmux window
    if exists('$TMUX')
        au BufEnter * if empty(&buftype) | call system('tmux rename-window '.expand('%:t:S')) | endif
        au VimLeave * call system('tmux set-window automatic-rename on')
    endif

    au FileType python call s:ExtraPythonMappings()

    " Stop vim-vue from slowing down (See https://github.com/posva/vim-vue)
    au FileType vue let g:vue_disable_pre_processors=1

    " Remove trailing whitespace automatically on write when desired (non-binary)
    au BufWritePre * call s:StripTrailingWhitespace()

augroup END

func! s:AdjustWindowHeight(minheight, maxheight)
    exe max([min([line('$'), a:maxheight]), a:minheight]) . 'wincmd _'
endfunction

func! s:MakeAndCopen()
    silent make
    if !empty(getqflist())
        copen
    else
        cclose
    endif
endfunc

func! s:StylishHaskell()
    let l:winview = winsaveview()
    silent! exe 'undojoin'
    silent! exe 'keepjumps %!stylish-haskell'
    call winrestview(l:winview)
endfunc

func! s:StripTrailingWhitespace()
    if &l:fileencoding ==? 'utf-8'
        let l:line_no = 1
        for l:line in getline(1, '$')
            call setline(l:line_no, substitute(l:line, '\s\+$', '', ''))
            let l:line_no = l:line_no + 1
        endfor
    endif
endfunction

" Thanks to Steve Losh (https://gist.github.com/sjl/1038710)
func! FoldexprMarkdown(lnum)
    let l:l1 = getline(a:lnum)

    if l:l1 =~# '^\s*$'
        " ignore empty lines
        return '='
    endif

    let l:l2 = getline(a:lnum+1)

    if l:l2 =~# '^==\+\s*'
        " next line is underlined (level 1)
        return '>1'
    elseif l:l2 =~# '^--\+\s*'
        " next line is underlined (level 2)
        return '>2'
    elseif l:l1 =~# '^#'
        " current line starts with hashes
        return '>'.matchend(l:l1, '^#\+')
    elseif a:lnum == 1
        " fold any 'preamble'
        return '>1'
    else
        " keep previous foldlevel
        return '='
    endif
endfunc

" Define Isort command and mapping for Python import sorting
func! s:ExtraPythonMappings()
    command! -range=% -nargs=* Isort :<line1>,<line2>! isort <args> -
    noremap <leader>i :Isort<CR>
endfunc

" }}}

" vim:foldmethod=marker:foldlevel=0
