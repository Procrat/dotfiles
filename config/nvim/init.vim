scriptencoding 'utf-8'

" Plugins {{{

call plug#begin()

" -- Fast plugins (< 5ms on boot)
Plug 'AndrewRadev/splitjoin.vim'
Plug 'AndrewRadev/switch.vim'
Plug 'chrisbra/csv.vim'
Plug 'christoomey/vim-tmux-navigator'
Plug 'cohama/lexima.vim'
Plug 'dense-analysis/ale'
Plug 'godlygeek/tabular'
Plug 'honza/vim-snippets'
Plug '/usr/share/vim/vimfiles/plugin/fzf.vim'
Plug 'junegunn/fzf.vim'
Plug 'junegunn/vim-peekaboo'
Plug 'leafgarland/typescript-vim'
Plug 'ludovicchabant/vim-gutentags'
Plug 'maksimr/vim-jsbeautify', { 'for': [
    \ 'javascript', 'javascript.jsx', 'json', 'html', 'css'] }
Plug 'mattn/emmet-vim'
Plug 'mattn/gist-vim', { 'on': 'Gist' }
Plug 'mattn/webapi-vim', { 'on': 'Gist' }  " Dependency for gist-vim
Plug 'mg979/vim-visual-multi'
Plug 'nvim-lua/diagnostic-nvim'
Plug 'plasticboy/vim-markdown'
Plug 'preservim/nerdtree', { 'on': 'NERDTreeToggle' }
Plug 'preservim/tagbar'
Plug 'psliwka/vim-smoothie'
Plug 'rhysd/clever-f.vim'
Plug 'rhysd/committia.vim'
Plug 'shime/vim-livedown'
Plug 'Shougo/echodoc.vim'
Plug 'tmux-plugins/vim-tmux'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-sensible'
Plug 'tpope/vim-sleuth'
Plug 'tpope/vim-surround'
Plug 'vim-scripts/JavaDecompiler.vim'
Plug 'wellle/context.vim'

" -- Slightly slower plugins (3ms -- 50ms)
Plug 'easymotion/vim-easymotion'  " ~3ms
Plug 'junegunn/vim-journal'  " ~8ms for journal files
" Loaded on demand for specific filetypes (Python, Vue)
Plug 'neovim/nvim-lspconfig', { 'for': [] }  " ~10ms
Plug 'norcalli/nvim-base16.lua'  " ~6ms
Plug 'rust-lang/rust.vim'  " ~15ms for Rust
" I have better alternative plugins for the following languages
let g:polyglot_disabled = [
    \ 'csv',
    \ 'latex',
    \ 'markdown',
    \ 'python',
    \ 'python-compiler',
    \ 'rust',
    \ 'tmux',
    \ 'typescript',
    \ ]
Plug 'sheerun/vim-polyglot'  " 20-50ms, depending on filetype
Plug 'vim-airline/vim-airline'  " ~40ms
Plug 'vim-airline/vim-airline-themes'

" -- Slow plugins (> 50ms)
" I don't write enough tex to optimise this
Plug 'lervag/vimtex'  " ~80ms for (La)TeX
" Completely defer to insert mode
Plug 'SirVer/ultisnips', { 'on': [] }  " ~105ms for Ruby, <3ms for others 🤷
" I'll fix this when I write some RoR again
Plug 'tpope/vim-rails'  " ~60ms for Rails

" -- Completion plugins (all < 3ms)
Plug 'ervandew/supertab'
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'deoplete-plugins/deoplete-tag'
Plug 'deoplete-plugins/deoplete-zsh'
Plug 'racer-rust/vim-racer'
Plug 'Shougo/deoplete-lsp'
Plug 'Shougo/neco-vim'

call plug#end()

" Defer loading of some plugins to insert mode
augroup load_insert_plugins
    autocmd!
    autocmd InsertEnter * call plug#load('ultisnips')
        \| autocmd! load_insert_plugins
augroup END

" }}}
" Colorscheme settings {{{

lua << EOF
    nvim = require 'nvim'
    local base16 = require 'base16'
    base16(base16.themes[nvim.env.BASE16_THEME or "mocha"], true)
EOF
let g:airline_theme = 'base16'

" }}}
" General settings {{{

let g:mapleader = "\<Space>"
let g:maplocalleader = "\<Space>"

" Hides buffers instead of closing
set hidden
" Persistent undo (slow for large files, so turned off with autocommand)
set undofile
" Line numbering
set number
" Show vertical column at 80
set colorcolumn=80
" Highlight current line
set cursorline
" Ignore case when searching except if search has uppercase letters
set ignorecase smartcase
" Always use a menu for autocompletion, insert longest match, no preview pane
set completeopt=menuone,longest
" Ignore files in autocompletion
set wildignore=*.o,*.obj,*.pyc,*.class,*.orig,*/.git/*
" Maximum height of the autocompletion popup menu (pum)
set pumheight=8
" Mouse interactivity
set mouse=a
" Copy/paste with X11 clipboard
set clipboard=unnamedplus
" Paste mode shortcut
set pastetoggle=<leader>p
" Start scrolling earlier
set scrolloff=6
" Make the new window below or right of the current one
set splitbelow
set splitright
" Check modelines (like the one at the bottom of this file)
set modelines=2
" Use ripgrep to grep and always print file name in Quickfix list
set grepprg=rg\ --vimgrep\ --word-regexp\ --fixed-strings\ \"$*\"
set grepformat=%f:%l:%c:%m
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
" Use conceal (and don't waste space like level 1)
set conceallevel=2
" Let wrapped lines keep indentation, but show a ">>" to mark them
set breakindent
set showbreak=>>

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

" Number of spaces that a tab respresents
set tabstop=8
" Number of spaces for an (auto-)indent
set shiftwidth=4
" Number of spaces that a Tab feels like while editing
set softtabstop=4
" Turn tabs into spaces
set expandtab
" Round off indentation to multiple of shiftwidth
set shiftround
" Indent after '{' and after keywords (if, for, else, while, do, switch)
set smartindent
" Try to maintain existing indentation
set copyindent
set preserveindent

" Indentation per filetype
augroup indentation
    autocmd!

    au FileType xml,html,xhtml,htmldjango,eruby,xslt
        \ setlocal tabstop=2 shiftwidth=2 softtabstop=2 textwidth=120
        \ colorcolumn=120
    au FileType js setlocal shiftwidth=2 softtabstop=2 textwidth=120
        \ colorcolumn=120
    au FileType css,scss setlocal shiftwidth=2 softtabstop=2
    au FileType python setlocal nocindent noshiftround shiftwidth=4
        \ softtabstop=4 textwidth=79 colorcolumn=79
    au FileType haskell setlocal shiftwidth=4 softtabstop=4
    au FileType rust setlocal shiftwidth=4 softtabstop=4 textwidth=99
        \ colorcolumn=99
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
" Don't draw separators for empty sections. (Adds ~35ms to startup time!)
" let g:airline_skip_empty_sections = 1
" Make airline a tiny bit faster (~5ms)
let g:airline_highlighting_cache = 1
" Optimization: don't search for all possible extensions
let g:airline_extensions = [
    \ 'ale',
    \ 'branch',
    \ 'csv',
    \ 'fugitiveline',
    \ 'gutentags',
    \ 'quickfix',
    \ 'tabline',
    \ 'tagbar',
    \ 'term',
    \ 'vimtex'
    \ ]
let g:airline#extensions#ale#error_symbol = '✖'
let g:airline#extensions#ale#warning_symbol = '⚠'
let g:airline#extensions#ale#show_line_numbers = 0
" Show column name for CSVs instead of index
let g:airline#extensions#csv#column_display = 'Name'
let g:airline#extensions#tabline#buffer_min_count = 2
let g:airline#extensions#tabline#formatter = 'unique_tail_improved'

" }}}
" chrisbra/csv.vim {{{

" Make plugin aware of quoted newlines
let g:csv_nl = 1
" Highlight the current column
let g:csv_highlight_column = 'y'

" }}}
" dense-analysis/ale {{{

" Adds about 30ms (depending on linter) to boot time if we do
let g:ale_lint_on_enter = 0
" Don't set signs in an extra column for warnings and errors
let g:ale_set_signs = 0

" }}}
" easymotion/vim-easymotion {{{

let g:EasyMotion_smartcase = 1
" Turn of messages like 'Jumping to [l,c]' and 'EasyMotion: Cancelled'
let g:EasyMotion_verbose = 0

" }}}
" junegunn/fzf {{{

" Remove statusline from FZF popup
augroup fzf
    autocmd!
    au FileType fzf set laststatus=0 noruler
        \| au BufLeave <buffer> set laststatus=2 ruler
augroup END

" }}}
" lervag/vimtex {{{

let g:tex_flavor = 'latex'
let g:vimtex_fold_enabled = 1

" }}}
" ludovicchabant/vim-gutentags {{{

let g:gutentags_cache_dir = '~/.cache/gutentag'
let g:gutentags_exclude_filetypes = [
    \ 'gitcommit',
    \ 'gitconfig',
    \ 'gitrebase',
    \ 'gitsendemail',
    \ 'git',
    \ ]

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
" nvim-lua/diagnostic-nvim {{{

" Don't show stuff in the gutter; we still have underlining for errors.
let g:diagnostic_show_sign = 0

" }}}
" plasticboy/vim-markdown {{{

" Use my own, better foldtext (uses spaces instead of dots for visual ease)
let g:vim_markdown_override_foldtext = 0
" Show strikethroughs
let g:vim_markdown_strikethrough = 1

" }}}
" preservim/nerdtree {{{

let g:NERDTreeIgnore = ['\~$', '\.pyc$', '\.class$', '\.pid$', '\.o$', '\.pdf$']
let g:NERDTreeMinimalUI = 1
let g:NERDTreeChDirMode = 2
let g:NERDTreeMapActivateNode = 'l'
let g:NERDTreeMapJumpParent = 'h'

" }}}
" preservim/tagbar {{{

let g:tagbar_autofocus = 1
let g:tagbar_sort = 0
let g:tagbar_compact = 1
let g:tagbar_iconchars=['▸', '▾']
let g:tagbar_type_ansible = {
    \ 'ctagstype': 'ansible',
    \ 'kinds': [
        \ 't:tasks'
    \ ],
    \ 'sort': 0
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
" racer-rust/vim-racer {{{

let g:racer_experimental_completer = 1

" }}}
" rhysd/clever-f.vim {{{

let g:clever_f_smart_case = 1

" }}}
" sheerun/vim-polyglot {{{

" Stop vim-vue from slowing down (See https://github.com/posva/vim-vue)
let g:vue_disable_pre_processors = 1

" }}}
" Shougo/echodoc.vim {{{

let g:echodoc_enable_at_startup = 1

" }}}
" SirVer/ultisnips {{{

" Let :UltiSnipsEdit split horizontally or vertically
let g:UltiSnipsEditSplit='context'

" }}}
" wellle/context.vim {{{

" Don't show the "<context.vim>" tag
let g:context_highlight_tag = '<hide>'
" Stop screen flicker
let g:context_nvim_no_redraw = 1

" }}}
" Completion settings {{{

let g:deoplete#enable_at_startup = 1
call deoplete#custom#var('omni', 'input_patterns', {
    \ 'tex': g:vimtex#re#deoplete
    \})
" Enable context-dependent completion
let g:SuperTabDefaultCompletionType = 'context'
" And fall back to normal <c-n>
let g:SuperTabContextDefaultCompletionType = '<C-n>'

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
" Save current file
nnoremap <leader>s :w<CR>
" Save all files and exit (useful when using vim as git mergetool)
nnoremap ZA :wqa<CR>
" Use C-O and C-P to shift between edited parts
nnoremap <C-p> <C-i>
" Make < and > behave like they should
vnoremap < <gv
vnoremap > >gv
" Jump to the end after pasting
vnoremap <silent> y y`]
vnoremap <silent> p p`]
nnoremap <silent> p p`]
" Window management
noremap <leader>w <C-w>
noremap + <C-w>+
noremap - <C-w>-
noremap <silent> <leader>wm :call ToggleZoom()<CR>
" Buffer management
nnoremap <Tab> :bnext<CR>
nnoremap <S-Tab> :bprev<CR>
nnoremap <leader>n :enew<cr>
nnoremap <leader>d :bd<cr>
nnoremap <leader>D :bd!<cr>
" Use S to grep (dependent on format of grepprg)
nnoremap S :grep! <C-R><C-W><CR>:cw<CR>
vnoremap S "hy:grep! <C-R>h<CR>:cw<CR>
" Error management
nnoremap <leader>el :call ToggleLocationList()<CR>
nnoremap <leader>ev :ALEInfo<CR>
nnoremap <leader>en :lnext<CR>
nnoremap <leader>ep :lprevious<CR>
" See also <leader>es mapping in LSP mappings
" Send a blame mail from a git repo
function! Blame()
    execute '!send_blame_mail ' . expand('%:p') . ' ' . line('.')
endfunction
nnoremap <leader>B :call Blame()<CR>
" Markdown mappings for header decorations
augroup markdown_mappings
    autocmd!
    au FileType markdown,journal call s:MarkdownMappings()
    function! s:MarkdownMappings()
        nnoremap <buffer> <leader>1 yypVr=:redraw<CR>
        nnoremap <buffer> <leader>2 yypVr-:redraw<CR>
        nnoremap <buffer> <leader>3 mzI###<Space><Esc>`zllll<CR>
    endfunction
augroup END
" Define Isort command and mapping for Python import sorting
augroup python_isort_mappings
    autocmd!
    au FileType python call s:PythonIsortMappings()
    function! s:PythonIsortMappings()
        command! -range=% -nargs=* Isort :<line1>,<line2>! isort <args> -
        nnoremap <buffer> <leader>i :Isort<CR>
        vnoremap <buffer> <leader>i :Isort<CR>
    endfunction
augroup END

" }}}
" Plugin mappings {{{

" AndrewRadev/splitjoin.vim {{{

"   gS  Split a one-liner into multiple lines
"   gJ  (with the cursor on the first line of a block) to join a block into a
"       single-line statement.

" }}}
" AndrewRadev/switch.vim {{{

"   gs  Switch a language construct to another form

" }}}
" bitc/vim-hdevtools {{{

augroup haskell_mappings
    autocmd!
    au FileType haskell call s:HaskellMappings()
    function! s:HaskellMappings()
"   <leader>ht  Show type information. Multiple presses for expansion
        nnoremap <buffer> <leader>ht :HdevtoolsType<CR>
"   <leader>hc  Clear the type information
        nnoremap <buffer> <silent> <leader>hc :HdevtoolsClear<CR>
    endfunction
augroup END

" }}}
" chrisbra/csv.vim {{{

" ⚠ Can be slow for large CSVs

"   <C-Right>/L/W                     Move to field on the right
"   <C-Left>/E/H                      Move to field on the left
"   <Up>/K                            Move up in the same column
"   <Down>/J                          Move down in the same column
"   <Enter>                           Fold lines away that don't match value
"   <Space>                           Fold lines away that match value
"   <BS>                              Undo fold

" Text objects:
"   if                                Inner field
"   af                                Outer field
"   iL                                Inner line

" Commands include:
"   :SearchInColumn [<col>] /<pat>/   Search for <pat> in column
"   :HiColumn [<col>]                 Highight column
"   :HiColumn!                        Remove highlight
"   :[range]ArrangeColumn[!]          Vertically align columns
"                                     (Slow for large files!)
"   :[range]UnArrangeColumn           Undo vertical align
"   :[V]HeaderToggle                  Freeze/unfreeze (vertical) header
"   :CSVTabularize                    Draw a fancy table in a new buffer
"   :DeleteColumn, :Sort, :Column (to copy), :MoveColumn, :NewDelimiter

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

let g:EasyMotion_do_mapping = 0
"   s<char><char><label>   Bi-directional find. Jump to the right place with
"                          the label that shows up. Works across windows in
"                          normal mode, within window in visual mode.
nmap s <Plug>(easymotion-overwin-f2)
vmap s <Plug>(easymotion-bd-f2)
"   s<char><char><Space>   Bi-directional find. Jump to first match with
"                          <Space>.
let g:EasyMotion_space_jump_first = 1

" Mappings in EasyMotion's pending mode:
"   <CR>                   Execute now
"   <C-c>/<Esc>            Cancel
"   <C-h>/<BS>             Delete a character

" }}}
" junegunn/fzf {{{

"   <leader>o         Fzf file in current working directory
nnoremap <leader>o :Files<CR>
"   <leader>O         Fzf recently opened file
nnoremap <leader>O :History<CR>
"   <leader>gr        Fzf text (with selection in visual mode)
nnoremap <leader>gr :Rg<space>
vnoremap <leader>gr "hy:Rg <C-R>h<CR>
"   <leader>t         Fzf tag
nnoremap <leader>t :Tags<CR>
"   <leader>/         Fzf line in current buffer
nnoremap <leader>/ :BLines<CR>

" Commands include:
"   :BLines[!]           Fzf line in current buffer
"   :BLines[!] <query>   Same but with static filter
"   :Files[!]            Fzf file in working dir
"   :Files[!] <dir>      Fzf file in given directory
"   :Helptags[!]         Fzf help tag
"   :History[!]          Fzf recently opened file
"   :Maps[!]             Fzf normal mode mapping
"   :Rg[!]               Fzf text recursively
"   :Rg[!] <query>       Same but with static filter
"   :Tags[!]             Fzf tag
"   :Tags[!] <query>     Same but with static filter

" For :Files and :History, use enter key, CTRL-T, CTRL-X or CTRL-V to open
" selected files in the current window, in new tabs, in horizontal splits, or
" in vertical splits respectively.
" Adding the bang puts fzf across the whole window.

" }}}
" lervag/vimtex {{{

" Compilation bindings:
"   <leader>lc        Clean auxiliary files
"   <leader>lC        Clean auxiliary and output files
"   <leader>le        Toggle errors (quickfix)
"   <leader>lg        Show compilation status
"   <leader>li        Show info
"   <leader>lI        Show full info
"   <leader>lk        Stop compilation
"   <leader>ll        Compile (and keep running in continous mode)
"   <leader>lm        Show list of insert-mode mappings
"   <leader>lo        Show compilation output
"   <leader>lt        Open table of contents (sidebar)
"   <leader>lT        Toggle table of contents (sidebar)
"   <leader>lv        Open pdf for current document
"
" Navigation bindings:
"   ]]                Go to next end of a section
"   ][                Go to next beginning of a section
"   []                Go to previous end of a section
"   [[                Go to previous beginning of a section
"   ]m                Go to next start of an environment
"   ]M                Go to next end of an environment
"   [m                Go to previous start of an environment
"   [M                Go to previous end of an environment
"   K                 Open documentation for package
"
" Defined text objects:
"   c   command
"   d   delimiter
"   e   environment
"   $   inline math
"   P   section
"   m   item
" So you can use [cd]s[ec$d] to change/delete surrounding environment /
" command / math delimiter / math environment.
"
" Insert mode bindings:
"   ]]                Close current environment
" Use <leader>lm to see other ones.
"
" Also bound, but probably won't use:
"   <leader>l[GKLqrsxX]
"   ts[fcedD]
"   <F7>
"   ]/, ]* [/, [*

" }}}
" mattn/emmet-vim {{{

"   <C-y>,            Interpret text before cursor with emmet.

" }}}
" mg979/vim-visual-multi {{{

" Start multi-cursor mode
"   <C-n>             ... by matching word under cursor
"   <C-↓/↑>           ... vertically
"   <S-←/→>           ... horizontally
"   \\A               ... by matching word under cursor and selecting all
"                         occurrenes
"   \\/               ... by matching a regex
" In multi-cursor mode:
"   n/N               Get next/previous occurrence
"   [/]               Select next/previous cursor
"   q                 Skip current cursor and get next occurrence
"   Q                 Remove current cursor and go back to previous occurrence
"   <Tab>             Switch between cursor (normal) and extend (visual) mode
"   m<motion>         Select all patterns in given <motion>
"   gc                (extend/visual mode) like c, but preserve case
"   <Esc>             Quit multi-cursor mode
" See :h visual-multi for more mappings.

" }}}
" preservim/nerdtree {{{

"   <leader>T  Open NERD Tree
nnoremap <leader>T :NERDTreeToggle<CR>

" }}}
" preservim/tagbar {{{

"   <leader>m  View output file.
nnoremap <silent> <leader>m :TagbarToggle<CR>

" }}}
" racer-rust/vim-racer {{{

"   gd         Go to definition
"   K          Open docs for item under cursor
augroup rust_racer_mappings
    autocmd!
    au FileType rust call s:RacerMappings()
    function! s:RacerMappings()
        nmap <buffer> gd <Plug>(rust-def)
        nmap <buffer>  K <Plug>(rust-doc)
    endfunction
augroup END

" }}}
" shime/vim-livedown {{{

"   <leader>v  Opens generated markdown in browser. See markdown_mappings.
augroup markdown_livedown_mappings
    autocmd!
    au FileType markdown call s:MarkdownLiveDownMappings()
    function! s:MarkdownLiveDownMappings()
        nnoremap <buffer> <leader>v :LivedownPreview<CR>
    endfunction
augroup END


" }}}
" SirVer/ultisnips {{{

"   ß                   Expand snippet
let g:UltiSnipsExpandTrigger='ß'
"   <C-j>               Move to next editable part in the snippet
"   <C-k>               Move the previous editable part in the snippet

" Commands:
"   :UltiSnipsEdit[!]   Open (global) snippets file

" }}}
" tmux-plugins/vim-tmux {{{

"   K           Jump to relevant manpage section
"   :make       Invoke tmux config and place errors in quicklist
"   g!<motion>  Excecute lines as tmux commands
"   g!!         Excecute current line as tmux commands

" }}}
" tpope/vim-commentary {{{

"   <leader>c<motion> Toggle comments over <motion>
nmap <leader>c <Plug>Commentary
vmap <leader>c <Plug>Commentary
"   <leader>cc        Toggle comments on the current line or selected text in
"                     visual mode.
nmap <leader>cc <Plug>CommentaryLine
"   <leader>cl        Same, just to save the pain from unlearning Spacemacs
"                     bindings.
nmap <leader>cl <Plug>CommentaryLine
"   <leader>cy        Yank and then toggle.
nmap <leader>cy yy<Plug>CommentaryLine
xmap <leader>cy ygv<Plug>Commentary
"   <leader>cu        Uncomment current and adjacent lines.
nmap <leader>cu <Plug>Commentary<Plug>Commentary

" }}}
" tpope/vim-eunuch {{{

" Binds a bunch of useful commands, including:
"   :Delete     Delete a buffer and the file on disk simultaneously.
"   :Move       Rename a buffer and the file on disk simultaneously.
"   :Rename     Like :Move, but relative to the current file's containing
"               directory.
"   :Chmod      Change the permissions of the current file.
"   :Mkdir      Create a directory, defaulting to the parent of the current
"               file.
"   :SudoWrite  Write a privileged file with sudo.
"   :SudoEdit   Edit a privileged file with sudo.

" }}}
" tpope/vim-fugitive {{{

"   <leader>gs   Open interactive mode (also shows status like git status)
nnoremap <leader>gs  :Git<CR>
"   <leader>gdf  Open git df in new window
nnoremap <leader>gdf :Git df<CR>
"   <leader>gdc  Open git dfc in new window
nnoremap <leader>gdc :Git dfc<CR>
"   <leader>gds  Show side-by-side diff for current file
nnoremap <leader>gds :Gdiffsplit<CR>
"   <leader>glg  Show git lg in new window
nnoremap <leader>glg :Git lg<CR>
"   <leader>glp  Show git lgp in new window
nnoremap <leader>glp :Git lgp<CR>
"   <leader>gb   Show git blame in sidebar
nnoremap <leader>gb  :Git blame<CR>

" Common mappings in the summary buffer (:Git):
"   s                       Stage file or hunk
"   u                       Unstage file or hunk
"   U                       Unstage everything
"   X                       Discard change
"   =                       Toggle diff
"   I/P                     Invoke git add -p or reset -p
"   dd                      Perform :Gdiffsplit
"   dv                      Perform :Gvdiffsplit
"   ds/dh                   Perform :Ghdiffsplit
"   <CR>                    Open file
"   o                       Open file in new split
"   gO                      Open file in new vertical split
"   (/)                     Jump to previous/next file, hunk or revision
"   i                       Jump to next file or hunk, expanding diffs
"   cc                      Create commit
"   ca                      Amend last commit and edit message
"   ce                      Amend last commit without editing message
"   cf                      Create `fixup!` commit
"   cF                      Create `fixup!` commit and immediately rebase it
"   co<Space>               Populate command line with ":Git checkout "
"   czz                     Push stash
"   czp                     Pop topmost stash, preserving index
"   ri                      Perform interactive rebase using ancestor of commit
"   rf                      Perform autosquash rebase without editing the todo
"                           list, using ancestor of commit
"   ru                      Perform interactive rebase against @{upstream}
"   rp                      Perform interactive rebase against @{push}
"   rr                      Continue current rebase
"   rs                      Skip current commit and continue rebase
"   ra                      Abort current rebase

" Other useful commands:
"   :GWrite           Write and git add
"   :GMove/:GRename   Like git mv
"   :GDelete          Like git rm

" }}}
" tpope/vim-surround {{{

" Custom mapping to avoid collision with S in visual mode
let g:surround_no_mappings = 1
"   ys<motion><char>         Wrap <motion> with <char>
nmap ys  <Plug>Ysurround
"   yS<motion><char>         Same, but put inner part on an indented new line
nmap yS  <Plug>YSurround
"   cs<old-char><new-char>   Change surroundings from <old-char> to <new-char>
nmap cs  <Plug>Csurround
"   ds<char>                 Delete <char> surroundings
nmap ds  <Plug>Dsurround
"   gs<char>                 (visual mode) Wrap selecton in <char>
vmap gs <Plug>VSurround
"   gS<char>                 (visual mode) Same, but put inner part on an
"                            indented new line.
vmap gS  <Plug>VgSurround

" Targets:
"   (){}[]<>'"`  as is
"   t            for XML/HTML tags
"   wWsp         just so you can use e.g. csw( for ysiw(
" Replacements:
"   )}]>'"`      as is
"   ({[          add an extra space inside
"   t<           prompts for a custom tag
"   f            prompts for wrapping in a function call

" }}}
" Comletion mappings {{{

"   <Tab>/<S-Tab>  Trigger completion (shows popup menu)
"   <C-Tab>        Insert real tab

" With the popup menu open:
"   <C-n>/<Tab>/<Down>    Select next match
"   <C-p>/<S-Tab>/<Up>    Select previous match
"   <C-e>/<Esc>           Exit completion and return to initial text
"   <PageUp>/<PageDown>   Scroll half a page up/down in the menu
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

" Disable netrw completely. It's not behaving.
let loaded_netrwPlugin = 1

augroup vimrc_misc
    autocmd!

    " Recognise binary(-like) filetypes mainly to speed up boot time
    au BufReadPre *.bin,*.csv,*.dat,*.db,*.db-journal,*.hex,*.log,*.sqlite,*.sqlite3,*.wasm
        \ setlocal binary nofoldenable foldmethod=manual

    " Unset some features when dealing with large files to speed up boot time
    au BufReadPre * call s:UnsetFeaturesForLargeFiles(10 * 1024 * 1024)

    " Configure LSP for some filetypes. Unfortunately, we can't use the
    " FileType or BufRead events to configure nvim-lsp for some unknown
    " reason.
    au BufReadPre,BufNewFile *.py call s:ConfigureLsp('python')
    au BufReadPre,BufNewFile *.vue call s:ConfigureLsp('vue')

    " Recognize some file extensions
    "   Highlight todo files and other textfiles with vim-journal
    "   (and override detection of Markdown and text filetypes)
    au BufNewFile,BufRead *.txt,*todo*,*TODO* setlocal filetype=journal

    " Set GHC options when configuring XMonad
    let s:xmonad_lib = expand('~/.xmonad/lib')
    au BufNewFile,BufRead ~/.xmonad/*
        \ let g:hdevtools_options = '-g-i' . s:xmonad_lib |
        \ let b:neomake_haskell_hdevtools_args =
            \ ['--verbosity', 'silent', 'exec', '--',
            \  'hdevtools', 'check', '-g-Wall', '-g-i' . s:xmonad_lib]

    " Help for Matlab/Octave (with shortcut K)
    au FileType matlab,octave setlocal keywordprg=info\ octave\ --vi-keys\ --index-search

    " Turn on spelling for some filetypes
    au FileType tex,mail,markdown,gitcommit setlocal spell

    " Set wrapping but don't wrap lines in the middle of a word
    au FileType text,journal,markdown setlocal textwidth=80 linebreak

    " Disable lexima for Markdown-like files, esp. for space rules vs [ ]
    au FileType journal,markdown let b:lexima_disabled = 1

    " Autoscale Quickfix window, don't let it appear in buffer lists and close
    " it with `q`
    au FileType qf
        \ call s:AdjustWindowHeight(3, 10) |
        \ setlocal nobuflisted |
        \ nnoremap <buffer> <silent> q :close<CR>

    " Also quit help files with `q`
    au FileType help nnoremap <buffer> <silent> q :close<CR>

    " Remove trailing whitespace automatically on write when desired (non-binary)
    au BufWritePre * call s:StripTrailingWhitespace()

    " Reload .vimrc on save
    au BufWritePost .vimrc,*/nvim/init.vim source %

    " Compile TypeScript and show errors on save
    au BufWritePost *.ts call s:MakeAndCopen()

    " Run stylish-haskell when saving Haskell files
    au BufWritePost *.hs call s:StylishHaskell()

    " Close loclist when last buffer is closed
    autocmd QuitPre * if empty(&buftype) | lclose | endif

augroup END


function! s:UnsetFeaturesForLargeFiles(max_file_size)
    let l:size = getfsize(expand('<afile>'))
    if l:size < a:max_file_size && l:size != -2
        return
    endif

    syntax clear
    setlocal binary
    setlocal noswapfile
    setlocal nofoldenable
    setlocal foldmethod=manual
    " This is the significant one
    setlocal noundofile
    setlocal undolevels=-1
    setlocal nowritebackup
endfunction

func! s:AdjustWindowHeight(minheight, maxheight)
    exe max([min([line('$'), a:maxheight]), a:minheight]) . 'wincmd _'
endfunction

function! s:ConfigureLsp(language)
    call plug#load('nvim-lspconfig')

    if a:language ==# 'python'
        lua require'nvim_lsp'.pyls.setup{on_attach=require'diagnostic'.on_attach}
    elseif a:language ==# 'vue'
        lua require'nvim_lsp'.vuels.setup{on_attach=require'diagnostic'.on_attach}
    endif

    " Set up bindings
    nnoremap <buffer> <silent> <c-]>  <cmd>lua vim.lsp.buf.definition()<CR>
    nnoremap <buffer> <silent> gd     <cmd>lua vim.lsp.buf.definition()<CR>
    if a:language ==# 'python'
        nnoremap <buffer> <silent> gD <cmd>lua vim.lsp.buf.declaration()<CR>
    else
        nnoremap <buffer> <silent> gD
            \ :echoerr 'Jumping to declaration is not supported.'<CR>
    endif
    nnoremap <buffer> <silent> K      <cmd>lua vim.lsp.buf.hover()<CR>
    nnoremap <buffer> <silent> gr     <cmd>lua vim.lsp.buf.references()<CR>
    if a:language ==# 'python'
        nnoremap <buffer> <silent> gR <cmd>lua vim.lsp.buf.rename()<CR>
    else
        nnoremap <buffer> <silent> gR
            \ :echoerr 'Renaming is not supported.'<CR>
    endif
    if a:language ==# 'python'
        nnoremap <buffer> <silent> <leader>es
            \ <cmd>lua vim.lsp.util.show_line_diagnostics()<CR>
    else
        nnoremap <buffer> <silent> <leader>es
            \ :echoerr 'Line diagnostics are not supported.'<CR>
    endif
    " Other useful functionality for future reference: implementation(),
    " code_action(), formatting(), signature_help(), type_definition(),
    " workspace_symbol().
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
        let l:winview = winsaveview()
        " vint: -ProhibitCommandRelyOnUser -ProhibitCommandWithUnintendedSideEffect
        keeppatterns %s/\s\+$//e
        " vint: +ProhibitCommandRelyOnUser +ProhibitCommandWithUnintendedSideEffect
        call winrestview(l:winview)
    endif
endfunction

" Toggle window maximisation
function! ToggleZoom() abort
    if exists('t:zoomed') && t:zoomed
        execute t:zoom_winrestcmd
        let t:zoomed = 0
    else
        let t:zoom_winrestcmd = winrestcmd()
        resize
        vertical resize
        let t:zoomed = 1
    endif
endfunction

function! ToggleLocationList()
    for i in range(1, winnr('$'))
        if getbufvar(winbufnr(i), '&buftype') ==# 'quickfix'
            lclose
            return
        endif
    endfor
    lopen
endfunction

" }}}

" vim:foldmethod=marker:foldlevel=0
