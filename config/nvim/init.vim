scriptencoding 'utf-8'

" Plugins {{{

call plug#begin()

" -- Fast plugins (< 5ms on boot)
Plug 'AndrewRadev/splitjoin.vim'
Plug 'AndrewRadev/switch.vim'
Plug 'chrisbra/csv.vim'
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
Plug 'leafgarland/typescript-vim'
Plug 'lervag/vimtex'
Plug 'ludovicchabant/vim-gutentags'
Plug 'majutsushi/tagbar'
Plug 'maksimr/vim-jsbeautify', { 'for': [
            \ 'javascript', 'javascript.jsx', 'json', 'html', 'css'] }
Plug 'mattn/emmet-vim'
Plug 'mattn/gist-vim', { 'on': 'Gist' }
Plug 'mattn/webapi-vim', { 'on': 'Gist' }  " Dependency for gist-vim
Plug 'mg979/vim-visual-multi'
Plug 'neomake/neomake'
Plug 'plasticboy/vim-markdown'
Plug 'rhysd/clever-f.vim'
Plug 'rhysd/committia.vim'
" Plug 'a-watson/vim-gdscript'
Plug 'rust-lang/rust.vim'
Plug 'scrooloose/nerdtree', { 'on': 'NERDTreeToggle' }
Plug 'shime/vim-livedown'
Plug 'Shougo/echodoc.vim'
Plug 'terryma/vim-smooth-scroll'
Plug 'tmux-plugins/vim-tmux'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-obsession'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-sensible'
Plug 'tpope/vim-sleuth'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'
Plug 'vim-scripts/JavaDecompiler.vim'
Plug 'wellle/context.vim'

" -- Slightly slower plugins (5ms -- 50ms)
Plug 'chriskempson/base16-vim'
Plug 'easymotion/vim-easymotion'
Plug 'junegunn/vim-journal'
" I have better alternative plugins for the following languages
let g:polyglot_disabled = [
    \ 'latex',
    \ 'markdown',
    \ 'python',
    \ 'rust',
    \ 'tmux',
    \ 'typescript'
    \ ]
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

" }}}
" Colorscheme settings {{{

let g:base16colorspace = 256  " Access colors present in 256 colorspace
colorscheme base16-mocha
let g:airline_theme = 'base16'

" }}}
" General settings {{{

let g:mapleader = "\<Space>"
let g:maplocalleader = "\<Space>"
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
    \ 'csv',
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
" chrisbra/csv.vim {{{

" Make plugin aware of quoted newlines
let g:csv_nl = 1
" Highlight the current column
let g:csv_highlight_column = 'y'

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
" junegunn/limelight.vim {{{

" Highlight complete top level elements
let g:limelight_bop = '^\s*\n\zs\S'
let g:limelight_eop = '\ze\n\s*\n\S'

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
    \ 'git'
    \ ]

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
" plasticboy/vim-markdown {{{

" Fold heading together with content
let g:vim_markdown_folding_style_pythonic = 1
" Use my own, better foldtext (uses spaces instead of dots for visual ease)
let g:vim_markdown_override_foldtext = 0
" Show strikethroughs
let g:vim_markdown_strikethrough = 1

" }}}
" racer-rust/vim-racer {{{

let g:racer_experimental_completer = 1

" }}}
" rhysd/clever-f.vim {{{

let g:clever_f_smart_case = 1

" }}}
" scrooloose/nerdtree {{{

let g:NERDTreeIgnore = ['\~$', '\.pyc$', '\.class$', '\.pid$', '\.o$', '\.pdf$']
let g:NERDTreeMinimalUI = 1
let g:NERDTreeChDirMode = 2
let g:NERDTreeMapActivateNode = 'l'
let g:NERDTreeMapJumpParent = 'h'

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
" wellle/context.vim {{{

" Don't show the "<context.vim>" tag
let g:context_highlight_tag = '<hide>'
" Stop screen flicker
let g:context_nvim_no_redraw = 1

" }}}
" Completion settings {{{

let g:deoplete#enable_at_startup = 1
let g:deoplete#sources#clang#libclang_path = '/usr/lib/libclang.so'
let g:deoplete#sources#clang#clang_header = '/usr/include/clang'
call deoplete#custom#var('omni', 'input_patterns', {
    \ 'tex': g:vimtex#re#deoplete
    \})
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
" Save current file
nnoremap <leader>s :w<CR>
" Save all files and exit (useful when using vim as git mergetool)
nnoremap ZA :wqa<CR>
" Use C-O and C-P to shift between edited parts
nnoremap <C-P> <C-I>
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
" Window management
noremap <leader>w <C-w>
noremap + <C-w>+
noremap - <C-w>-
noremap <silent> <leader>wm :call ToggleZoom()<CR>
" Buffer management
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

let g:EasyMotion_do_mapping = 0
"   s<char><char><label>   Bi-directional find. Jump to the right place with
"                          the label that shows up. Works across windows in
"                          normal mode, within window in visual mode.
nmap s <Plug>(easymotion-overwin-s2)
vmap s <Plug>(easymotion-s2)
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
" junegunn/limelight.vim {{{

"   <leader>l         Toggle Limelight
nnoremap <leader>l :Limelight!!<CR>

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
" majutsushi/tagbar {{{

"   <leader>m  View output file.
nnoremap <silent> <leader>m :TagbarToggle<CR>

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
"   <C-k>    Move the previous editable part in the snippet

" }}}
" terryma/vim-smooth-scroll {{{

nnoremap <silent> <C-u> :call smooth_scroll#up(&scroll, 10, 2)<CR>
nnoremap <silent> <C-d> :call smooth_scroll#down(&scroll, 10, 2)<CR>
nnoremap <silent> <C-b> :call smooth_scroll#up(&scroll*2, 10, 4)<CR>
nnoremap <silent> <C-f> :call smooth_scroll#down(&scroll*2, 10, 4)<CR>

" }}}
" tpope/vim-commentary {{{

"   <leader>c<motion> Toggle comments over <motion>
map <leader>c <Plug>Commentary
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
    au FileType journal,markdown setlocal textwidth=80

    " Autoscale Quickfix window, don't let it appear in buffer lists and close
    " it with `q`
    au FileType qf
        \ call s:AdjustWindowHeight(3, 10) |
        \ setlocal nobuflisted |
        \ nnoremap <buffer> <silent> q :close<CR>

    " Also quit help files with `q`
    au FileType help nnoremap <buffer> <silent> q :close<CR>

    " Reload .vimrc on save
    au BufWritePost .vimrc,*/nvim/init.vim source %

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
        let l:save = winsaveview()
        keeppatterns %s/\s\+$//e
        call winrestview(l:save)
    endif
endfunction

" Define Isort command and mapping for Python import sorting
func! s:ExtraPythonMappings()
    command! -range=% -nargs=* Isort :<line1>,<line2>! isort <args> -
    noremap <buffer> <leader>i :Isort<CR>
endfunc

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

" }}}

" vim:foldmethod=marker:foldlevel=0
