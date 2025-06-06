scriptencoding 'utf-8'

" Plugins {{{

call plug#begin()

" -- Fast plugins (< 5ms on boot)
Plug 'AndrewRadev/splitjoin.vim'
Plug 'chrisbra/csv.vim'
Plug 'cohama/lexima.vim'
Plug 'godlygeek/tabular'
" Requires fzf and optinally rg for :Rg and bat for syntax highlighting in
" previews
Plug 'junegunn/fzf.vim'
Plug 'mg979/vim-visual-multi'
Plug 'mhinz/vim-sayonara', { 'on': 'Sayonara' }
" Requires the tree-sitter CLI for automatically installing new parsers
Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}
Plug 'nvim-treesitter/nvim-treesitter-context'
Plug 'plasticboy/vim-markdown'
Plug 'psliwka/vim-smoothie'
Plug 'rhysd/clever-f.vim'
Plug 'rhysd/committia.vim'
Plug 'shime/vim-livedown'  " Requires livedown
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-fugitive'  " Requires git
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-sleuth'
Plug 'tpope/vim-surround'

" -- Slightly slower plugins (3ms -- 50ms)
Plug 'junegunn/vim-journal'  " ~10ms for journal files
Plug 'neovim/nvim-lspconfig'  " ~20ms, doesn't work properly on demand
" I have better alternative plugins for the following languages
let g:polyglot_disabled = [
    \ 'csv',
    \ 'latex',
    \ 'markdown',
    \ 'python',
    \ 'python-compiler',
    \ 'rust',
    \ 'typescript',
    \ ]
Plug 'sheerun/vim-polyglot'  " 35-50ms, depending on filetype

" -- Slow plugins (> 50ms)
" I don't write enough LaTeX to optimise this. Requires a LaTeX compilation
" backend.
Plug 'lervag/vimtex'  " ~80ms for (La)TeX

" -- Not profiled
Plug 'j-hui/fidget.nvim', { 'tag': 'legacy' }
Plug 'folke/trouble.nvim'  " Optionally requires nvim-web-devicons
" For trouble.nvim & bufferline.nvim. Requires a Nerd Font.
Plug 'nvim-tree/nvim-web-devicons'
Plug 'simrat39/rust-tools.nvim'
Plug 'nvim-lua/plenary.nvim'  " For telescope.nvim & none-ls.nvim
Plug 'nvim-telescope/telescope.nvim'
Plug 'nvim-telescope/telescope-ui-select.nvim'
Plug 'nvimtools/none-ls.nvim'
Plug 'gbprod/none-ls-shellcheck.nvim'
Plug 'folke/which-key.nvim'
Plug 'nvim-lualine/lualine.nvim'
Plug 'RRethy/nvim-base16'  " For lualine theme
Plug 'akinsho/bufferline.nvim', { 'tag': 'v4.*' }
Plug 'ActivityWatch/aw-watcher-vim'

" -- Debugging / DAP (also not profiled yet)
Plug 'mfussenegger/nvim-dap'
Plug 'nvim-neotest/nvim-nio'  " For nvim-dap-ui
Plug 'rcarriga/nvim-dap-ui'  " Requires nvim-nio & nvim-dap
Plug 'mfussenegger/nvim-dap-python' " Requires nvim-dap and debugpy (external)
Plug 'LiadOz/nvim-dap-repl-highlights'
Plug 'rcarriga/cmp-dap'
Plug 'theHamsta/nvim-dap-virtual-text'
Plug 'jbyuki/one-small-step-for-vimkind'  " Lua DAP adapter; requires nvim-dap

" -- Completion and snippets (also not profiled yet)
Plug 'hrsh7th/cmp-buffer'
Plug 'hrsh7th/cmp-cmdline'
Plug 'hrsh7th/cmp-nvim-lsp'
Plug 'hrsh7th/cmp-path'
Plug 'hrsh7th/cmp-vsnip'
Plug 'hrsh7th/nvim-cmp'
Plug 'hrsh7th/vim-vsnip'
Plug 'onsails/lspkind.nvim'
Plug 'rafamadriz/friendly-snippets'
Plug 'tamago324/cmp-zsh'

" -- File tree
Plug 'nvim-neo-tree/neo-tree.nvim', { 'branch': 'v3.x' }  " requires plenary, nvim-web-devicons and nui
Plug 'MunifTanjim/nui.nvim'
" Update LSP servers after file tree modification
Plug 'antosha417/nvim-lsp-file-operations'  " requires plenary

call plug#end()

" }}}
" Colorscheme settings {{{

colorscheme base16-mocha
lua << EOF
    local base16 = require('base16-colorscheme')
    -- Highlight line number to make them visually distinct from code
    base16.highlight.LineNr = { guibg = base16.colors.base01 }
EOF

" }}}
" General settings {{{

let g:mapleader = "\<Space>"
let g:maplocalleader = "\<Space>"

" Persistent undo (slow for large files, so turned off with autocommand)
set undofile
" Line numbering
set number
" Show vertical column at whatever textwidth is set to
set colorcolumn=+0
" Highlight current line
set cursorline
" Ignore case when searching except if search has uppercase letters
set ignorecase smartcase
" Turn off built-in completion; we use nvim-cmp for that.
set complete=
" Always use a menu for autocompletion and don't insert text or select a match
" without user interaction. The preview window configured by nvim-cmp.
set completeopt=menuone,noinsert,noselect,fuzzy
" Ignore files in autocompletion
set wildignore=*.o,*.obj,*.pyc,*.class,*.orig,*/.git/*
" Maximum height of the autocompletion popup menu (pum)
set pumheight=8
" Mouse interactivity, but no context menu on right click
set mouse=a
noremap  <RightMouse>   <Nop>
noremap  <2-RightMouse> <Nop>
noremap  <3-RightMouse> <Nop>
noremap  <4-RightMouse> <Nop>
inoremap <RightMouse>   <Nop>
inoremap <2-RightMouse> <Nop>
inoremap <3-RightMouse> <Nop>
inoremap <4-RightMouse> <Nop>
" Copy/paste with X11 clipboard
set clipboard=unnamedplus
" Start scrolling earlier
set scrolloff=6
set sidescrolloff=6
" Make the new window below or right of the current one
set splitbelow
set splitright
" Check modelines (like the one at the bottom of this file)
set modelines=2
" Disable showing the current mode because lualine already shows it
set noshowmode
" Wait less than a second for mapped seauence to complete
set timeoutlen=500
" Skip intro, skip autocompletion output and use more abbreviations
set shortmess+=Ica
" Allow non-existing blocks in Visual block mode
set virtualedit=block
" Show normally invisible characters (trailing whitespace, tabs, nbsp)
set list
" Ignore whitespace in diff mode & try to align lines within a chunk
set diffopt+=iwhite
" Set window title
set title
" Use conceal (and don't waste space like level 1)
set conceallevel=2
" Let wrapped lines keep indentation, but show a ">>" to mark them
set breakindent
set showbreak=>>
" Show substitution feedback <3
set inccommand=split
" Indicate that the line extends beyond the view in nowrap mode
set listchars+=extends:>,precedes:<
" Use rounded borders for pop-ups
set winborder=rounded
" Ask for confirmation instead of failing when e.g. exiting
set confirm
" Wrap long lines at word boundaries
set linebreak
" More undo-ability
set undolevels=10000
" Syntax highlight Lua in Vim scripts
let g:vimsyn_embed = 'l'
" Supposedly makes startup faster
let g:python3_host_prog = '/usr/bin/python3'
" Disable netrw completely. It's not behaving.
let g:loaded_netrw = 1
let g:loaded_netrwPlugin = 1

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

augroup indentation
    autocmd!

    au FileType css,scss setlocal shiftwidth=2 softtabstop=2
    au FileType haskell setlocal shiftwidth=4 softtabstop=4
    au FileType javascript setlocal shiftwidth=2 softtabstop=2 textwidth=120
    au FileType lua setlocal shiftwidth=2 softtabstop=2
    au FileType prolog setlocal shiftwidth=4 softtabstop=4
    au FileType python setlocal nocindent noshiftround textwidth=79
    au FileType tex setlocal shiftwidth=2 softtabstop=2 textwidth=80
    au FileType vim setlocal shiftwidth=4 softtabstop=4
    au FileType xml,html,xhtml,htmldjango,eruby,xslt
        \ setlocal tabstop=2 shiftwidth=2 softtabstop=2 textwidth=120
augroup END

" }}}
" Plugin settings {{{

" akinsho/bufferline.nvim {{{

lua << EOF
    require('bufferline').setup({
      options = {
        show_close_icon = false,
        show_buffer_close_icons = false,
        always_show_bufferline = false,
        separator_style = 'slant',
        diagnostics = "nvim_lsp",
      },
    })
EOF

" }}}
" AndrewRadev/splitjoin.vim {{{

" Put closing angle bracket in HTML on a new line
let g:splitjoin_html_attributes_bracket_on_new_line = 1

" }}}
" chrisbra/csv.vim {{{

" Make plugin aware of quoted newlines
let g:csv_nl = 1
" Highlight the current column
let g:csv_highlight_column = 'y'

" }}}
" cohama/lexima.vim {{{

" Don't map <Esc>
let g:lexima_map_escape = ''

" }}}
" folke/which-key.nvim {{{

lua require('which-key').setup({})

" }}}
" nvimtools/none-ls.nvim {{{

lua << EOF
    local null_ls = require('null-ls')

    local function has_stylelint_config(utils)
      return utils.root_has_file({
        ".stylelintrc",
        ".stylelintrc.js",
        ".stylelintrc.json",
        ".stylelintrc.yml",
        ".stylelintrc.yaml",
        "stylelint.config.js",
        "stylelint.config.cjs",
      })
    end

    null_ls.setup({
      sources = {
        -- See https://github.com/nvimtools/none-ls.nvim/blob/main/doc/BUILTINS.md
        --
        -- CSS/Sass/SCSS/Less
        null_ls.builtins.diagnostics.stylelint.with({
          condition = has_stylelint_config,
        }),
        null_ls.builtins.formatting.stylelint.with({
          condition = has_stylelint_config,
        }),
        -- Haskell
        null_ls.builtins.formatting.stylish_haskell,
        -- JS/TS/JSX/TSX/Vue
        null_ls.builtins.formatting.prettier.with({
          filetypes = {
            'javascript',
            'javascriptreact',
            'typescript',
            'typescriptreact',
            'vue'
          },
        }),
        -- Lua
        null_ls.builtins.diagnostics.luacheck,
        -- Shell
        require('none-ls-shellcheck.code_actions'),
        require('none-ls-shellcheck.diagnostics'),
        -- Terraform
        null_ls.builtins.formatting.terraform_fmt,
        -- Vim
        null_ls.builtins.diagnostics.vint,
      },
    })
EOF

" }}}
" lervag/vimtex {{{

let g:vimtex_compiler_method = 'tectonic'
let g:vimtex_fold_enabled = 1

" }}}
" nvim-lualine/lualine.nvim {{{

lua << EOF
    require('lualine').setup({
      options = {
        theme = 'base16',
        component_separators = '│',
      },
      extensions = {
        'fugitive',
        'man',
        'neo-tree',
        'nvim-dap-ui',
        'quickfix',
      },
      sections = {
        lualine_a = { 'mode' },
        lualine_b = {
          {
            'branch',
            icon = '',
            on_click = function() vim.cmd('Git') end,
          },
        },
        lualine_c = {
          { 'filename', path = 1 },
        },
        lualine_x = {
          {
            'diagnostics',
            on_click = function() vim.cmd('Trouble') end,
          },
        },
        lualine_y = {
          'filetype',
          {
            'fileformat',
            fmt = function(format)
              return format == '' and '' or format
            end,
          },
          {
            'encoding',
            fmt = function(encoding)
              return encoding == 'utf-8' and '' or encoding
            end,
          },
        },
        lualine_z = { 'location', 'progress' },
      },
    })
EOF

" }}}
" nvim-neo-tree/neo-tree.nvim {{{

lua << EOF
  require('neo-tree').setup({
    close_if_last_window = true,
    -- Keep cursor on first letter of the filename
    enable_cursor_hijack = false,
    source_selector = {
      show_scrolled_off_parent_node = true,
      winbar = true,
    },
    filesystem = {
      follow_current_file = {
        enabled = true,
      },
    },
    event_handlers = {
      {
        event = 'neo_tree_window_after_open',
        handler = function(args)
          if args.position == 'left' or args.position == 'right' then
            vim.cmd('wincmd =')
          end
        end
      },
    },
    commands = {
      close_parent = function(state)
        local node = state.tree:get_node()
        local parent = state.tree:get_node(node:get_parent_id())
        parent:collapse()
        require('neo-tree.ui.renderer').redraw(state)
        require('neo-tree.ui.renderer').focus_node(state, parent:get_id())
      end
    },
    window = {
      mappings = {
        ['l'] = 'open',
        ['h'] = 'close_parent',
      },
    },
  })
  require('lsp-file-operations').setup()
EOF

" }}}
" nvim-telescope/telescope-ui-select.nvim {{{

" Use telescope UI for vim.ui.select, such as code action prompt
lua require('telescope').load_extension('ui-select')

" }}}
" nvim-treesitter/nvim-treesitter {{{

lua << EOF
    require('nvim-treesitter.configs').setup({
      -- Automatically installs the relevant parser when entering a buffer.
      -- Requires the tree-sitter CLI.
      auto_install = true,
    })
EOF

" }}}
" plasticboy/vim-markdown {{{

" Use my own, better foldtext (uses spaces instead of dots for visual ease)
let g:vim_markdown_override_foldtext = 0
" Show strikethroughs
let g:vim_markdown_strikethrough = 1

" }}}
" rhysd/clever-f.vim {{{

let g:clever_f_smart_case = 1

" }}}
" sheerun/vim-polyglot {{{

" Stop vim-vue from slowing down (See https://github.com/posva/vim-vue)
let g:vue_disable_pre_processors = 1

" }}}

" }}}
" General mappings {{{

" jj/fd in insert mode to go to normal mode
inoremap jj <Esc>:w<CR>
inoremap fd <Esc>

" Quick navigation in insert mode
inoremap <C-h> <Left>
inoremap <C-j> <Down>
inoremap <C-k> <Up>
inoremap <C-l> <Right>

" Use backspace to delete character
noremap <BS> X

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
nnoremap <leader>d :Sayonara!<CR>
nnoremap <leader>D :Sayonara<CR>

" Use S to grep (dependent on format of grepprg)
nnoremap S :grep! <C-R><C-W><CR>:cw<CR>
vnoremap S "hy:grep! <C-R>h<CR>:cw<CR>

" LSP bindings
nnoremap gd         <cmd>lua vim.lsp.buf.definition()<CR>
nnoremap gr         <cmd>lua vim.lsp.buf.references()<CR>
nnoremap gR         <cmd>lua vim.lsp.buf.rename()<CR>
nnoremap ga         <cmd>lua vim.lsp.buf.code_action()<CR>
nnoremap <leader>=  <cmd>lua vim.lsp.buf.format({ async = true })<CR>
vnoremap <leader>=  <cmd>lua vim.lsp.buf.format({ async = true })<CR>
" Other useful functionality for future reference: declaration(),
" implementation(), type_definition(),
" workspace_symbol(), vim.lsp.codelens.

" Diagnostic management
nnoremap <leader>en <cmd>lua vim.diagnostic.goto_next()<CR>
nnoremap <leader>ep <cmd>lua vim.diagnostic.goto_prev()<CR>
nnoremap <leader>ev <cmd>lua =vim.lsp.get_active_clients()<CR>
nnoremap <leader>el <cmd>TroubleToggle document_diagnostics<CR>
nnoremap <leader>eL <cmd>TroubleToggle workspace_diagnostics<CR>

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

" }}}
" Plugin mappings {{{

" AndrewRadev/splitjoin.vim {{{

"   gS  Split a one-liner into multiple lines
"   gJ  (with the cursor on the first line of a block) to join a block into a
"       single-line statement.

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
" junegunn/fzf {{{

"   <leader>o         Fzf file in current working directory
nnoremap <leader>o :Files<CR>
"   <leader>O         Fzf recently opened file
nnoremap <leader>O :History<CR>
"   <leader>gr        Fzf text (with selection in visual mode)
nnoremap <leader>gr :Rg<CR>
vnoremap <leader>gr "hy:Rg <C-R>h<CR>
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

" For :Files and :History, use enter key, CTRL-T, CTRL-X or CTRL-V to open
" selected files in the current window, in new tabs, in horizontal splits, or
" in vertical splits respectively.
" Adding the bang puts fzf across the whole window.

" }}}
" junegunn/vim-journal {{{

"   [[/]]         Jump to previous/next section (also in visual mode)

augroup journal_mappings
    autocmd!
    " Remove some annoying default mappings
    au FileType journal unmap <buffer> <Esc><CR>
augroup END

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
" nvim-neo-tree/neo-tree.nvim {{{

"   <leader>T  Open file tree
nnoremap <silent> <leader>T :Neotree toggle<CR>

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

" }}}
" LSP setup {{{

lua << EOF
    -- Show initialisation progress
    require('fidget').setup({
      text = {
        spinner = 'dots',
      },
      window = {
        border = 'rounded',
      },
    })

    -- Tell all LSP servers that we can handle snippets
    local cmp_capabilities = require('cmp_nvim_lsp').default_capabilities()
    vim.lsp.config('*', {
      capabilities = cmp_capabilities
    })

    -- Configure Rust language server and extra tools
    require('rust-tools').setup({
      tools = {
        inlay_hints = {
          only_current_line = true,
        },
      },
      server = {
        settings = {
          ["rust-analyzer"] = {
            -- https://github.com/rust-analyzer/rust-analyzer/blob/master/docs/user/generated_config.adoc
            checkOnSave = {
              -- default: `cargo check`
              command = "clippy",
            },
            procMacro = {
              enable = true,
            },
          },
        },
      },
    })

    -- Enable langue servers
    vim.lsp.enable({
      'biome',   -- Biome language server for linting/formatting JS/TS
      'eslint',  -- ESLint language server for linting JS/TS
      'jsonls',  -- JSON language server
      'lua_ls',  -- Lua language server
      'pyright', -- Python language server
      'ruff',    -- Python linter & formatter
      'ts_ls',   -- TypeScript language server
    })

    -- Configure diagnostics
    require('trouble').setup({
      padding = false,
      indent_lines = false,
      auto_close = true,
    })
    -- Reduce LSP diagnostic noise from virtual text and signs
    vim.diagnostic.config({
      signs = false,
      float = {
        focusable = false,
        header = '',
      },
    })
EOF

" }}}
" Completion setup {{{

lua << EOF
  local has_words_before = function()
    local row, col = unpack(vim.api.nvim_win_get_cursor(0))
    if col == 0 then
      return false
    end
    local last_char = vim.api.nvim_buf_get_text(0, row - 1, col - 1, row - 1, col, {})[1]
    return last_char:match('%s') == nil
  end

  local feedkey = function(key_string)
    local key = vim.api.nvim_replace_termcodes(key_string, true, true, true)
    vim.api.nvim_feedkeys(key, '', false)
  end

  local cmp = require('cmp')
  local cmp_dap = require('cmp_dap')

  cmp.setup({
    -- Overridden so we _do_ get completion for the DAP REPL
    enabled = function()
      return not (  -- always, except:
        -- when we're recording/executing a macro
        vim.fn.reg_recording() ~= '' or vim.fn.reg_executing() ~= ''
        -- when we're in a prompt buffer (but a DAP prompt buffer is fine!)
        or (
          vim.api.nvim_get_option_value('buftype', { buf = 0 }) == 'prompt'
          and not cmp_dap.is_dap_buffer()
        )
      )
    end,
    snippet = {
      expand = function(args)
        vim.fn["vsnip#anonymous"](args.body)
      end,
    },
    window = {
      documentation = cmp.config.window.bordered(),
    },
    mapping = {
       -- Emulate something like ye ole SuperTab
       ['<Tab>'] = cmp.mapping(function(fallback)
         if cmp.visible() then
           cmp.select_next_item()
         elseif vim.fn['vsnip#available'](1) == 1 then
           feedkey('<Plug>(vsnip-expand-or-jump)')
         elseif has_words_before() then
           cmp.complete()
         else
           fallback()
         end
       end, { 'i', 's', 'c' }),
       ['<S-Tab>'] = cmp.mapping(function(fallback)
         if cmp.visible() then
           cmp.select_prev_item()
         elseif vim.fn['vsnip#jumpable'](-1) == 1 then
           feedkey('<Plug>(vsnip-jump-prev)')
         else
           fallback()
         end
       end, { 'i', 's', 'c' }),
       ['<C-n>'] = cmp.mapping(cmp.mapping.select_next_item(), { 'i', 'c' }),
       ['<C-p>'] = cmp.mapping(cmp.mapping.select_prev_item(), { 'i', 'c' }),
       ['<CR>'] = cmp.mapping.confirm({ select = false }),
       ['<C-c>'] = cmp.mapping(cmp.mapping.abort(), { 'i', 'c' }),
    },
    sources = cmp.config.sources({
      { name = 'nvim_lsp' },
      { name = 'vsnip' },
      { name = 'path' },
      { name = 'zsh' },
    }),
    formatting = {
      format = require('lspkind').cmp_format({
        mode = 'symbol',
      }),
    },
  })

  cmp.setup.cmdline({ '/', '?' }, {
    sources = {
      { name = 'buffer' },
    },
  })

  cmp.setup.cmdline(':', {
    sources = cmp.config.sources({
      { name = 'path' },
    }, {
      { name = 'cmdline' },
    })
  })

  cmp.setup.filetype({ 'dap-repl', 'dapui_watches' }, {
    sources = {
      { name = 'dap' },
    },
  })
EOF

" }}}
" Debugging / DAP setup {{{

" Commands include:
"     :DapToggleBreakpoint
"     :DapToggleRepl
"     :DapContinue
"     :DapStep{Into,Out,Over}
"     :DapTerminate
"     :DapShowLog
"     :DapLoadLaunchJSON
"     :DapVirtualText{Enable,Disable,Toggle}

lua << EOF
  local dap = require('dap')
  local dapui = require('dapui')

  dapui.setup({
    layouts = {
      {
        position = 'right',
        size = 0.33,
        elements = {
          { id = 'breakpoints', size = 0.15 },
          { id = 'stacks', size = 0.30 },
          { id = 'scopes', size = 0.45 },
          { id = 'watches', size = 0.1 },
        },
      },
      {
        position = 'bottom',
        size = 0.25,
        elements = {
          'repl',
          'console',
        },
      },
    },
  })
  require('dap-python').setup()
  require('nvim-dap-repl-highlights').setup()
  require('nvim-dap-virtual-text').setup()

  -- Neovim Lua debugging
  dap.configurations.lua = {
    {
      type = 'nlua',
      request = 'attach',
      name = 'Attach to running Neovim instance',
    }
  }
  dap.adapters.nlua = function(callback, config)
    callback({
      type = 'server',
      host = config.host or '127.0.0.1',
      port = config.port or 8086,
    })
  end

  -- Breakpoint sign styling
  vim.fn.sign_define("DapBreakpoint", {
    text = "",
  })
  vim.fn.sign_define("DapBreakpointCondition", {
    text = "",
  })
  vim.fn.sign_define("DapLogPoint", {
    text = "󰛿",
  })
  vim.fn.sign_define("DapBreakpointRejected", {
    text = "",
    texthl = "DiagnosticSignError",
  })
  vim.fn.sign_define("DapStopped", {
    text = "",
    texthl = "DiagnosticSignInfo",
    linehl = "Visual",
    numhl = "Visual",
  })

  -- Global mapping: breakpoint management
  vim.keymap.set('n', '<leader>bb', dap.toggle_breakpoint)
  vim.keymap.set('n', '<leader>bc', dap.clear_breakpoints)
  vim.keymap.set('n', '<leader>bL', dap.run_to_cursor)
  vim.keymap.set('n', '<leader>bC', function()
    vim.ui.input({ prompt = 'Condition: ' }, function(condition)
      if condition then dap.set_breakpoint(condition) end
    end)
  end)
  vim.keymap.set('n', '<leader>bl', function()
    vim.ui.input({ prompt = 'Log message: ' }, function(message)
      if message then dap.set_breakpoint(nil, nil, message) end
    end)
  end)
  -- Global mapping: starting/stopping debugging mode
  vim.keymap.set('n', '<leader>b<CR>', dap.continue)
  vim.keymap.set('n', '<leader>bq', dap.disconnect)

  -- Toggle DAP UI based on execution
  dap.listeners.after.event_initialized['dapui_config'] = dapui.open
  dap.listeners.before.event_terminated['dapui_config'] = dapui.close

  -- Local mapping in debug mode
  vim.api.nvim_create_autocmd('FileType', {
    pattern = {
      'dap-repl',
      'dapui_breakpoints',
      'dapui_console',
      'dapui_scopes',
      'dapui_stacks',
      'dapui_watches',
    },
    callback = function(args)
      vim.keymap.set('n', 'c', dap.continue, { buffer = args.buf })
      vim.keymap.set('n', 'n', dap.step_over, { buffer = args.buf })
      vim.keymap.set('n', 's', dap.step_into, { buffer = args.buf })
      vim.keymap.set('n', 'r', dap.step_out, { buffer = args.buf })
      vim.keymap.set('n', 'q', dap.terminate, { buffer = args.buf })
      vim.keymap.set('n', 'u', dap.up, { buffer = args.buf })
      vim.keymap.set('n', 'd', dap.down, { buffer = args.buf })
      vim.keymap.set('n', 'b', dap.step_back, { buffer = args.buf })
    end
  })
EOF

" }}}
" Misc autocommands {{{

augroup misc
    autocmd!

    " Recognise binary(-like) filetypes mainly to speed up boot time
    au BufReadPre *.bin,*.csv,*.dat,*.db,*.db-journal,*.hex,*.log,*.sqlite,*.sqlite3,*.wasm
        \ setlocal binary nofoldenable foldmethod=manual

    " Unset some features when dealing with large files to speed up boot time
    au BufReadPre * call s:UnsetFeaturesForLargeFiles(10 * 1024 * 1024)

    " Recognize some file extensions
    "   Highlight todo files and other text files with vim-journal
    "   (and override detection of Markdown and text filetypes)
    au BufNewFile,BufRead *.txt,todo setlocal filetype=journal

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

    " Run LSP formatter on save for a few file types
    au BufWritePre *.hs,*.rs,*.tf lua vim.lsp.buf.format()

    " Remove trailing whitespace automatically on write when desired (non-binary)
    au BufWritePre * call s:StripTrailingWhitespace()

    " Reload init.vim on save
    au BufWritePost */nvim/init.vim source %

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
    setlocal nolinebreak
endfunction

func! s:AdjustWindowHeight(minheight, maxheight)
    exe max([min([line('$'), a:maxheight]), a:minheight]) . 'wincmd _'
endfunction

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

" }}}

" vim:foldmethod=marker:foldlevel=0
