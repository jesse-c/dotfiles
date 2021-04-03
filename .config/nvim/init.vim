" For a paranoia.
" Normally `:set nocp` is not needed, because it is done automatically
" when .vimrc is found.
if &compatible
  " `:set nocp` has many side effects. Therefore this should be done
  " only when 'compatible' is set.
  set nocompatible
endif

" Plugins ---------------------------------------------------------------------

call plug#begin('~/.vim/plugged')

" Miscellaneous
Plug 'nvim-lua/popup.nvim'
Plug 'nvim-lua/plenary.nvim'

" Theme
Plug 'ayu-theme/ayu-vim'
Plug 'bluz71/vim-moonfly-colors'
" Plug 'sainnhe/edge'

" VCS
Plug 'tpope/vim-fugitive'
Plug 'mhinz/vim-signify'
Plug 'APZelos/blamer.nvim'

" LSP
Plug 'neovim/nvim-lspconfig'
Plug 'glepnir/lspsaga.nvim', { 'branch': 'main' }
Plug 'kosayoda/nvim-lightbulb'
Plug 'liuchengxu/vista.vim'
Plug 'onsails/lspkind-nvim'

" UI
Plug 'nvim-telescope/telescope.nvim'
Plug 'kyazdani42/nvim-web-devicons'
Plug 'tommcdo/vim-lion'
Plug 'mbbill/undotree'
Plug 'luochen1990/rainbow'
Plug 'google/vim-searchindex'
Plug 'troydm/zoomwintab.vim'
Plug 'hoob3rt/lualine.nvim'
Plug 'chrisbra/Colorizer'
Plug 'mhinz/vim-startify'
Plug 'Yggdroot/indentLine'
Plug 'psliwka/vim-smoothie'
Plug 'tpope/vim-abolish'
Plug 'easymotion/vim-easymotion'
Plug 'norcalli/nvim-colorizer.lua'
" Marks
Plug 'kshenoy/vim-signature'
" Registers
Plug 'junegunn/vim-peekaboo'
" Plug 'gennaro-tedesco/nvim-peekup'

" Session
Plug 'tpope/vim-obsession'

" Buffers
Plug 'jeetsukumaran/vim-buffergator'

" Comments
Plug 'b3nj5m1n/kommentary', { 'branch': 'main' }

" Completion
Plug 'hrsh7th/nvim-compe'

" File system
Plug 'tpope/vim-eunuch'
Plug 'airblade/vim-rooter'
Plug 'francoiscabrol/ranger.vim'
Plug 'ms-jpq/chadtree', {'branch': 'chad', 'do': 'python3 -m chadtree deps'}

" Tree-sitter
Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}  " We recommend updating the parsers on update

" Development
Plug 'sheerun/vim-polyglot'

" Snippets
Plug 'hrsh7th/vim-vsnip'
Plug 'hrsh7th/vim-vsnip-integ'

" Formatting
Plug 'sbdchd/neoformat'

" Languages
" Clojure
Plug 'eraserhd/parinfer-rust', {'do': 'cargo build --release'}
" Plug 'liquidz/vim-iced', {'for': 'clojure'}

call plug#end()

" UI ---------------------------------------------------------------------------

" Scrolling
let g:smoothie_enabled = 0

" Theme
set termguicolors " enable true colors support
" set background=dark
" let ayucolor="dark"
" colorscheme ayu
" let g:edge_style = 'dark'
" colorscheme edge
colorscheme moonfly

" Colours
lua require'colorizer'.setup()

" Current line
set ruler
set cul

" Syntax
syntax on

" Line numbers
set number relativenumber
augroup numbertoggle
  autocmd!
  autocmd BufEnter,FocusGained,InsertLeave * set relativenumber
  autocmd BufLeave,FocusLost,InsertEnter   * set norelativenumber
augroup END

" lualine
lua require'lualine'.setup()

set showtabline=2
set guioptions-=e
set laststatus=2

" Finder
" nmap <Leader>g :Tags<CR>
nmap <Leader>g :Telescope tags<CR>
nmap <Leader>h :Telescope buffers<CR>
nmap <Leader>j :Telescope find_files<CR>
" nmap <Leader>j :call Fzf_dev()<CR><Space>
nmap <Leader>k :Telescope marks<CR>
nmap <Leader>; :Telescope grep_string<CR>
nmap <Leader>' :Telescope command_history<CR>

" Tab
set title

" Structure
set colorcolumn=80,90,120
set showtabline=2
set autoindent
set smartindent
set signcolumn=yes

" Whitespace
set tabstop=2
set softtabstop=2
set shiftwidth=2
set smarttab
set expandtab

" Files
set autochdir

" Splits/Windows/Buffers
set splitbelow
set splitright
" Close a buffer but keep the window open
command! Bd enew\|bd \#

set scrolloff=4

nmap <silent> <C-k> :wincmd k<CR>
nmap <silent> <C-j> :wincmd j<CR>
nmap <silent> <C-h> :wincmd h<CR>
nmap <silent> <C-l> :wincmd l<CR>
nmap <silent> <C-q> :q<CR>

" Quickfix list
nnoremap <leader>l <cmd>call setqflist([])<cr>

" Formatting
nnoremap <leader>d <cmd>Neoformat<cr>

augroup fmt
  autocmd!
  autocmd BufWritePre * undojoin | Neoformat
augroup END

" Miscellaneous
let g:rainbow_active = 1

let g:indentLine_enabled = 1
" let g:indentLine_showFirstIndentLevel = 1
let g:indentLine_setColors = 0
"let g:indentLine_char = '┆'

set noerrorbells
set novisualbell

if has('gui')
  set guioptions-=e
endif

if has('mouse')
  set mouse=a
endif

" Search ----------------------------------------------------------------------

set showmatch
set hlsearch
nnoremap <esc> :noh<return><esc>
set ignorecase
set smartcase

" Completion ------------------------------------------------------------------

set completeopt=menuone,noselect

let g:compe = {}
let g:compe.enabled = v:true
let g:compe.autocomplete = v:true
let g:compe.debug = v:false
let g:compe.min_length = 1
let g:compe.preselect = 'enable'
let g:compe.throttle_time = 80
let g:compe.source_timeout = 200
let g:compe.incomplete_delay = 400
let g:compe.max_abbr_width = 100
let g:compe.max_kind_width = 100
let g:compe.max_menu_width = 100
let g:compe.documentation = v:true

let g:compe.source = {}
let g:compe.source.path = v:true
let g:compe.source.buffer = v:true
let g:compe.source.calc = v:true
let g:compe.source.vsnip = v:true
let g:compe.source.nvim_lsp = v:true
let g:compe.source.nvim_lua = v:true
let g:compe.source.spell = v:true
let g:compe.source.tags = v:true
let g:compe.source.snippets_nvim = v:true
let g:compe.source.treesitter = v:true

" Elixir
" https://github.com/florinpatrascu/vscode-elixir-snippets

lua << EOF 

local t = function(str)
  return vim.api.nvim_replace_termcodes(str, true, true, true)
end

-- Use (s-)tab to:
--- move to prev/next item in completion menuone
--- jump to prev/next snippet's placeholder
_G.tab_complete = function()
  if vim.fn.pumvisible() == 1 then
    return t "<C-n>"
  elseif vim.fn.call("vsnip#available", {1}) == 1 then
    return t "<Plug>(vsnip-expand-or-jump)"
  else
    return t "<Tab>"
  end
end
_G.s_tab_complete = function()
  if vim.fn.pumvisible() == 1 then
    return t "<C-p>"
  elseif vim.fn.call("vsnip#jumpable", {-1}) == 1 then
    return t "<Plug>(vsnip-jump-prev)"
  else
    return t "<S-Tab>"
  end
end

vim.api.nvim_set_keymap("i", "<Tab>", "v:lua.tab_complete()", {expr = true})
vim.api.nvim_set_keymap("s", "<Tab>", "v:lua.tab_complete()", {expr = true})
vim.api.nvim_set_keymap("i", "<S-Tab>", "v:lua.s_tab_complete()", {expr = true})
vim.api.nvim_set_keymap("s", "<S-Tab>", "v:lua.s_tab_complete()", {expr = true})

EOF 

inoremap <silent><expr> <C-Space> compe#complete()
inoremap <silent><expr> <CR>      compe#confirm('<CR>')
inoremap <silent><expr> <C-e>     compe#close('<C-e>')

" Tree-sitter -----------------------------------------------------------------

lua <<EOF
require'nvim-treesitter.configs'.setup {
  highlight = {
    enable = true
  },
}
EOF

" LSP -------------------------------------------------------------------------

lua << EOF 

local lspconfig = require 'lspconfig'
lspconfig.gopls.setup{}
lspconfig.clojure_lsp.setup{}
lspconfig.sourcekit.setup{}
lspconfig.elixirls.setup{
  cmd = { '/Users/jesse/src/github.com/elixir-lsp/elixir-ls/rel/language_server.sh' };
}

local saga = require 'lspsaga'
saga.init_lsp_saga()

vim.cmd [[autocmd CursorHold,CursorHoldI * lua require'nvim-lightbulb'.update_lightbulb()]]

require('lspkind').init({
    -- with_text = true,
    -- symbol_map = {
    --   Text = '',
    --   Method = 'ƒ',
    --   Function = '',
    --   Constructor = '',
    --   Variable = '',
    --   Class = '',
    --   Interface = 'ﰮ',
    --   Module = '',
    --   Property = '',
    --   Unit = '',
    --   Value = '',
    --   Enum = '了',
    --   Keyword = '',
    --   Snippet = '﬌',
    --   Color = '',
    --   File = '',
    --   Folder = '',
    --   EnumMember = '',
    --   Constant = '',
    --   Struct = ''
    -- },
})

require('nvim-treesitter.configs').setup {
  ensure_installed = "maintained", -- one of "all", "maintained" (parsers with maintainers), or a list of languages
  highlight = {
    enable = true               -- false will disable the whole extension
  },
}

EOF 

" https://neovim.io/doc/user/lsp.html

" Go to definition
nmap <silent> gd <cmd>lua vim.lsp.buf.definition()<CR>

" Hover doc
" nmap <silent> K  <cmd>lua vim.lsp.buf.hover()<CR>
" Using Lspsaga
nnoremap <silent> K <cmd>lua require('lspsaga.hover').render_hover_doc()<CR>
" scroll down hover doc
nnoremap <silent> <C-f> <cmd>lua require('lspsaga.hover').smart_scroll_hover(1)<CR>
" scroll up hover doc
nnoremap <silent> <C-b> <cmd>lua require('lspsaga.hover').smart_scroll_hover(-1)<CR>

" Code action
nnoremap <silent><leader>ca <cmd>lua require('lspsaga.codeaction').code_action()<CR>
vnoremap <silent><leader>ca <cmd>'<,'>lua require('lspsaga.codeaction').range_code_action()<CR>

" Signature help
nnoremap <silent> gs <cmd>lua require('lspsaga.signaturehelp').signature_help()<CR>

" Rename
nnoremap <silent>gr <cmd>lua require('lspsaga.rename').rename()<CR>

" Preview definition
nnoremap <silent> ge <cmd>lua require'lspsaga.provider'.preview_definition()<CR>

" Jump Diagnostic and Show Diagnostics

"" Show
nnoremap <silent> <leader>cd <cmd>lua require'lspsaga.diagnostic'.show_line_diagnostics()<CR>
nnoremap <silent> <leader>cd :Lspsaga show_line_diagnostics<CR>

"" Jump diagnostic
nnoremap <silent> [e <cmd>lua require'lspsaga.diagnostic'.lsp_jump_diagnostic_prev()<CR>
nnoremap <silent> ]e <cmd>lua require'lspsaga.diagnostic'.lsp_jump_diagnostic_next()<CR>

" Float terminal
" Float terminal also you can pass the cli command in open_float_terminal function
" or open_float_terminal('lazygit')<CR>
nnoremap <silent> <A-d> <cmd>lua require('lspsaga.floaterm').open_float_terminal()<CR>
nnoremap <silent> <A-d> <C-\><C-n>:lua require('lspsaga.floaterm').close_float_terminal()<CR>

" Languages -------------------------------------------------------------------

" let g:iced_enable_default_key_mappings = v:true

" VCS -------------------------------------------------------------------------

let g:blamer_enabled = 1

" Copy/Paste ------------------------------------------------------------------

set pastetoggle=<F10>
set nopaste
set clipboard=unnamed

" File system -----------------------------------------------------------------

set hidden
set nobackup
set nowritebackup
set noswapfile
filetype plugin on
nnoremap <leader>v <cmd>CHADopen<cr>

" Undo ------------------------------------------------------------------------

if has("persistent_undo")
  set undodir=~/.undo/
  set undofile
endif

" History ----------------------------------------------------------------------

set undolevels=10000

" Editor -----------------------------------------------------------------------

set updatetime=300
" don't give |ins-completion-menu| messages.
set shortmess+=c

let g:python_host_prog='/Users/jesse/.asdf/shims/python2'
let g:python3_host_prog='/Users/jesse/.asdf/shims/python3'
nnoremap <F5> :UndotreeToggle<cr>

nmap <F8> :Vista!!<CR>
let g:vista#renderer#enable_icon = 1

" Local per machine settings ---------------------------------------------------

try
  source ~/.config/nvim/local.vim
catch
  " No such file? No problem; just ignore it.
endtry

" Miscellaneous ---------------------------------------------------------------

nmap <F3> i<C-R>=strftime("%Y-%m-%d")<CR><Esc>
imap <F3> <C-R>=strftime("%Y-%m-%d")<CR>

nnoremap <leader>ev :vsplit $MYVIMRC<cr> " Edit my vimrc
nnoremap <leader>sv :source $MYVIMRC<cr> " Source my vimrc

" https://vim.fandom.com/wiki/Copy_filename_to_clipboard
nmap <leader>cs :let @*=expand("%")<CR>
nmap <leader>cl :let @*=expand("%:p")<CR>

set encoding=UTF-8
set inccommand=nosplit

" Fix some common typos --------------------------------------------------------
cnoreabbrev W! w!
cnoreabbrev Q! q!
cnoreabbrev Qall! qall!
cnoreabbrev Wq wq
cnoreabbrev Wa wa
cnoreabbrev wQ wq
cnoreabbrev W w
cnoreabbrev Q q
cnoreabbrev Sp sp
cnoreabbrev sP sp
cnoreabbrev Qall qall
cnoreabbrev FOrmat Format

" Tips -------------------------------------------------------------------------
" \t tab listing
" \b buffer listing
" \f ranger
" \ leader
" <C-w>o zoom tab
