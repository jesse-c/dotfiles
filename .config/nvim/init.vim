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
Plug 'rbong/vim-crystalline'
Plug 'chrisbra/Colorizer'
Plug 'mhinz/vim-startify'
Plug 'Yggdroot/indentLine'
Plug 'psliwka/vim-smoothie'
Plug 'tpope/vim-abolish'
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
let ayucolor="dark"
colorscheme ayu
" let g:edge_style = 'dark'
" colorscheme edge

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

" Tabline / Statusline
function! StatusLine(current, width)
  let l:s = ''

  if a:current
    let l:s .= crystalline#mode() . crystalline#right_mode_sep('')
  else
    let l:s .= '%#CrystallineInactive#'
  endif
  let l:s .= ' %f%h%w%m%r '
  if a:current
    let l:s .= crystalline#right_sep('', 'Fill') . ' %{fugitive#head()}'
  endif

  let l:s .= '%='
  if a:current
    let l:s .= crystalline#left_sep('', 'Fill') . ' %{&paste ?"PASTE ":""}%{&spell?"SPELL ":""}'
    let l:s .= crystalline#left_mode_sep('')
  endif
  if a:width > 80
    let l:s .= ' %{&ft}[%{&fenc!=#""?&fenc:&enc}][%{&ff}] %l/%L %c%V %P '
  else
    let l:s .= ' '
  endif

  return l:s
endfunction

function! TabLine()
  let l:vimlabel = has('nvim') ?  ' NVIM ' : ' VIM '
  return crystalline#bufferline(2, len(l:vimlabel), 1) . '%=%#CrystallineTab# ' . l:vimlabel
endfunction

let g:crystalline_enable_sep = 0
let g:crystalline_statusline_fn = 'StatusLine'
let g:crystalline_tabline_fn = 'TabLine'
let g:crystalline_theme = 'molokai'

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


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

"call plug#begin('~/.vim/plugged')

"" UI -------------------------------------------------------------------------
"" Plug 'chriskempson/tomorrow-theme', { 'rtp': 'vim' }
"Plug 'chriskempson/base16-vim'
"Plug 'ryanoasis/vim-devicons'
"Plug 'ms-jpq/chadtree', {'branch': 'chad', 'do': ':UpdateRemotePlugins'}
"Plug 'editorconfig/editorconfig-vim'
"Plug 'gcmt/taboo.vim'
"Plug 'ludovicchabant/vim-gutentags'
"Plug 'junegunn/vim-easy-align'

"" Statusbar
"Plug 'itchyny/lightline.vim'

"" Editor ---------------------------------------------------------------------
"Plug 'tpope/vim-surround'

"" Comments
"Plug 'tpope/vim-commentary'

"" Development ----------------------------------------------------------------
"Plug 'sheerun/vim-polyglot'
"Plug 'neoclide/coc.nvim', {'branch': 'release'}
"" Plug 'metakirby5/codi.vim'
"" Plug 'tpope/vim-projectionist'

"" Go
"Plug 'buoto/gotests-vim'
"Plug 'sebdah/vim-delve'

"" Elm
"Plug 'andys8/vim-elm-syntax'

"" Clojure
"Plug 'eraserhd/parinfer-rust', {'do': 'cargo build --release'}
"" Plug 'tpope/vim-fireplace'
"" Plug 'tpope/vim-salve'
"" Plug 'tpope/vim-dispatch'
"" Plug 'guns/vim-sexp',    {'for': 'clojure'}
"" Plug 'liquidz/vim-iced', {'for': 'clojure'}
"" Plug 'liquidz/vim-iced-coc-source', {'for': 'clojure'}

"" VCS ------------------------------------------------------------------------
"Plug 'rhysd/git-messenger.vim'
"Plug 'jreybert/vimagit'

"" Search ---------------------------------------------------------------------
"" Plug 'junegunn/fzf'
"" Plug 'junegunn/fzf.vim'
"Plug 'liuchengxu/vim-clap', { 'do': ':Clap install-binary!' }

"" Org ------------------------------------------------------------------------
"Plug 'vimwiki/vimwiki'

"call plug#end()


"" Development ------------------------------------------------------------------

"" Run a given vim command on the results of alt from a given path.
"" See usage below.
"function! AltCommand(path, vim_command)
"  let l:alternate = system("alt " . a:path)
"  if empty(l:alternate)
"    echo "No alternate file for " . a:path . " exists!"
"  else
"    exec a:vim_command . " " . l:alternate
"  endif
"endfunction

"" Find the alternate file for the current path and open it
"nnoremap <leader>. :w<cr>:call AltCommand(expand('%'), ':e')<cr>

"" Polyglot
"" let g:polyglot_disabled = ['elm']

"" Elm 
"let g:elm_jump_to_error = 0
"let g:elm_make_show_warnings = 0
"let g:elm_syntastic_show_warnings = 0
"let g:elm_browser_command = ""
"let g:elm_detailed_complete = 0
"let g:elm_format_autosave = 0
"let g:elm_format_fail_silently = 0
"let g:elm_setup_keybindings = 0

"" Go
"let g:go_fmt_command = "goimports"

"" Clojure
"let g:iced_enable_default_key_mappings = v:true

"" LSP --------------------------------------------------------------------------

"" Use <c-space> to trigger completion.
"inoremap <silent><expr> <c-space> coc#refresh()

"" Use <cr> to confirm completion, `<C-g>u` means break undo chain at current position.
"" Coc only does snippet and additional edit on confirm.
"inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"

"" Use tab for trigger completion with characters ahead and navigate.
"" Use command ':verbose imap <tab>' to make sure tab is not mapped by other plugin.
"inoremap <silent><expr> <TAB>
"      \ pumvisible() ? "\<C-n>" :
"      \ <SID>check_back_space() ? "\<TAB>" :
"      \ coc#refresh()
"inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

"function! s:check_back_space() abort
"  let col = col('.') - 1
"  return !col || getline('.')[col - 1]  =~# '\s'
"endfunction

"autocmd User CocNvimInit call echom "CoC initialised"

"nmap <silent> <C-f> <Plug>(coc-diagnostic-prev)
"nmap <silent> <C-g> <Plug>(coc-diagnostic-next)

"" Remap keys for gotos
"nmap <silent> gd <Plug>(coc-definition)
"nmap <silent> gy <Plug>(coc-type-definition)
"nmap <silent> gi <Plug>(coc-implementation)
"nmap <silent> gr <Plug>(coc-references)

"" Use K to show documentation in preview window
"nnoremap <silent> K :call <SID>show_documentation()<CR>

"function! s:show_documentation()
"  if (index(['vim','help'], &filetype) >= 0)
"    execute 'h '.expand('<cword>')
"  else
"    call CocAction('doHover')
"  endif
"endfunction

"nmap <leader>rn <Plug>(coc-rename)

"" nmap <leader>f :call CocAction('format')<cr>

"" Use `:Format` to format current buffer
"command! -nargs=0 Format :call CocAction('format')

"" Using CocList
"" Show all diagnostics
"nnoremap <silent> <space>a  :<C-u>CocList diagnostics<cr>
"" Manage extensions
"nnoremap <silent> <space>e  :<C-u>CocList extensions<cr>
"" Show commands
"nnoremap <silent> <space>c  :<C-u>CocList commands<cr>
"" Find symbol of current document
"nnoremap <silent> <space>o  :<C-u>CocList outline<cr>
"" Search workspace symbols
"nnoremap <silent> <space>s  :<C-u>CocList -I symbols<cr>
"" Do default action for next item.
"nnoremap <silent> <space>j  :<C-u>CocNext<CR>
"" Do default action for previous item.
"nnoremap <silent> <space>k  :<C-u>CocPrev<CR>
"" Resume latest coc list
"nnoremap <silent> <space>p  :<C-u>CocListResume<CR>

"" List yanks
"nnoremap <silent> <space>y  :<C-u>CocList -A --normal yank<cr>

"" Remap for do codeAction of current line
"nmap <leader>ac  <Plug>(coc-codeaction)
"" Fix autofix problem of current line
"nmap <leader>qf  <Plug>(coc-fix-current)

"" Search -----------------------------------------------------------------------

"cnoreabbrev ag Ag
"cnoreabbrev aG Ag
"cnoreabbrev AG Ag

"cnoreabbrev rg rg
"cnoreabbrev rG rg
"cnoreabbrev RG rg

"" vim-clap

"" let g:clap_provider_grep_opts = '-H --no-heading --vimgrep --smart-case --hidden -g "!.git/"'
"" let g:clap_provider_files_opts = '--hidden'

"" nmap <Leader>g :Tags<CR>
"nmap <Leader>g :Clap tags<CR>
"nmap <Leader>h :Clap buffers<CR>
"nmap <Leader>j :Clap files<CR>
"" nmap <Leader>j :call Fzf_dev()<CR><Space>
"nmap <Leader>k :Clap marks<CR>
"nmap <Leader>l :Clap hist:<CR>
"nmap <Leader>; :Clap grep<CR>
"nmap <Leader>' :Clap history<CR>

"" FZF
"" " nmap <Leader>g :Tags<CR>
"" nmap <Leader>g :Vista finder coc<CR>
"" nmap <Leader>h :Buffers<CR>
"" nmap <Leader>j :Files<CR>
"" " nmap <Leader>j :call Fzf_dev()<CR><Space>
"" nmap <Leader>k :Marks<CR>
"" nmap <Leader>l :History<CR>
"" nmap <Leader>; :Rg<Space>
"" nmap <Leader>' :History:<CR>
"let g:vista_fzf_preview = ['right:50%']

"let g:fzf_action = {
"  \ 'ctrl-t': 'tab split',
"  \ 'ctrl-x': 'split',
"  \ 'ctrl-v': 'vsplit' }

"command! -bang -nargs=* Rg
"  \ call fzf#vim#grep(
"  \   'rg --column --line-number --no-heading --color=always --smart-case '.shellescape(<q-args>), 1,
"  \   <bang>0 ? fzf#vim#with_preview('up:60%')
"  \           : fzf#vim#with_preview('right:50%:hidden', '?'),
"  \   <bang>0)

"let $FZF_DEFAULT_COMMAND = 'fd --type f'

"let g:fzf_files_options =
"  \ '--preview "bat --theme="OneHalfDark" --style=numbers,changes --color=always {} | head -'.&lines.'"'

"" https://www.reddit.com/r/vim/comments/9xpb18/file_preview_with_fzf_rg_bat_and_devicons/ 
"if executable('rg')
"  let $FZF_DEFAULT_COMMAND = 'rg --files --hidden --follow --glob "!.git/*"'
"  set grepprg=rg\ --vimgrep
"  command! -bang -nargs=* Find call fzf#vim#grep('rg --column --line-number --no-heading --fixed-strings --ignore-case --hidden --follow --glob "!.git/*" --color "always" '.shellescape(<q-args>).'| tr -d "\017"', 1, <bang>0)
"endif

"" Files + devicons
"function! Fzf_dev()
"  let l:fzf_files_options = '--preview "bat --theme="OneHalfDark" --style=numbers,changes --color always {2..-1} | head -'.&lines.'"'

"  function! s:files()
"    let l:files = split(system($FZF_DEFAULT_COMMAND), '\n')
"    return s:prepend_icon(l:files)
"  endfunction

"  function! s:prepend_icon(candidates)
"    let l:result = []
"    for l:candidate in a:candidates
"      let l:filename = fnamemodify(l:candidate, ':p:t')
"      let l:icon = WebDevIconsGetFileTypeSymbol(l:filename, isdirectory(l:filename))
"      call add(l:result, printf('%s %s', l:icon, l:candidate))
"    endfor

"    return l:result
"  endfunction

"  function! s:edit_file(item)
"    let l:pos = stridx(a:item, ' ')
"    let l:file_path = a:item[pos+1:-1]
"    execute 'silent e' l:file_path
"  endfunction

"  call fzf#run({
"        \ 'source': <sid>files(),
"        \ 'sink':   function('s:edit_file'),
"        \ 'options': '-m ' . l:fzf_files_options,
"        \ 'down':    '40%' })
"endfunction

"" UI ---------------------------------------------------------------------------











"nmap <silent> <C-m> :GitMessenger<CR>

"" Start interactive EasyAlign in visual mode (e.g. vipga)
"xmap ga <Plug>(EasyAlign)

"" Start interactive EasyAlign for a motion/text object (e.g. gaip)
"nmap ga <Plug>(EasyAlign)

"" Tags -------------------------------------------------------------------------

"" Gutentags
"" Store tags file somewhere else than in the project dir
"let g:gutentags_cache_dir = '~/.gutentags'
"" Only create tags for tracked files
"let g:gutentags_file_list_command = 'git ls-files'

"" Vista
"let g:vista_default_executive = 'coc'

"" Lightline --------------------------------------------------------------------

"set noshowmode " it's in the statusline now

"function! MyFiletype()
"  return winwidth(0) > 70 ? (strlen(&filetype) ? &filetype . ' ' . WebDevIconsGetFileTypeSymbol() : 'no ft') : ''
"endfunction

"function! MyFileformat()
"  return winwidth(0) > 70 ? (&fileformat . ' ' . WebDevIconsGetFileFormatSymbol()) : ''
"endfunction

"function! NearestMethodOrFunction() abort
"  return get(b:, 'vista_nearest_method_or_function', '')
"endfunction

"autocmd User GutentagsUpdating call lightline#update()
"autocmd User GutentagsUpdated call lightline#update()

"autocmd User CocStatusChange call lightline#update()
"autocmd User CocDiagnosticChange call lightline#update()

"let g:lightline = {
"      \ 'colorscheme': 'ayu',
"      \ 'active': {
"      \   'left': [ [ 'mode', 'paste' ],
"      \             [ 'gitbranch', 'readonly', 'fp', 'modified' ] ],
"      \   'right': [ [ 'lineinfo' ],
"      \              [ 'percent' ],
"      \              [ 'method', 'cocstatus', 'fileformat', 'fileencoding', 'filetype'] ]
"      \ },
"      \ 'inactive': {
"      \   'left': [ [ 'readonly', 'fp', 'modified', 'gitbranch' ] ],
"      \   'right': [ [ ] ]
"      \ },
"      \ 'component': {
"      \   'fp': '%<%F%<'
"      \ },
"      \ 'component_function': {
"      \   'gitbranch': 'fugitive#head',
"      \   'cocstatus': 'coc#status',
"      \   'filetype': 'MyFiletype',
"      \   'fileformat': 'MyFileformat',
"      \   'method': 'NearestMethodOrFunction'
"      \ },
"      \ }

"let g:lightline.tabline = {
"  \   'left': [ ['tabs'] ],
"  \   'right': [ [ ] ],
"  \ }

"" Colorizer --------------------------------------------------------------------
"let g:colorizer_auto_filetype='css,scss,html,htm,elm'
"let g:colorizer_skip_comments = 1

"" VimiDevIcons -----------------------------------------------------------------
"let g:webdevicons_enable = 1

"" CHADTree ---------------------------------------------------------------------

"set guifont=RobotoMono\ Nerd\ Font:h11 " TODO
""set guifont=*

"" Org --------------------------------------------------------------------------

"" Vimwiki
"let g:vimwiki_folding='expr'
"let g:vimwiki_list = [{'path': '$HOME/Library/Mobile Documents/com~apple~CloudDocs/Documents/vimwiki',
"      \ 'diary_header': 'Plan',
"      \ 'diary_index': 'index',
"      \ 'diary_rel_path': 'plan/'}]

"" Misc. ------------------------------------------------------------------------

"command! FormatJson :%!jq '.'

