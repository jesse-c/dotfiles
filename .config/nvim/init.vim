" PLUGIN MANAGEMENT ============================================================
call plug#begin('~/.vim/plugged')

" File system ------------------------------------------------------------------
Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }
Plug '/usr/local/opt/fzf' | Plug 'junegunn/fzf.vim'
Plug 'francoiscabrol/ranger.vim'
Plug 'tpope/vim-eunuch'

" VCS --------------------------------------------------------------------------
Plug 'tpope/vim-fugitive'
Plug 'mhinz/vim-signify'

" Editor -----------------------------------------------------------------------
" UI
Plug 'jeetsukumaran/vim-buffergator'
Plug 'kshenoy/vim-signature'
Plug 'Yggdroot/indentLine'
Plug 'rbgrouleff/bclose.vim'
Plug 'itchyny/lightline.vim'
Plug 'maximbaz/lightline-ale'
Plug 'mbbill/undotree'
Plug 'chrisbra/Colorizer'
Plug 'editorconfig/editorconfig-vim'
"Plug 'henrik/vim-indexed-search'
Plug 'google/vim-searchindex'
Plug 'ryanoasis/vim-devicons'
Plug 'tiagofumo/vim-nerdtree-syntax-highlight'
Plug 'Xuyuanp/nerdtree-git-plugin'
" Colour schemes
Plug 'ayu-theme/ayu-vim'
" Sessions
Plug 'tpope/vim-obsession'
" Text
Plug 'tpope/vim-surround'

" Workflow
Plug 'vimwiki/vimwiki'
Plug 'mhinz/vim-startify'

" Programming ------------------------------------------------------------------
" Lint
Plug 'w0rp/ale'
" Autocomplete
" Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'Shougo/echodoc.vim'
" Format
"Plug 'sbdchd/neoformat'
Plug 'junegunn/vim-easy-align'
" Documentation
Plug 'rizzatti/dash.vim'
Plug 'majutsushi/tagbar'
Plug 'ludovicchabant/vim-gutentags'
" Comments
Plug 'tpope/vim-commentary'

" Golang
" Plug 'zchee/deoplete-go', {'do': 'make'}
Plug 'fatih/vim-go', { 'do': ':GoInstallBinaries' }
Plug 'buoto/gotests-vim'
Plug 'sebdah/vim-delve'
" JavaScript
Plug 'pangloss/vim-javascript'
Plug 'othree/jspc.vim'
" Plug 'carlitux/deoplete-ternjs', { 'do': 'npm install -g tern' }
" TypeScript
Plug 'Quramy/tsuquyomi'
Plug 'leafgarland/typescript-vim'
" Elm
Plug 'ElmCast/elm-vim'
"Plug 'bitterjug/vim-tagbar-ctags-elm'
" Plug 'pbogut/deoplete-elm'
" Rust
Plug 'rust-lang/rust.vim'
Plug 'racer-rust/vim-racer'
" Plug 'sebastianmarkow/deoplete-rust'
" Racket
Plug 'wlangstroth/vim-racket'
Plug 'MicahElliott/vrod'
" Lisp/Scheme
"Plug 'kovisoft/slimv'
"Plug 'jpalardy/vim-slime'
Plug 'luochen1990/rainbow'
Plug 'ds26gte/scmindent'
" Ruby
" Plug 'Shougo/deoplete-rct'
Plug 'vim-ruby/vim-ruby'
" Neomutt
Plug 'neomutt/neomutt.vim'
" Nginx
Plug 'chr4/nginx.vim'
" Python
" Plug 'zchee/deoplete-jedi'
" Z3
Plug 'bohlender/vim-z3-smt2'
" Scala
Plug 'derekwyatt/vim-scala'

Plug 'roxma/nvim-completion-manager'
if !has('nvim')
    Plug 'roxma/vim-hug-neovim-rpc'
endif
Plug 'roxma/nvim-cm-tern',  {'do': 'npm install'}
Plug 'roxma/ncm-elm-oracle'
Plug 'roxma/nvim-cm-racer'
Plug 'roxma/ncm-clang'
Plug 'roxma/ncm-rct-complete'
Plug 'calebeby/ncm-css'
Plug 'sassanh/nvim-cm-eclim'

call plug#end()

" PLUGIN CONFIGURATION =========================================================

" Lightline --------------------------------------------------------------------
let g:lightline = {
  \   'colorscheme': 'one',
  \   'active': {
  \   'left': [ [ 'mode', 'paste' ],
  \             [ 'readonly', 'fp', 'modified', 'gitbranch' ] ],
  \   'right': [ [ 'lineinfo' ],
  \              [ 'percent' ],
  \              [ 'fileformat', 'fileencoding', 'filetype', 'charvaluehex' ],
  \              [ 'linter_checking', 'linter_errors', 'linter_warnings', 'linter_ok', 'gutentags' ]
  \            ]
  \   },
  \   'inactive': {
  \   'left': [ [ 'readonly', 'fp', 'modified', 'gitbranch' ] ],
  \   'right': [ [ ] ]
  \   },
  \   'component': {
  \     'fp': '%<%F%<'
  \   },
  \   'component_function': {
  \     'gitbranch': 'fugitive#head',
  \     'filetype': 'MyFiletype',
  \     'fileformat': 'MyFileformat',
  \     'gutentags': 'MyGutentagsStatusLineRefresher',
  \   },
  \   'component_expand': {
  \     'linter_checking': 'lightline#ale#checking',
  \     'linter_warnings': 'lightline#ale#warnings',
  \     'linter_errors': 'lightline#ale#errors',
  \     'linter_ok': 'lightline#ale#ok',
  \   }
  \ }

let g:lightline.tabline = {
  \   'left': [ ['tabs'] ],
  \   'right': [ [ ] ],
  \ }

let g:lightline#ale#indicator_checking = "\uf110"
let g:lightline#ale#indicator_warnings = "\uf071"
let g:lightline#ale#indicator_errors = "\uf05e"
let g:lightline#ale#indicator_ok = "\uf00c"

" Colorizer --------------------------------------------------------------------
let g:colorizer_auto_filetype='css,scss,html,htm,elm'
"let g:colorizer_skip_comments = 1

" Neoformat --------------------------------------------------------------------
" augroup fmt
"   autocmd!
"   autocmd BufWritePre * undojoin | Neoformat
" augroup END

" let g:neoformat_javascript_prettier = {
"   \ 'exe': 'prettier',
"   \ 'args': ['--stdin', '--stdin-filepath', '%:p', '--single-quote', '--no-semi', '--trailing-comma all'],
"   \ 'stdin': 1,
" \ }

" let g:neoformat_javascript_prettiereslint = {
"   \ 'exe': 'prettier-eslint',
"   \ 'args': ['--stdin', '--stdin-filepath', '%:p', '--single-quote true', '--no-semi true', '--trailing-comma all'],
"   \ 'stdin': 1,
" \ }
" let g:neoformat_javascript_standard = {
"   \ 'exe': 'standard',
"   \ 'stdin': 1,
" \ }

" let g:neoformat_sql_sqlformat = {
"   \ 'exe': 'sqlformat',
"   \ 'args': ['-'],
"   \ 'stdin': 1,
" \ }

" let g:neoformat_enabled_javascript = ['prettiereslint', 'standard']
" let g:neoformat_enabled_scss = ['prettier']
" let g:neoformat_enabled_go = []
" let g:neoformat_enabled_elm = []
" let g:neoformat_enabled_python = ['yapf', 'isort']
" let g:neoformat_basic_format_trim = 1
" let g:neoformat_basic_format_retab = 1

" FZF --------------------------------------------------------------------------
nmap <Leader>g :Tags<CR>
nmap <Leader>h :Buffers<CR>
nmap <Leader>j :Files<CR>
nmap <Leader>k :Marks<CR>
nmap <Leader>l :History<CR>
nmap <Leader>; :Rg<Space>
cnoreabbrev ag Ag
cnoreabbrev aG Ag
cnoreabbrev AG Ag
command! -bang -nargs=* Rg
  \ call fzf#vim#grep(
  \   'rg --smart-case --column --line-number --no-heading --color=always '.shellescape(<q-args>), 1,
  \   <bang>0 ? fzf#vim#with_preview('up:60%')
  \           : fzf#vim#with_preview('right:50%:hidden', '?'),
  \   <bang>0)

" ALE --------------------------------------------------------------------------
"let g:ale_linters = {'go': ['gometalinter', 'gofmt', 'gobuild'], 'rust': ['cargo']}
"let g:ale_linters = {'go': ['gometalinter', 'gofmt']}
let g:ale_go_gometalinter_options = '--fast --enable=unused --enable=unparam --enable=goimports --enable=golint --enable=gotype --enable=varcheck --enable=megacheck'
"let g:ale_go_gometalinter_executable = '/Users/j/go/bin/gometalinter'
let g:ale_linters = {
  \ 'go': ['gometalinter', 'gofmt', 'gobuild'],
  \ 'javascript': ['standard']
\}
  " 'js': ['eslint'],
  " 'yaml': ['yamllint'],
  " 'docker': ['hadolint'],
  " 'json': ['jsonlint'],
  " 'python': ['flake8'],
  " 'shell': ['shellcheck']
  "
let g:ale_fixers = {
  \ 'javascript': ['prettier_standard'],
  \ 'sass': ['stylelint']
\}
let g:ale_fix_on_save = 1

let g:ale_html_tidy_options = '-q -e -language en --tab-size 2 --wrap 80 --break-before-br no --vertical-space no'

nmap <silent> <C-k> <Plug>(ale_previous_wrap)
nmap <silent> <C-j> <Plug>(ale_next_wrap)

"let g:ale_open_list = 1
"let g:ale_lint_delay = 1000
"let g:ale_sign_error = '⤫'
"let g:ale_sign_warning = '⚠'
let g:airline#extensions#ale#enabled = 1
let g:go_auto_type_info = 1
"let g:go_auto_sameids = 1

function! LinterStatus() abort
    let l:counts = ale#statusline#Count(bufnr(''))

    let l:all_errors = l:counts.error + l:counts.style_error
    let l:all_non_errors = l:counts.total - l:all_errors

    return l:counts.total == 0 ? 'OK' : printf(
    \   '%dW %dE',
    \   all_non_errors,
    \   all_errors
    \)
endfunction

set statusline=%{LinterStatus()}

" Undo -------------------------------------------------------------------------
if has("persistent_undo")
  set undodir=~/.undo/
  set undofile
endif
nnoremap <F5> :UndotreeToggle<cr>

" Deoplete ---------------------------------------------------------------------
" let g:deoplete#enable_at_startup = 1
" set completeopt+=noselect

" golang -----------------------------------------------------------------------
" let g:deoplete#sources#go#gocode_binary = $GOPATH.'/bin/gocode'
" let g:deoplete#sources#go#pointer = 1
" let g:deoplete#sources#go#sort_class = ['package', 'func', 'type', 'var', 'const']
" let g:deoplete#sources#go#use_cache = 1
" let g:deoplete#sources#go#json_directory = '~/.cache/deoplete/go/$GOOS_$GOARCH'
"let g:neomake_go_gometalinter_args = ['--disable-all', '--enable=gosimple', '--enable=unused', '--enable=misspell', '--enable=staticcheck', '--enable=interfacer', '--enable=govet', '--enable=errcheck', '--enable=varcheck', '--enable=golint', '--enable=structcheck', '--enable=aligncheck', '--enable=goconst', '--enable=ineffassign']
map <silent> <c-e> :GoDecls<CR>
let g:go_fmt_command = "goimports"

" Lisp/Scheme ------------------------------------------------------------------
let g:rainbow_active = 1
"let g:slimv_swank_cmd = '!osascript -e "tell application \"Terminal\" to do script \"sbcl --load ~/.vim//plugged/slimv/slime/start-swank.lisp\""'

" Racket
"au BufNewFile,BufRead,BufReadPost *.rkt,*.rktl,*.rktd set filetype=scheme " Use with SLIMV (Swank)

" Elm --------------------------------------------------------------------------
let g:elm_format_autosave = 1
let g:elm_detailed_complete = 1
let g:elm_setup_keybindings = 0
let g:elm_make_show_warnings = 1

" Rust -------------------------------------------------------------------------
let g:rustfmt_autosave = 1
" let g:deoplete#sources#rust#racer_binary='which racer'
" let g:deoplete#sources#rust#rust_source_path='/Users/jesse/.rustup/toolchains/nightly-x86_64-apple-darwin'

" IndentLine
"let g:indentLine_char = ''
"let g:indentLine_first_char = ''
let g:indentLine_showFirstIndentLevel = 1
let g:indentLine_setColors = 0

let g:go_highlight_functions = 1
let g:go_highlight_function_calls = 1
let g:go_highlight_methods = 1
let g:go_highlight_build_constraints = 1

" VimiDevIcons -----------------------------------------------------------------
let g:webdevicons_enable = 1
let g:webdevicons_enable_nerdtree = 1
"let g:webdevicons_conceal_nerdtree_brackets = 1

" NerdTree ---------------------------------------------------------------------
nmap <silent> <c-S-n> :NERDTreeToggle<CR>
let g:NERDTreeShowHidden = 1

set guifont=RobotoMono\ Nerd\ Font:h11
"set guifont=*

" ctags ------------------------------------------------------------------------
"set tags=tags;/

" Gutentags
" Store tags file somewhere else than in the project dir
let g:gutentags_cache_dir = '~/.gutentags'
" Only create tags for tracked files
let g:gutentags_file_list_command = 'git ls-files'
" Explicitly only create tags for elm files. Change to use '.gutctags' file
" per project if working on multiple projects
"let g:gutentags_ctags_extra_args = ['--languages=Elm']

" Lightline --------------------------------------------------------------------
function! MyFiletype()
  return winwidth(0) > 70 ? (strlen(&filetype) ? &filetype . ' ' . WebDevIconsGetFileTypeSymbol() : 'no ft') : ''
endfunction

function! MyFileformat()
  return winwidth(0) > 70 ? (&fileformat . ' ' . WebDevIconsGetFileFormatSymbol()) : ''
endfunction

augroup MyGutentagsStatusLineRefresher
    autocmd!
    autocmd User GutentagsUpdating call lightline#update()
    autocmd User GutentagsUpdated call lightline#update()
augroup END

" Tagbar -----------------------------------------------------------------------
nmap <F8> :TagbarToggle<CR>

let g:tagbar_type_go = {
  \ 'ctagstype' : 'go',
  \ 'kinds'     : [
    \ 'p:package',
    \ 'i:imports:1',
    \ 'c:constants',
    \ 'v:variables',
    \ 't:types',
    \ 'n:interfaces',
    \ 'w:fields',
    \ 'e:embedded',
    \ 'm:methods',
    \ 'r:constructor',
    \ 'f:functions'
  \ ],
  \ 'sro' : '.',
  \ 'kind2scope' : {
    \ 't' : 'ctype',
    \ 'n' : 'ntype'
  \ },
  \ 'scope2kind' : {
    \ 'ctype' : 't',
    \ 'ntype' : 'n'
  \ },
  \ 'ctagsbin'  : 'gotags',
  \ 'ctagsargs' : '-sort -silent'
\ }

" Vimwiki ----------------------------------------------------------------------
let g:vimwiki_folding='expr'
let g:vimwiki_list = [{'path': '$HOME/Library/Mobile Documents/com~apple~CloudDocs/Documents/vimwiki',
      \ 'diary_header': 'Plan',
      \ 'diary_index': 'index',
      \ 'diary_rel_path': 'plan/'}]

" Change cursor shape between insert and normal mode in iTerm2.app
" if $TERM_PROGRAM =~ "iTerm"
"     let &t_SI = "\<Esc>]50;CursorShape=1\x7" " Vertical bar in insert mode
"     let &t_EI = "\<Esc>]50;CursorShape=0\x7" " Block in normal mode
" endif
"set guicursor=n-v-c:block-Cursor/lCursor-blinkon0,i-ci:ver25-Cursor/lCursor,r-cr:hor20-Cursor/lCursor

" UI ===========================================================================

syntax on
set ruler
set encoding=utf8

set t_8b=^[[48;2;%lu;%lu;%lum
set t_8f=^[[38;2;%lu;%lu;%lum
set background=dark
let ayucolor="dark"
colorscheme ayu
" if (empty($TMUX))
  if (has("termguicolors"))
    set termguicolors
  endif
" endif

set number relativenumber

augroup numbertoggle
  autocmd!
  autocmd BufEnter,FocusGained,InsertLeave * set relativenumber
  autocmd BufLeave,FocusLost,InsertEnter   * set norelativenumber
augroup END

set showmode
set showcmd
set title
set cul
set colorcolumn=81
set autochdir

set splitbelow
set splitright
"set textwidth=79
set scrolloff=4
set laststatus=2
"set cmdheight=2
"set noshowmode

set showmatch
set hlsearch
nnoremap <esc> :noh<return><esc>
set ignorecase
set smartcase

set noerrorbells
set novisualbell

set autoindent
set smartindent

set tabstop=2
set softtabstop=2
set shiftwidth=2
set smarttab
set expandtab

if has('gui')
  set guioptions-=e
endif
set showtabline=2

if has('mouse')
  set mouse=a
endif

" EDITOR =======================================================================

let g:python3_host_skip_check = 1

set hidden
set nobackup
set nowritebackup
set noswapfile

set clipboard=unnamedplus
set completeopt-=preview

" Terminal
"tnoremap <Esc> <C-\><C-n>

" Copy/paste
set pastetoggle=<F10>

" History
set undolevels=10000

" MAPPINGS =====================================================================

" Fix some common typos --------------------------------------------------------
cnoreabbrev W! w!
cnoreabbrev Q! q!
cnoreabbrev Qall! qall!
cnoreabbrev Wq wq
cnoreabbrev Wa wa
cnoreabbrev wQ wq
cnoreabbrev WQ wq
cnoreabbrev W w
cnoreabbrev Q q
cnoreabbrev Sp sp
cnoreabbrev sP sp
cnoreabbrev Qall qall

" TIPS =========================================================================
" \t tab listing
" \b buffer listing
" \f ranger
