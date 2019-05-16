" PLUGINS ======================================================================
call plug#begin('~/.local/share/nvim/plugged')

" UNSORTED ---------------------------------------------------------------------
Plug 'vimwiki/vimwiki'

" UI ---------------------------------------------------------------------------
Plug 'ayu-theme/ayu-vim'
Plug 'ryanoasis/vim-devicons'
Plug 'scrooloose/nerdtree'
Plug 'Xuyuanp/nerdtree-git-plugin'
Plug 'tiagofumo/vim-nerdtree-syntax-highlight'
Plug 'chrisbra/Colorizer'
Plug 'mhinz/vim-startify'
Plug 'Yggdroot/indentLine'
Plug 'mbbill/undotree'
Plug 'luochen1990/rainbow'
Plug 'editorconfig/editorconfig-vim'
Plug 'google/vim-searchindex'
Plug 'gcmt/taboo.vim'
Plug 'troydm/zoomwintab.vim'
Plug 'tpope/vim-abolish'

" Marks
Plug 'kshenoy/vim-signature'

" Buffers
Plug 'jeetsukumaran/vim-buffergator'

" Registers
Plug 'junegunn/vim-peekaboo'

" Session
Plug 'tpope/vim-obsession'

" Statusbar
Plug 'itchyny/lightline.vim'
Plug 'maximbaz/lightline-ale'

" Editor -----------------------------------------------------------------------
Plug 'tpope/vim-surround'

" Comments
Plug 'tpope/vim-commentary'

" VCS --------------------------------------------------------------------------
Plug 'tpope/vim-fugitive'
Plug 'rhysd/git-messenger.vim'
Plug 'jreybert/vimagit'
Plug 'mhinz/vim-signify'

" Search -----------------------------------------------------------------------
Plug '/usr/local/opt/fzf'
Plug 'junegunn/fzf.vim'

" File system ------------------------------------------------------------------
Plug 'francoiscabrol/ranger.vim'
Plug 'tpope/vim-eunuch'
Plug 'airblade/vim-rooter'

" Development ------------------------------------------------------------------
Plug 'w0rp/ale'

Plug 'ncm2/ncm2'
Plug 'roxma/nvim-yarp'
Plug 'ncm2/ncm2-bufword'
Plug 'ncm2/ncm2-tmux'
Plug 'ncm2/ncm2-path'
Plug 'ncm2/ncm2-go'
Plug 'ncm2/ncm2-racer'

Plug 'autozimu/LanguageClient-neovim', {
    \ 'branch': 'next',
    \ 'do': 'bash install.sh',
    \ }

" Documentation
" Plug 'Shougo/echodoc.vim'
Plug 'rizzatti/dash.vim'
Plug 'majutsushi/tagbar'
Plug 'ludovicchabant/vim-gutentags'

" HTTP
Plug 'diepm/vim-rest-console'

" Neomutt
Plug 'neomutt/neomutt.vim'

" Elm
Plug 'ElmCast/elm-vim'

" Go
Plug 'fatih/vim-go'
Plug 'buoto/gotests-vim'
" Plug 'sebdah/vim-delve'

" Rust
" Racket
" Lisp/Scheme
" Ruby
" Python

call plug#end()

" UNSORTED ======================================================================
set encoding=UTF-8
set inccommand=nosplit

" HTTP -------------------------------------------------------------------------
let g:vrc_horizontal_split = 1
"let g:vrc_set_default_mapping = 0
"let g:vrc_show_command = 1
let g:vrc_trigger = '<Leader>r'
let s:vrc_auto_format_response_patterns = {
      \ 'json': 'jq "."',
      \ 'xml': 'xmllint --format -',
      \}
let g:vrc_curl_opts={
    \'--include': '',
    \'--location': '',
    \'--show-error': '',
    \'--silent': ''
\}

" Vimwiki ----------------------------------------------------------------------
let g:vimwiki_folding='expr'
let g:vimwiki_list = [{'path': '$HOME/Library/Mobile Documents/com~apple~CloudDocs/Documents/vimwiki',
      \ 'diary_header': 'Plan',
      \ 'diary_index': 'index',
      \ 'diary_rel_path': 'plan/'}]

" FILE SYSTEM ===================================================================
set hidden
set nobackup
set nowritebackup
set noswapfile
filetype plugin on

" DEVELOPMENT ==================================================================
" LSP --------------------------------------------------------------------------
let g:LanguageClient_serverCommands = {
    \ 'rust': ['rls'],
    \ 'python': ['pyls'],
    \ 'go': ['gopls'],
    \ 'bash': ['bash-language-server'],
    \ 'ruby': ['solargraph'],
    \ 'cpp': ['ccls', '--log-file=/tmp/cc.log'],
    \ 'c': ['ccls', '--log-file=/tmp/cc.log'],
    \ 'javascript': ['javascript-typescript-stdio'],
    \ }
let g:LanguageClient_rootMarkers = {
    \ 'go': ['.git', 'go.mod'],
    \ }
nnoremap <c-p> :call LanguageClient_contextMenu()<CR>
let g:LanguageClient_loadSettings = 1 " Use an absolute configuration path if you want system-wide settings
let g:LanguageClient_settingsPath = '/home/jesse/.config/nvim/lsp-settings.json'

" NCM2 -------------------------------------------------------------------------
autocmd BufEnter * call ncm2#enable_for_buffer()
au User Ncm2PopupOpen set completeopt=noinsert,menuone,noselect
au User Ncm2PopupClose set completeopt=menuone
inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"

" ALE --------------------------------------------------------------------------
let g:ale_fix_on_save = 1
" let g:ale_completion_enabled = 1
let g:ale_fixers = {
\   '*': ['remove_trailing_lines', 'trim_whitespace'],
\   'python': ['black'],
\}
let g:ale_elm_format_use_global = 1
let g:ale_elm_format_options = '--yes --elm-version=0.19'
let g:ale_elm_make_use_global = 1
nmap <silent> <C-f> <Plug>(ale_previous_wrap)
nmap <silent> <C-g> <Plug>(ale_next_wrap)

" EDITOR =======================================================================
" Copy/paste -------------------------------------------------------------------
set pastetoggle=<F10>
set nopaste
set clipboard=unnamed

let g:python_host_prog='/Users/jesse/.asdf/shims/python2'
let g:python3_host_prog='/Users/jesse/.asdf/shims/python3'

" Undo -------------------------------------------------------------------------
if has("persistent_undo")
  set undodir=~/.undo/
  set undofile
endif
nnoremap <F5> :UndotreeToggle<cr>

" Search -----------------------------------------------------------------------
" FZF
nmap <Leader>g :Tags<CR>
nmap <Leader>h :Buffers<CR>
nmap <Leader>j :Files<CR>
nmap <Leader>k :Marks<CR>
nmap <Leader>l :History<CR>
nmap <Leader>; :Rg<Space>
nmap <Leader>' :History:<CR>
cnoreabbrev ag Ag
cnoreabbrev aG Ag
cnoreabbrev AG Ag
command! -bang -nargs=* Rg
  \ call fzf#vim#grep(
  \   'rg --smart-case --column --line-number --no-heading --color=always '.shellescape(<q-args>), 1,
  \   <bang>0 ? fzf#vim#with_preview('up:60%')
  \           : fzf#vim#with_preview('right:50%:hidden', '?'),
  \   <bang>0)

" History ----------------------------------------------------------------------
set undolevels=10000

" UI ===========================================================================
syntax on
set ruler
" set background=dark
set termguicolors     " enable true colors support
let ayucolor="dark"   " for dark version of theme
colorscheme ayu

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
set showtabline=2

set splitbelow
set splitright
set scrolloff=4
set laststatus=2

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

if has('mouse')
  set mouse=a
endif

let g:rainbow_active = 1

nmap <silent> <C-k> :wincmd k<CR>
nmap <silent> <C-j> :wincmd j<CR>
nmap <silent> <C-h> :wincmd h<CR>
nmap <silent> <C-l> :wincmd l<CR>

nmap <silent> <C-m> :GitMessenger<CR>

" Tags -------------------------------------------------------------------------
nmap <F8> :TagbarToggle<CR>

" Gutentags
" Store tags file somewhere else than in the project dir
let g:gutentags_cache_dir = '~/.gutentags'
" Only create tags for tracked files
let g:gutentags_file_list_command = 'git ls-files'

" Lightline --------------------------------------------------------------------
let g:indentLine_enabled = 1
"let g:indentLine_showFirstIndentLevel = 1
let g:indentLine_setColors = 0
"let g:indentLine_char = '┆'
let g:lightline#ale#indicator_checking = "\uf110"
let g:lightline#ale#indicator_warnings = "\uf071"
let g:lightline#ale#indicator_errors = "\uf05e"
let g:lightline#ale#indicator_ok = "\uf00c"

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


" Colorizer --------------------------------------------------------------------
let g:colorizer_auto_filetype='css,scss,html,htm,elm'
"let g:colorizer_skip_comments = 1

" VimiDevIcons -----------------------------------------------------------------
let g:webdevicons_enable = 1
let g:webdevicons_enable_nerdtree = 1
"let g:webdevicons_conceal_nerdtree_brackets = 1

" NerdTree ---------------------------------------------------------------------
nmap <silent> <c-S-n> :NERDTreeToggle<CR>
let g:NERDTreeShowHidden = 1

set guifont=RobotoMono\ Nerd\ Font:h11 " TODO
"set guifont=*

" LANGUAGES ====================================================================
" Go ---------------------------------------------------------------------------
map <silent> <c-e> :GoDecls<CR>
let g:go_fmt_command = "goimports"
let g:go_highlight_functions = 1
let g:go_highlight_function_calls = 1
let g:go_highlight_methods = 1
let g:go_highlight_build_constraints = 1

" Elm --------------------------------------------------------------------------
let g:elm_format_autosave = 1
let g:elm_format_fail_silently = 0
let g:elm_detailed_complete = 1
let g:elm_setup_keybindings = 0
let g:elm_make_show_warnings = 1

" MISC. ========================================================================

command! FormatJson :%!jq '.'

nmap <F3> i<C-R>=strftime("%Y-%m-%d")<CR><Esc>
imap <F3> <C-R>=strftime("%Y-%m-%d")<CR>

nnoremap <leader>ev :vsplit $MYVIMRC<cr> " Edit my vimrc
nnoremap <leader>sv :source $MYVIMRC<cr> " Source my vimrc

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
" \ leader
