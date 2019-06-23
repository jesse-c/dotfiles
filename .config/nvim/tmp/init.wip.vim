" PLUGINS ======================================================================
call plug#begin('~/.local/share/nvim/plugged')




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
" Plug 'ElmCast/elm-vim'

" Go
" Plug 'fatih/vim-go'
Plug 'buoto/gotests-vim'
" Plug 'sebdah/vim-delve'

" Rust
" Racket
" Lisp/Scheme
" Ruby
" Python
" Clojure
Plug 'venantius/vim-cljfmt'
Plug 'tpope/vim-fireplace/'

call plug#end()

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

" Tags -------------------------------------------------------------------------
nmap <F8> :TagbarToggle<CR>

" Gutentags
" Store tags file somewhere else than in the project dir
let g:gutentags_cache_dir = '~/.gutentags'
" Only create tags for tracked files
let g:gutentags_file_list_command = 'git ls-files'


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
