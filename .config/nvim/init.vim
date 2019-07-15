" For a paranoia.
" Normally `:set nocp` is not needed, because it is done automatically
" when .vimrc is found.
if &compatible
  " `:set nocp` has many side effects. Therefore this should be done
  " only when 'compatible' is set.
  set nocompatible
endif

call plug#begin('~/.vim/plugged')

" UI -------------------------------------------------------------------------
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
Plug 'ludovicchabant/vim-gutentags'
Plug 'liuchengxu/vista.vim'
Plug 'junegunn/vim-easy-align'

" Marks
Plug 'kshenoy/vim-signature'

" Registers
Plug 'junegunn/vim-peekaboo'

" Session
Plug 'tpope/vim-obsession'

" Statusbar
Plug 'itchyny/lightline.vim'

" Buffers
Plug 'jeetsukumaran/vim-buffergator'

" Editor ---------------------------------------------------------------------
Plug 'tpope/vim-surround'

" Comments
Plug 'tpope/vim-commentary'

" Development ----------------------------------------------------------------
Plug 'sheerun/vim-polyglot'
Plug 'neoclide/coc.nvim', {'branch': 'release'}

" Go
Plug 'buoto/gotests-vim'
Plug 'sebdah/vim-delve'

" VCS ------------------------------------------------------------------------
Plug 'tpope/vim-fugitive'
Plug 'rhysd/git-messenger.vim'
Plug 'jreybert/vimagit'
Plug 'mhinz/vim-signify'

" Search ---------------------------------------------------------------------
Plug 'junegunn/fzf'
Plug 'junegunn/fzf.vim'

" File system ----------------------------------------------------------------
Plug 'francoiscabrol/ranger.vim'
Plug 'tpope/vim-eunuch'
Plug 'airblade/vim-rooter'

" Org ------------------------------------------------------------------------
Plug 'vimwiki/vimwiki'

call plug#end()

" File system -------------------------------------------------------------------

set hidden
set nobackup
set nowritebackup
set noswapfile
filetype plugin on

" Editor -----------------------------------------------------------------------

set updatetime=300
" don't give |ins-completion-menu| messages.
set shortmess+=c
set signcolumn=yes

" Copy/paste
set pastetoggle=<F10>
set nopaste
set clipboard=unnamed

let g:python_host_prog='/Users/jesse/.asdf/shims/python2'
let g:python3_host_prog='/Users/jesse/.asdf/shims/python3'

" Undo
if has("persistent_undo")
  set undodir=~/.undo/
  set undofile
endif
nnoremap <F5> :UndotreeToggle<cr>

" Development ------------------------------------------------------------------

" Polyglot
let g:polyglot_disabled = []

" Elm 
let g:elm_jump_to_error = 0
let g:elm_make_show_warnings = 0
let g:elm_syntastic_show_warnings = 0
let g:elm_browser_command = ""
let g:elm_detailed_complete = 0
let g:elm_format_autosave = 0
let g:elm_format_fail_silently = 0
let g:elm_setup_keybindings = 0

" Go
let g:go_fmt_command = "goimports"

" LSP --------------------------------------------------------------------------

" Use <c-space> to trigger completion.
inoremap <silent><expr> <c-space> coc#refresh()

" Use <cr> to confirm completion, `<C-g>u` means break undo chain at current position.
" Coc only does snippet and additional edit on confirm.
inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"

" Use tab for trigger completion with characters ahead and navigate.
" Use command ':verbose imap <tab>' to make sure tab is not mapped by other plugin.
inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

autocmd BufWritePre *.go :call CocAction("format")

nmap <silent> <C-f> <Plug>(coc-diagnostic-prev)
nmap <silent> <C-g> <Plug>(coc-diagnostic-next)

" Remap keys for gotos
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

" Use K to show documentation in preview window
nnoremap <silent> K :call <SID>show_documentation()<CR>

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction

" Remap for rename current word
nmap <leader>rn <Plug>(coc-rename)

" Use `:Format` to format current buffer
command! -nargs=0 Format :call CocAction('format')

" Using CocList
" Show all diagnostics
nnoremap <silent> <space>a  :<C-u>CocList diagnostics<cr>
" Manage extensions
nnoremap <silent> <space>e  :<C-u>CocList extensions<cr>
" Show commands
nnoremap <silent> <space>c  :<C-u>CocList commands<cr>
" Find symbol of current document
nnoremap <silent> <space>o  :<C-u>CocList outline<cr>
" Search workspace symbols
nnoremap <silent> <space>s  :<C-u>CocList -I symbols<cr>
" Do default action for next item.
nnoremap <silent> <space>j  :<C-u>CocNext<CR>
" Do default action for previous item.
nnoremap <silent> <space>k  :<C-u>CocPrev<CR>
" Resume latest coc list
nnoremap <silent> <space>p  :<C-u>CocListResume<CR>
"
" Remap for do codeAction of current line
nmap <leader>ac  <Plug>(coc-codeaction)
" Fix autofix problem of current line
nmap <leader>qf  <Plug>(coc-fix-current)

" Search -----------------------------------------------------------------------

" FZF
" nmap <Leader>g :Tags<CR>
nmap <Leader>g :Vista finder coc<CR>
nmap <Leader>h :Buffers<CR>
nmap <Leader>j :Files<CR>
" nmap <Leader>j :call Fzf_dev()<CR><Space>
nmap <Leader>k :Marks<CR>
nmap <Leader>l :History<CR>
nmap <Leader>; :Rg<Space>
nmap <Leader>' :History:<CR>
cnoreabbrev ag Ag
cnoreabbrev aG Ag
cnoreabbrev AG Ag

let g:vista_fzf_preview = ['right:50%']

let g:fzf_action = {
  \ 'ctrl-t': 'tab split',
  \ 'ctrl-x': 'split',
  \ 'ctrl-v': 'vsplit' }

command! -bang -nargs=* Rg
  \ call fzf#vim#grep(
  \   'rg --column --line-number --no-heading --color=always --smart-case '.shellescape(<q-args>), 1,
  \   <bang>0 ? fzf#vim#with_preview('up:60%')
  \           : fzf#vim#with_preview('right:50%:hidden', '?'),
  \   <bang>0)

let $FZF_DEFAULT_COMMAND = 'fd --type f'

let g:fzf_files_options =
  \ '--preview "(bat --theme="OneHalfDark" --style=numbers,changes --color always {} || cat {}) | head -'.&lines.'"'

" https://www.reddit.com/r/vim/comments/9xpb18/file_preview_with_fzf_rg_bat_and_devicons/ 
if executable('rg')
  let $FZF_DEFAULT_COMMAND = 'rg --files --hidden --follow --glob "!.git/*"'
  set grepprg=rg\ --vimgrep
  command! -bang -nargs=* Find call fzf#vim#grep('rg --column --line-number --no-heading --fixed-strings --ignore-case --hidden --follow --glob "!.git/*" --color "always" '.shellescape(<q-args>).'| tr -d "\017"', 1, <bang>0)
endif

" Files + devicons
function! Fzf_dev()
  let l:fzf_files_options = '--preview "bat --theme="OneHalfDark" --style=numbers,changes --color always {2..-1} | head -'.&lines.'"'

  function! s:files()
    let l:files = split(system($FZF_DEFAULT_COMMAND), '\n')
    return s:prepend_icon(l:files)
  endfunction

  function! s:prepend_icon(candidates)
    let l:result = []
    for l:candidate in a:candidates
      let l:filename = fnamemodify(l:candidate, ':p:t')
      let l:icon = WebDevIconsGetFileTypeSymbol(l:filename, isdirectory(l:filename))
      call add(l:result, printf('%s %s', l:icon, l:candidate))
    endfor

    return l:result
  endfunction

  function! s:edit_file(item)
    let l:pos = stridx(a:item, ' ')
    let l:file_path = a:item[pos+1:-1]
    execute 'silent e' l:file_path
  endfunction

  call fzf#run({
        \ 'source': <sid>files(),
        \ 'sink':   function('s:edit_file'),
        \ 'options': '-m ' . l:fzf_files_options,
        \ 'down':    '40%' })
endfunction

" History ----------------------------------------------------------------------

set undolevels=10000

" UI ---------------------------------------------------------------------------

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

let g:indentLine_enabled = 1
"let g:indentLine_showFirstIndentLevel = 1
let g:indentLine_setColors = 0
"let g:indentLine_char = '┆'

" Start interactive EasyAlign in visual mode (e.g. vipga)
xmap ga <Plug>(EasyAlign)

" Start interactive EasyAlign for a motion/text object (e.g. gaip)
nmap ga <Plug>(EasyAlign)

" Tags -------------------------------------------------------------------------

" Gutentags
" Store tags file somewhere else than in the project dir
let g:gutentags_cache_dir = '~/.gutentags'
" Only create tags for tracked files
let g:gutentags_file_list_command = 'git ls-files'

" Vista
nmap <F8> :Vista!!<CR>
let g:vista_default_executive = 'coc'

" Lightline --------------------------------------------------------------------

set noshowmode " it's in the statusline now

function! MyFiletype()
  return winwidth(0) > 70 ? (strlen(&filetype) ? &filetype . ' ' . WebDevIconsGetFileTypeSymbol() : 'no ft') : ''
endfunction

function! MyFileformat()
  return winwidth(0) > 70 ? (&fileformat . ' ' . WebDevIconsGetFileFormatSymbol()) : ''
endfunction

function! NearestMethodOrFunction() abort
  return get(b:, 'vista_nearest_method_or_function', '')
endfunction

autocmd User GutentagsUpdating call lightline#update()
autocmd User GutentagsUpdated call lightline#update()

autocmd User CocDiagnosticChange call lightline#update()

let g:lightline = {
      \ 'colorscheme': 'ayu',
      \ 'active': {
      \   'left': [ [ 'mode', 'paste' ],
      \             [ 'gitbranch', 'readonly', 'fp', 'modified' ] ],
      \   'right': [ [ 'lineinfo' ],
      \              [ 'percent' ],
      \              [ 'method', 'cocstatus', 'fileformat', 'fileencoding', 'filetype'] ]
      \ },
      \ 'inactive': {
      \   'left': [ [ 'readonly', 'fp', 'modified', 'gitbranch' ] ],
      \   'right': [ [ ] ]
      \ },
      \ 'component': {
      \   'fp': '%<%F%<'
      \ },
      \ 'component_function': {
      \   'gitbranch': 'fugitive#head',
      \   'cocstatus': 'coc#status',
      \   'filetype': 'MyFiletype',
      \   'fileformat': 'MyFileformat',
      \   'method': 'NearestMethodOrFunction'
      \ },
      \ }

let g:lightline.tabline = {
  \   'left': [ ['tabs'] ],
  \   'right': [ [ ] ],
  \ }

" Colorizer --------------------------------------------------------------------
let g:colorizer_auto_filetype='css,scss,html,htm,elm'
let g:colorizer_skip_comments = 1

" VimiDevIcons -----------------------------------------------------------------
let g:webdevicons_enable = 1
let g:webdevicons_enable_nerdtree = 1
let g:webdevicons_conceal_nerdtree_brackets = 1

" NerdTree ---------------------------------------------------------------------
nmap <silent> <c-S-n> :NERDTreeToggle<CR>
let g:NERDTreeShowHidden = 1

set guifont=RobotoMono\ Nerd\ Font:h11 " TODO
"set guifont=*

" Org --------------------------------------------------------------------------

" Vimwiki
let g:vimwiki_folding='expr'
let g:vimwiki_list = [{'path': '$HOME/Library/Mobile Documents/com~apple~CloudDocs/Documents/vimwiki',
      \ 'diary_header': 'Plan',
      \ 'diary_index': 'index',
      \ 'diary_rel_path': 'plan/'}]

" Misc. ------------------------------------------------------------------------

command! FormatJson :%!jq '.'

nmap <F3> i<C-R>=strftime("%Y-%m-%d")<CR><Esc>
imap <F3> <C-R>=strftime("%Y-%m-%d")<CR>

nnoremap <leader>ev :vsplit $MYVIMRC<cr> " Edit my vimrc
nnoremap <leader>sv :source $MYVIMRC<cr> " Source my vimrc

" https://vim.fandom.com/wiki/Copy_filename_to_clipboard
nmap <leader>cs :let @*=expand("%")<CR>
nmap <leader>cl :let @*=expand("%:p")<CR>

set encoding=UTF-8
set inccommand=nosplit

" Local per machine settings ---------------------------------------------------

try
  source ~/.config/nvim/local.vim
catch
  " No such file? No problem; just ignore it.
endtry

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

" Tips -------------------------------------------------------------------------
" \t tab listing
" \b buffer listing
" \f ranger
" \ leader
