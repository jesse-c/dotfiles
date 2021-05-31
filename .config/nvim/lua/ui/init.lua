local o = vim.o -- For the globals options
local wo = vim.wo -- For the window local options
local bo = vim.bo -- For the buffer local options

local autocmd = require('core.autocmd')

--------
-- Theme
--------
vim.cmd [[ colorscheme edge ]]
-- o.background = 'light'
o.background = 'dark'
---------------
-- Line numbers
---------------
o.number = true
wo.relativenumber = true
----------------------
-- Syntax highlighting
----------------------
vim.cmd [[ filetype plugin indent on ]]
vim.cmd [[ syntax enable ]]
----------------------
-- Search
----------------------
o.incsearch = true
o.hlsearch = true
o.showmatch = true
vim.cmd [[ set ignorecase ]]
vim.cmd [[ set smartcase ]]
vim.cmd [[ nnoremap <esc> :noh<return><esc> ]]
----------------------
-- Splits
----------------------
-- Auto resize splits when window is resized
autocmd {
  events = { 'VimResized' },
  pattern = '*',
  command = 'wincmd ='
}
o.splitbelow = true
o.splitright = true
vim.cmd [[ nmap <silent> <C-k> :wincmd k<CR> ]]
vim.cmd [[ nmap <silent> <C-j> :wincmd j<CR> ]]
vim.cmd [[ nmap <silent> <C-h> :wincmd h<CR> ]]
vim.cmd [[ nmap <silent> <C-l> :wincmd l<CR> ]]
vim.cmd [[ nmap <silent> <C-q> :q<CR> ]]
----------
-- Buffers
----------
-- Close a buffer but keep the window open
vim.cmd [[ command! Bd enew\|bd \# ]]
------------
-- Scrolling
------------
o.scrolloff = 4
-----------------------------
-- Current line
-----------------------------
o.ruler = true
o.cursorline = true
------------------------
-- Conceal
------------------------
o.concealcursor = ''
-----------------------
-- Status
-----------------------
o.showmode = true
------------------------------
-- Column marker
------------------------------
o.colorcolumn = '81'
---------------------------
-- Characters
---------------------------
-- Show whitespace characters
o.list = true
-- Show trailing whitespace as dots
o.listchars = 'trail:·,tab:→ ,nbsp:·'
----------------------------
-- Sign column
----------------------------
-- Always show sign column
wo.signcolumn = 'yes'
----------------------
-- Folds
----------------------
o.foldlevelstart = 99
-------------------------
-- Terminal
-------------------------
-- Enable mouse support
o.mouse = 'a'
-- Restore cursor on exit
autocmd {
  events = { 'VimLeave' },
  pattern = '*',
  command = 'set guicursor=a:ver100-blinkon0'
}
----------------------------
-- Indentation
----------------------------
-- Use spaces instead of tabs
o.expandtab = true
-- More intelligent tabulation
o.smarttab = true
-- Number of spaces that a <Tab> counts for
o.tabstop = 2
-- Number of spaces used for autoindent
o.shiftwidth = o.tabstop
-- Number of spaces that a <Tab> counts for
o.softtabstop = o.tabstop
-- Copy indent from current line when starting a new line
o.autoindent = true
-- Do smart autoindenting when starting a new line
o.smartindent = true
-------------------------
-- Wrapping
-------------------------
-- Enable soft-wrapping
o.wrap = true
-- Do not break words at the middle
o.linebreak = true
-- Mantain indentation on wrap
o.breakindent = true
-- Add characters after wrap
o.breakindentopt = 'shift:2'
-- Character to show after wrap
o.showbreak = '↳ '
----------
-- Tabline
----------
o.showtabline = 2
o.laststatus = 2
o.title = true
--------
-- Bells
--------
o.errorbells = false
o.visualbell = false
