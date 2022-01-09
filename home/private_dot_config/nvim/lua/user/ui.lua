local o = vim.o -- For the globals options
local wo = vim.wo -- For the window local options
local bo = vim.bo -- For the buffer local options

local autocmd = require("user.autocmd")

---------------
-- Line numbers
---------------
o.number = true
wo.relativenumber = true
----------------------
-- Syntax highlighting
----------------------
vim.cmd([[ filetype plugin indent on ]])
vim.cmd([[ syntax enable ]])
vim.g.indent_blankline_enabled = true
vim.g.indent_blankline_char = "┆"
vim.g.indent_blankline_char_list = { "|", "¦", "┆", "┊" }
vim.g.indent_blankline_use_treesitter = true
vim.g.indent_blankline_show_first_indent_level = false
----------------------
-- Splits
----------------------
-- Auto resize splits when window is resized
autocmd({
	events = { "VimResized" },
	pattern = "*",
	command = "wincmd =",
})
o.splitbelow = true
o.splitright = true
vim.api.nvim_set_keymap("n", "<C-X>", "<cmd>split<cr>", { silent = true, noremap = true })
vim.api.nvim_set_keymap("n", "<C-V>", "<cmd>vsplit<cr>", { silent = true, noremap = true })
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
o.concealcursor = ""
-----------------------
-- Status
-----------------------
o.showmode = true
------------------------------
-- Column marker
------------------------------
-- Using lukas-reineke/virt-column.nvim instead
-- o.colorcolumn = "81"
---------------------------
-- Characters
---------------------------
-- Show whitespace characters
o.list = true
-- Show trailing whitespace as dots
o.listchars = "trail:·,tab:→ ,nbsp:·"
----------------------------
-- Sign column
----------------------------
-- Always show sign column
wo.signcolumn = "yes"
----------------------
-- Folds
----------------------
o.foldlevelstart = 99
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
o.breakindentopt = "shift:2"
-- Character to show after wrap
o.showbreak = "↳ "
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
------------
-- Rendering
------------
-- When running macros and regexes on a large file, lazy redraw tells Neovim
-- not to draw the screen, which greatly speeds it up, upto 6-7x faster
o.lazyredraw = true

vim.g.rainbow_active = true