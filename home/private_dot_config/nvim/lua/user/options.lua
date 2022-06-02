local configuration_folder = vim.fn.expand("~") .. "/.config/nvim"

-- ====================
-- Editor
-- ====================

----------
-- Colours
----------
-- True colour support
vim.o.termguicolors = true
---------------------
-- File
---------------------
-- Do not write backups
vim.o.backup = false
vim.o.writebackup = false
-- Do not create swap files
vim.o.swapfile = false
-- Opt-in to filetype.lua
-- https://www.reddit.com/r/neovim/comments/rvwsl3/introducing_filetypelua_and_a_call_for_help/
vim.g.do_filetype_lua = true
vim.g.did_load_filetypes = true
-------------------------
-- Encoding
-------------------------
vim.o.encoding = "utf-8"
------------
-- Clipboard
------------
vim.o.clipboard = "unnamed"
-----------------------
-- Format
-----------------------
vim.o.formatoptions = "qj"
------------------------------
-- Spellchecking
------------------------------
vim.o.spelllang = "en"
-------------------
-- Undo persistance
-------------------
vim.o.undodir = configuration_folder .. "/undodir.nvim"
vim.o.undofile = true
vim.o.undolevels = 10000
--------
-- Shell
--------
-- For fish users out there, its quite slow compared to stock bash, as such
-- tell Neovim to use bash to execute commands
vim.opt.shell = "/bin/bash"
-----------
-- Commands
-----------
vim.o.inccommand = "nosplit"
-----------------------------
-- Local per machine settings
-----------------------------
local f = io.open("~/.config/nvim/local.vim", "r")

if f ~= nil then
	io.close(f)
	vim.cmd([[ source ~/.config/nvim/local.vim ]])
end
--------
-- Folds
--------
vim.wo.foldmethod = "expr"
vim.wo.foldexpr = "nvim_treesitter#foldexpr()"
