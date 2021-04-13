local o = vim.o -- For the globals options
local wo = vim.wo -- For the window local options
local bo = vim.bo -- For the buffer local options

local configuration_folder = vim.fn.expand'~'..'/.config/nvim'

require('abbrs')

-- ====================
-- Editor
-- ====================

---------------------
-- File
---------------------
-- Do not write backups
o.backup = false
o.writebackup = false
-- Do not create swap files
o.swapfile = false
-------------------------
-- Encoding
-------------------------
o.encoding = 'utf-8'
------------
-- Clipboard
------------
o.clipboard = 'unnamed'
-----------------------
-- Format
-----------------------
o.formatoptions = 'qj'
------------------------------
-- Spellchecking
------------------------------
o.spelllang = 'en,es'
-------------------
-- Undo persistance
-------------------
o.undodir = configuration_folder..'/undodir.nvim'
o.undofile = true
o.undolevels = 10000
-----------
-- Commands
-----------
o.inccommand = 'nosplit'
-----------------------------
-- Local per machine settings
-----------------------------
local f=io.open('~/.config/nvim/local.vim', "r")

if f~=nil then
  io.close(f)
  vim.cmd [[ source ~/.config/nvim/local.vim ]]
end
----------------
-- Miscellaneous
----------------
vim.cmd [[ nmap <F3> i<C-R>=strftime("%Y-%m-%d")<CR><Esc> ]]
vim.cmd [[ imap <F3> <C-R>=strftime("%Y-%m-%d")<CR> ]]

vim.cmd [[ nnoremap <leader>ev :vsplit $MYVIMRC<cr> ]] -- Edit my vimrc
vim.cmd [[ nnoremap <leader>sv :source $MYVIMRC<cr> ]] -- Source my vimrc

-- https://vim.fandom.com/wiki/Copy_filename_to_clipboard
vim.cmd [[ nmap <leader>cs :let @*=expand("%")<CR> ]]
vim.cmd [[ nmap <leader>cl :let @*=expand("%:p")<CR> ]]
