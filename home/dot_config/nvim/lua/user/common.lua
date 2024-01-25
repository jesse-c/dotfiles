-- Pre-setup for keymaps
vim.o.timeoutlen = 300

-- Pre-setup for UI
-- True colour support
vim.o.termguicolors = true

-- Local per machine settings
local f = io.open("~/.config/nvim/local.vim", "r")

if f ~= nil then
  io.close(f)
  vim.cmd([[ source ~/.config/nvim/local.vim ]])
end
