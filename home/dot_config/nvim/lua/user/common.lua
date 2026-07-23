-- Pre-setup for keymaps
vim.o.timeoutlen = 300

-- Local per machine settings
local local_vim = vim.fn.expand("~/.config/nvim/local.vim")
if vim.uv.fs_stat(local_vim) then
  vim.cmd("source " .. local_vim)
end
