-- Close a buffer but keep the window open
vim.api.nvim_create_user_command("Bd", "enew | bd #", {})
