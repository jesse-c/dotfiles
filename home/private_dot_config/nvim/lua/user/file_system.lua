local ok, nvim_tree = pcall(require, "nvim-tree")
if not ok then
	return
end

nvim_tree.setup()

vim.api.nvim_set_keymap("n", "<Leader>v", "<CMD>NvimTreeFindFile<CR>", { silent = true, noremap = true })
vim.api.nvim_set_keymap("n", "<Leader>n", "<CMD>NvimTreeToggle<CR>", { silent = true, noremap = true })
vim.g.nvim_tree_width = 60
