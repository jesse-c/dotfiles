local ok, telescope = pcall(require, "telescope")
if not ok then
	return
end

local actions = require("telescope.actions")

telescope.setup({
	pickers = {
		buffers = {
			show_all_buffers = true,
			sort_lastused = true,
			mappings = {
				i = {
					["<c-d>"] = actions.delete_buffer + actions.move_to_top,
				},
			},
		},
	},
})

telescope.load_extension("fzf")
telescope.load_extension("notify")
telescope.load_extension("zoxide")
telescope.load_extension("file_browser")

-- Finder
vim.api.nvim_set_keymap("n", "<Leader>g", "<CMD>Telescope tags<CR>", { silent = true, noremap = true })
vim.api.nvim_set_keymap("n", "<Leader>h", "<CMD>Telescope buffers<CR>", { silent = true, noremap = true })
vim.api.nvim_set_keymap("n", "<Leader>j", "<CMD>Telescope find_files<CR>", { silent = true, noremap = true })
vim.api.nvim_set_keymap("n", "<Leader>k", "<CMD>Telescope marks<CR>", { silent = true, noremap = true })
vim.api.nvim_set_keymap("n", "<Leader>;", "<CMD>Telescope live_grep<CR>", { silent = true, noremap = true })
vim.api.nvim_set_keymap("n", "<Leader>'", "<CMD>Telescope command_history<CR>", { silent = true, noremap = true })
-- LSP
vim.api.nvim_set_keymap("n", "<Leader>q", "<CMD>Telescope lsp_document_symbols<CR>", { silent = true, noremap = true })
vim.api.nvim_set_keymap("n", "<Leader>w", "<CMD>Telescope lsp_workspace_symbols<CR>", {
	silent = true,
	noremap = true,
})
vim.api.nvim_set_keymap("n", "<Leader>r", "<CMD>Telescope diagnostics bufnr=0<CR>", { silent = true, noremap = true })
vim.api.nvim_set_keymap("n", "<Leader>t", "<CMD>Telescope diagnostics<CR>", { silent = true, noremap = true })
