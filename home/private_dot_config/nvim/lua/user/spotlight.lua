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
	extensions = {
		file_browser = {
			path = "%:p:h",
		},
	},
})

telescope.load_extension("fzf")
telescope.load_extension("notify")
telescope.load_extension("zoxide")
telescope.load_extension("file_browser")

-- Finder
vim.api.nvim_set_keymap("n", "<Space>g", "<CMD>Telescope tags<CR>", { silent = true, noremap = true })
vim.api.nvim_set_keymap("n", "<Space>h", "<CMD>Telescope buffers<CR>", { silent = true, noremap = true })
vim.api.nvim_set_keymap("n", "<Space>j", "<CMD>Telescope find_files<CR>", { silent = true, noremap = true })
vim.api.nvim_set_keymap("n", "<Space>k", "<CMD>Telescope marks<CR>", { silent = true, noremap = true })
vim.api.nvim_set_keymap("n", "<Space>;", "<CMD>Telescope live_grep<CR>", { silent = true, noremap = true })
vim.api.nvim_set_keymap("n", "<Space>'", "<CMD>Telescope command_history<CR>", { silent = true, noremap = true })
vim.api.nvim_set_keymap("n", "<Space>f", "<CMD>Telescope file_browser<CR>", { silent = true, noremap = true })
-- LSP
vim.api.nvim_set_keymap("n", "<Space>q", "<CMD>Telescope lsp_document_symbols<CR>", { silent = true, noremap = true })
vim.api.nvim_set_keymap("n", "<Space>w", "<CMD>Telescope lsp_workspace_symbols<CR>", {
	silent = true,
	noremap = true,
})
vim.api.nvim_set_keymap("n", "<Space>r", "<CMD>Telescope diagnostics bufnr=0<CR>", { silent = true, noremap = true })
vim.api.nvim_set_keymap("n", "<Space>t", "<CMD>Telescope diagnostics<CR>", { silent = true, noremap = true })
