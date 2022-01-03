-- Diagnostics
-- https://www.reddit.com/r/neovim/comments/ru871v/how_to_change_the_position_of_linting_in/
vim.cmd([[autocmd CursorHold,CursorHoldI * lua vim.diagnostic.open_float(nil, {focus=false})]])
vim.diagnostic.config({
	virtual_text = {
		prefix = "●", -- Could be '■', '●', '▎', 'x'
	},
})

-- Symbols
-- https://github.com/neovim/nvim-lspconfig/wiki/UI-customization#change-diagnostic-symbols-in-the-sign-column-gutter
local signs = { Error = " ", Warn = " ", Hint = " ", Info = " " }
for type, icon in pairs(signs) do
	local hl = "DiagnosticSign" .. type
	vim.fn.sign_define(hl, { text = icon, texthl = hl, numhl = hl })
end

-- Go to definition
vim.api.nvim_set_keymap("n", "gd", "<CMD>lua vim.lsp.buf.definition()<CR>", { silent = true, noremap = true })

-- Hover doc
vim.api.nvim_set_keymap("n", "K", "<CMD>lua vim.lsp.buf.hover()<CR>", { silent = true, noremap = true })

-- Format
vim.api.nvim_set_keymap("n", "<Leader>d", "<CMD>lua vim.lsp.buf.formatting()<CR>", { silent = true, noremap = true })

-- Code actions
vim.api.nvim_set_keymap("n", "ga", "<CMD>lua vim.lsp.buf.code_action()<CR>", { silent = true, noremap = true })

local ok, lsp_installer = pcall(require, "nvim-lsp-installer")
if not ok then
	return
end

-- Register a handler that will be called for all installed servers.
-- Alternatively, you may also register handlers on specific server instances instead (see example below).
lsp_installer.on_server_ready(function(server)
	local aerial = require("aerial")

	local opts = {
		on_attach = aerial.on_attach,
	}

	-- This setup() function is exactly the same as lspconfig's setup function.
	-- Refer to https://github.com/neovim/nvim-lspconfig/blob/master/doc/server_configurations.md
	server:setup(opts)
end)

local ok, aerial = pcall(require, "aerial")
if not ok then
	return
end
-- Aerial does not set any mappings by default, so you'll want to set some up
aerial.register_attach_cb(function(bufnr)
	-- Toggle the aerial window with <Leader>a
	vim.api.nvim_buf_set_keymap(bufnr, "n", "<Leader>a", "<cmd>AerialToggle!<CR>", {})
	-- Jump forwards/backwards with '{' and '}'
	vim.api.nvim_buf_set_keymap(bufnr, "n", "{", "<cmd>AerialPrev<CR>", {})
	vim.api.nvim_buf_set_keymap(bufnr, "n", "}", "<cmd>AerialNext<CR>", {})
	-- Jump up the tree with '[[' or ']]'
	vim.api.nvim_buf_set_keymap(bufnr, "n", "[[", "<cmd>AerialPrevUp<CR>", {})
	vim.api.nvim_buf_set_keymap(bufnr, "n", "]]", "<cmd>AerialNextUp<CR>", {})
end)

local ok, null_ls = pcall(require, "null-ls")
if not ok then
	return
end

null_ls.setup({
	sources = {
		-- Diagnostics
		null_ls.builtins.diagnostics.credo,
		null_ls.builtins.diagnostics.flake8,
		null_ls.builtins.diagnostics.proselint,
		null_ls.builtins.diagnostics.shellcheck,
		-- Code actions
		null_ls.builtins.code_actions.gitsigns,
		null_ls.builtins.code_actions.proselint,
		null_ls.builtins.code_actions.shellcheck,
		-- Formatters
		null_ls.builtins.formatting.prettier,
		null_ls.builtins.formatting.joker,
		null_ls.builtins.formatting.stylua,
		null_ls.builtins.formatting.swiftformat,
	},
})
