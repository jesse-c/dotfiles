local has_telescope, telescope = pcall(require, "telescope")
if not has_telescope then
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

-- Multiple leader keys to for groups of like operations AKA minor modes
--
-- Links:
-- - Using Which Key:
--   - https://github.com/folke/which-key.nvim#%EF%B8%8F-mappings
-- - Inspiration:
--   - https://bluz71.github.io/2021/09/10/vim-tips-revisited.html#multiple-leader-keys--group-similar-actions-with-the-same-prefix
-- - Inspiration with example config:
--   - https://www.reddit.com/r/neovim/comments/sng5jy/comment/hw316x0/?utm_source=reddit&utm_medium=web2x&context=3
--   - https://gist.github.com/JSchrtke/cabd3e42a6920ef1f0835bd7ae286aad
-- - Helix:
--   - https://docs.helix-editor.com/keymap.html
local has_wk, wk = pcall(require, "which-key")
if not has_wk then
	return
end

wk.register({
	-- Minor mode: TODO Window

	-- Minor mode: TODO View

	-- Minor mode: Actions
	a = {
		name = "Actions",
		-- TODO Tree-Sitter
		-- LSP
		l = {
			name = "LSP",
			r = { "<cmd>lua vim.lsp.buf.rename()<CR>", "Rename" },
		},
	},

	-- Minor mode: Goto
	g = {
		name = "Goto",
		-- TODO Tree-Sitter
		a = { "<CMD>lua require'hop'.hint_words()<CR>", "Hop / Words" },
		-- LSP
		l = {
			name = "LSP",
			s = { "<CMD>Telescope lsp_document_symbols<CR>", "Symbols / Document" },
			S = { "<CMD>Telescope lsp_workspace_symbols<CR>", "Symbols / Workspace" },
			h = { "<CMD>Telescope diagnostics bufnr=0<CR>", "Diagnostisc / Document" },
			H = { "<CMD>Telescope diagnostics<CR>", "Diagnostics / Workspace" },
			d = { "<cmd>lua vim.lsp.buf.definition()<CR>", "Definition" },
			D = { "<cmd>lua vim.lsp.buf.declaration()<CR>", "Declaration" },
			i = { "<cmd>lua vim.lsp.buf.implementation()<CR>", "Implementation" },
			t = { "<cmd>lua vim.lsp.buf.type_definition()<CR>", "Type definition" },
		},
	},

	-- Minor mode: Show
	h = {
		name = "Show",
		-- LSP
		l = {
			name = "LSP",
			k = { "<CMD>lua vim.lsp.buf.hover()<CR>", "Hover" },
			e = { "<CMD>im.diagnostic.open_float()<CR>", "Diagnostics" },
		},
	},

	-- Minor mode: Open
	o = {
		name = "Open",
		a = { '<CMD>:lua require("user.a")()<CR>', "Alternate file" },
		t = { "<CMD>Telescope tags<CR>", "Tags" },
		b = { "<CMD>Telescope buffers<CR>", "Buffers" },
		m = { "<CMD>Telescope marks<CR>", "Marks" },
		f = { "<CMD>Telescope file_browser<CR>", "File browsers" },
	},

	-- Minor mode: Find
	f = {
		name = "Find",
		f = { "<CMD>Telescope find_files<CR>", "Files" },
		g = { "<CMD>Telescope live_grep<CR>", "Grep" },
	},

	-- Minor mode: Surround
	s = {
		name = "Surround",
		a = { "<CMD><Plug>(sandwich-add)<CR>", "Add" },
		d = { "<CMD><Plug>(sandwich-delete)<CR>", "Delete" },
		r = { "<CMD><Plug>(sandwich-replace)<CR>", "Replace" },
	},

	-- Minor mode: TODO Folds

	-- Minor mode: Previous
	p = {
		name = "Previous",
		e = { "<cmd>silent lua vim.diagnostic.goto_prev()<cr>", "Error" },
		q = { "<cmd>cprevious<cr>", "quickfix item" },
		c = { "<cmd>lua require('gitsigns.actions').prev_hunk()<CR>", "Change" },
	},

	-- Minor mode: Next
	n = {
		name = "Next",
		e = { "<cmd>silent lua vim.diagnostic.goto_next()<cr>", "Error" },
		q = { "<cmd>cnext<cr>", "quickfix item" },
		c = { "<cmd>lua require('gitsigns.actions').next_hunk()<CR>", "Change" },
	},

	-- Minor mode: Run
	r = {
		name = "Run",
		y = { "<CMD>UltestLast<CR>", "Test / Last" },
		u = { "<CMD>UltestNearest<CR>", "Test / Nearest" },
		i = { "<CMD>Ultest<CR>", "Test" },
	},
}, {
	mode = "n",
	prefix = "<Space>",
	buffer = nil,
	silent = true,
	noremap = true,
	nowait = false,
})
