-- Diagnostics
-- https://www.reddit.com/r/neovim/comments/ru871v/how_to_change_the_position_of_linting_in/
-- Disable hover since it's redundant due to lsp_lines.
-- vim.cmd([[autocmd CursorHold,CursorHoldI * lua vim.diagnostic.open_float(nil, {focus=false})]])
vim.diagnostic.config({
	-- Disable virtual_text since it's redundant due to lsp_lines.
	virtual_text = false,
	-- virtual_text = {
	-- 	prefix = "●", -- Could be '■', '●', '▎', 'x'
	-- },
})

-- Symbols
-- https://github.com/neovim/nvim-lspconfig/wiki/UI-customization#change-diagnostic-symbols-in-the-sign-column-gutter
local signs = { Error = " ", Warn = " ", Hint = " ", Info = " " }
for type, icon in pairs(signs) do
	local hl = "DiagnosticSign" .. type
	vim.fn.sign_define(hl, { text = icon, texthl = hl, numhl = hl })
end

local ok, _ = pcall(require, "nvim-lsp-installer")
if not ok then
	return
end

local lspconfig = require("lspconfig")

local opts = {}

local has_aerial, aerial = pcall(require, "aerial")
if has_aerial then
	local on_attach = function(client, bufnr)
		aerial.on_attach(client, bufnr)

		-- Code lens
		if client.server_capabilities.code_lens then
			-- Do an initial refresh
			vim.lsp.codelens.refresh()

			-- Setup auto-refreshing

			-- https://github.com/hydeik/dotfiles/blob/022763feec5aa50383474f161dbe7cc111d22bd5/private_dot_config/nvim/lua/rc/config/lsp/code_lens.lua
			vim.api.nvim_create_augroup("ConfigLspCodeLens", { clear = true })

			vim.api.nvim_create_autocmd({ "BufEnter", "BufWritePost", "CursorHold", "InsertLeave", "TextChanged" }, {
				group = "ConfigLspCodeLens",
				buffer = bufnr,
				callback = function()
					vim.lsp.codelens.refresh()
				end,
				desc = "[LSP] Refresh the codelens for the current buffer.",
			})
		end
	end

	opts = {
		on_attach = on_attach,
	}
end

-- Source: http://lua-users.org/wiki/CopyTable (22-05-12)
local function shallowcopy(orig)
	local orig_type = type(orig)
	local copy
	if orig_type == "table" then
		copy = {}
		for orig_key, orig_value in pairs(orig) do
			copy[orig_key] = orig_value
		end
	else -- number, string, boolean, etc
		copy = orig
	end
	return copy
end

local elixirls_opts = shallowcopy(opts)
elixirls_opts["cmd"] = { "/Users/jesse/.local/share/nvim/lsp_servers/elixirls/elixir-ls/language_server.sh" }

local sumenko_opts = shallowcopy(opts)
sumenko_opts["cmd"] = {
	"/Users/jesse/.local/share/nvim/lsp_servers/sumneko_lua/extension/server/bin/lua-language-server",
}
sumenko_opts["settings"] = {
	-- Get the language server to recognize the `vim` global
	-- https://www.reddit.com/r/neovim/comments/khk335/lua_configuration_global_vim_is_undefined/
	-- https://neovim.discourse.group/t/how-to-suppress-warning-undefined-global-vim/1882/8
	Lua = {
		diagnostics = {
			globals = { "vim" },
		},
	},
}

local pylsp_opts = shallowcopy(opts)
pylsp_opts["cmd"] = { "/Users/jesse/.local/share/nvim/lsp_servers/pylsp/venv/bin/pylsp" }

local tsserver_opts = shallowcopy(opts)
tsserver_opts["cmd"] = {
	"/Users/jesse/.local/share/nvim/lsp_servers/tsserver/node_modules/.bin/typescript-language-server",
	"--stdio",
}

local bashls_opts = shallowcopy(opts)
bashls_opts["cmd"] = {
	"/Users/jesse/.local/share/nvim/lsp_servers/bashls/node_modules/.bin/bash-language-server",
	"start",
}

local sqls_opts = shallowcopy(opts)
sqls_opts["cmd"] = { "/Users/jesse/.local/share/nvim/lsp_servers/sqls/sqls" }

local prosemd_lsp_opts = shallowcopy(opts)
prosemd_lsp_opts["cmd"] = { "/Users/jesse/.local/share/nvim/lsp_servers/prosemd_lsp/prosemd-lsp" }

local ltex_lsp_opts = shallowcopy(opts)
ltex_lsp_opts["cmd"] = { "/Users/jesse/.local/share/nvim/lsp_servers/ltex/ltex-ls/bin/ltex-ls" }

local jsonls_opts = shallowcopy(opts)
jsonls_opts["cmd"] = {
	"/Users/jesse/.local/share/nvim/lsp_servers/jsonls/node_modules/.bin/vscode-json-language-server",
	"--stdio",
}

local html_opts = shallowcopy(opts)
html_opts["cmd"] = {
	"/Users/jesse/.local/share/nvim/lsp_servers/html/node_modules/vscode-langservers-extracted/bin/vscode-html-language-server",
	"--stdio",
}

local yamlls_opts = shallowcopy(opts)
yamlls_opts["cmd"] = {
	"/Users/jesse/.local/share/nvim/lsp_servers/yamlls/node_modules/.bin/yaml-language-server",
	"--stdio",
}

local solargraph_opts = shallowcopy(opts)
solargraph_opts["cmd"] = {
	"/Users/jesse/.local/share/nvim/lsp_servers/solargraph/bin/solargraph",
	"stdio",
}

local sorbet_opts = shallowcopy(opts)
sorbet_opts["cmd"] = {
	"/Users/jesse/.local/share/nvim/lsp_servers/sorbet/bin/srb",
	"tc",
	--lsp",
}

local lemminx_opts = shallowcopy(opts)
lemminx_opts["cmd"] = {
	"/Users/jesse/.local/share/nvim/lsp_servers/lemminx/lemminx",
}

local clojure_lsp_opts = shallowcopy(opts)
clojure_lsp_opts["cmd"] = {
	"/Users/jesse/.local/share/nvim/lsp_servers/clojure_lsp/clojure-lsp",
}

local gopls_opts = shallowcopy(opts)
gopls_opts["cmd"] = {
	"/Users/jesse/.local/share/nvim/lsp_servers/gopls/clojure-lsp",
}

lspconfig.bashls.setup(bashls_opts)
lspconfig.clojure_lsp.setup(clojure_lsp_opts)
lspconfig.erlangls.setup(opts)
lspconfig.elixirls.setup(elixirls_opts)
lspconfig.gopls.setup(gopls_opts)
lspconfig.html.setup(html_opts)
lspconfig.jsonls.setup(jsonls_opts)
lspconfig.lemminx.setup(lemminx_opts)
lspconfig.ltex.setup(ltex_lsp_opts)
lspconfig.pylsp.setup(pylsp_opts)
lspconfig.rust_analyzer.setup(opts)
lspconfig.sourcekit.setup(opts)
lspconfig.prosemd_lsp.setup(prosemd_lsp_opts)
lspconfig.sorbet.setup(sorbet_opts)
lspconfig.solargraph.setup(solargraph_opts)
lspconfig.sqls.setup(sqls_opts)
lspconfig.sumneko_lua.setup(sumenko_opts)
lspconfig.tsserver.setup(tsserver_opts)
lspconfig.yamlls.setup(yamlls_opts)

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
		null_ls.builtins.diagnostics.vale,
		-- Code actions
		null_ls.builtins.code_actions.gitsigns,
		null_ls.builtins.code_actions.proselint,
		null_ls.builtins.code_actions.shellcheck,
		-- Formatters
		null_ls.builtins.formatting.prettier,
		null_ls.builtins.formatting.joker,
		null_ls.builtins.formatting.stylua,
		null_ls.builtins.formatting.swiftformat,
		null_ls.builtins.formatting.black,
		null_ls.builtins.formatting.isort,
		null_ls.builtins.formatting.erlfmt,
		null_ls.builtins.formatting.fish_indent,
		null_ls.builtins.formatting.fnlfmt,
		null_ls.builtins.formatting.shfmt,
		null_ls.builtins.formatting.shellharden,
	},
})
