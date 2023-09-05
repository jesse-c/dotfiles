-- Diagnostics
-- https://www.reddit.com/r/neovim/comments/ru871v/how_to_change_the_position_of_linting_in/
-- Disable hover since it's redundant due to lsp_lines.
-- vim.cmd([[autocmd CursorHold,CursorHoldI * lua vim.diagnostic.open_float(nil, {focus=false})]])
vim.diagnostic.config({
    -- Disable virtual_text since it's redundant due to lsp_lines.
    virtual_text = false
    -- virtual_text = {
    -- 	prefix = "●", -- Could be '■', '●', '▎', 'x'
    -- },
})

-- Symbols
-- https://github.com/neovim/nvim-lspconfig/wiki/UI-customization#change-diagnostic-symbols-in-the-sign-column-gutter
local signs = {Error = " ", Warn = " ", Hint = " ", Info = " "}
for type, icon in pairs(signs) do
    local hl = "DiagnosticSign" .. type
    vim.fn.sign_define(hl, {text = icon, texthl = hl, numhl = hl})
end

local lspconfig = require("lspconfig")

-- local capabilities = require('cmp_nvim_lsp').default_capabilities()

local opts = {}

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
elixirls_opts["cmd"] = {"elixir-ls"}

local sumenko_opts = shallowcopy(opts)
sumenko_opts["cmd"] = {"lua-language-server"}
sumenko_opts["settings"] = {
    -- Get the language server to recognize the `vim` global
    -- https://www.reddit.com/r/neovim/comments/khk335/lua_configuration_global_vim_is_undefined/
    -- https://neovim.discourse.group/t/how-to-suppress-warning-undefined-global-vim/1882/8
    Lua = {
        completion = {enable = true},
        diagnostics = {enable = true, globals = {"vim", "hs"}},
        workspace = {
            library = {
                ['/Applications/Hammerspoon.app/Contents/Resources/extensions/hs/'] = true,
                [vim.fn.expand('$VIMRUNTIME/lua')] = true,
                [vim.fn.expand('$VIMRUNTIME/lua/vim/lsp')] = true,
            }
        }
    }
}

local pylsp_opts = shallowcopy(opts)
pylsp_opts["cmd"] = {"pylsp"}

local tsserver_opts = shallowcopy(opts)
tsserver_opts["cmd"] = {"typescript-language-server", "--stdio"}

local bashls_opts = shallowcopy(opts)
bashls_opts["cmd"] = {"bash-language-server", "start"}

local sqlls_opts = shallowcopy(opts)
sqlls_opts["on_attach"] = function(client, bufnr)
    -- This is currently overriding the default on_attach function
    require("sqlls").on_attach(client, bufnr)
end

local prosemd_lsp_opts = shallowcopy(opts)
prosemd_lsp_opts["cmd"] = {"prosemd-lsp", "--stdio"}

local ltex_lsp_opts = shallowcopy(opts)
ltex_lsp_opts["cmd"] = {"ltex-ls"}

local jsonls_opts = shallowcopy(opts)
jsonls_opts["cmd"] = {"vscode-json-language-server", "--stdio"}

local html_opts = shallowcopy(opts)
html_opts["cmd"] = {"vscode-html-language-server", "--stdio"}

local yamlls_opts = shallowcopy(opts)
yamlls_opts["cmd"] = {"yaml-language-server", "--stdio"}

local solargraph_opts = shallowcopy(opts)
solargraph_opts["cmd"] = {"solargraph", "stdio"}

local sorbet_opts = shallowcopy(opts)
sorbet_opts["cmd"] = {
    "srb", "tc"
    -- lsp",
}

local lemminx_opts = shallowcopy(opts)
lemminx_opts["cmd"] = {"lemminx"}

local clojure_lsp_opts = shallowcopy(opts)
clojure_lsp_opts["cmd"] = {"clojure-lsp"}

local gopls_opts = shallowcopy(opts)
gopls_opts["cmd"] = {"gopls"}

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
lspconfig.sqlls.setup(sqlls_opts)
lspconfig.lua_ls.setup(sumenko_opts)
lspconfig.tsserver.setup(tsserver_opts)
lspconfig.yamlls.setup(yamlls_opts)
