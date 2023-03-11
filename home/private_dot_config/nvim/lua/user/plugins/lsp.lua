return {
    "neovim/nvim-lspconfig", {
        "williamboman/mason-lspconfig.nvim",
        dependencies = {
            "williamboman/mason.nvim", "neovim/nvim-lspconfig",
            "stevearc/aerial.nvim"
        },
        config = function()
            require("mason-lspconfig").setup({
                ensure_installed = {
                    "bashls", "clojure_lsp", "elixirls", "erlangls", "html",
                    "gopls", "jsonls", "lemminx", "ltex", "prosemd_lsp",
                    "pylsp", "rust_analyzer", "solargraph", "sorbet",
                    -- "sourcekit",
                    "sqls", "lua_ls", "tsserver", "yamlls"
                }
            })
        end
    }, -- vscode-like pictograms for neovim lsp completion items
    "onsails/lspkind.nvim",
    -- Use Neovim as a language server to inject LSP diagnostics, code actions, and more via Lua
    {
        "jose-elias-alvarez/null-ls.nvim",
        dependencies = {"nvim-lua/plenary.nvim", "lewis6991/gitsigns.nvim"},
        event = "VimEnter"
    },
    -- 🚦 A pretty diagnostics, references, telescope results, quickfix and location list to help you solve all the trouble your code is causing.
    {
        "folke/trouble.nvim",
        dependencies = "kyazdani42/nvim-web-devicons",
        config = function() require("trouble").setup() end
    }, -- Neovim plugin for a code outline window
    {
        "stevearc/aerial.nvim",
        dependencies = "stevearc/stickybuf.nvim",
        config = function() require("aerial").setup() end
    }, -- Standalone UI for nvim-lsp progress. Eye candy for the impatient.
    {
        "j-hui/fidget.nvim",
        dependencies = "neovim/nvim-lspconfig",
        config = function() require("fidget").setup() end
    }
}
