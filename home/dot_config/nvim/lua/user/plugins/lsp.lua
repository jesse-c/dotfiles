return {
  "neovim/nvim-lspconfig",
  {
    "williamboman/mason-lspconfig.nvim",
    dependencies = {
      "williamboman/mason.nvim",
      "neovim/nvim-lspconfig",
    },
    config = function()
      require("mason-lspconfig").setup({
        ensure_installed = {},
      })
    end,
  },
  -- vscode-like pictograms for neovim lsp completion items
  "onsails/lspkind.nvim",
  -- Use Neovim as a language server to inject LSP diagnostics, code actions, and more via Lua
  -- 🚦 A pretty diagnostics, references, telescope results, quickfix and location list to help you solve all the trouble your code is causing.
  {
    "folke/trouble.nvim",
    dependencies = "kyazdani42/nvim-web-devicons",
    config = function()
      require("trouble").setup()
    end,
  },
  -- Standalone UI for nvim-lsp progress. Eye candy for the impatient.
  {
    "j-hui/fidget.nvim",
    branch = "legacy",
    dependencies = "neovim/nvim-lspconfig",
    config = function()
      require("fidget").setup()
    end,
  },
}
