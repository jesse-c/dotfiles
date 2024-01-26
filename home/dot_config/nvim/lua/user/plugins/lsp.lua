return {
  {
    "neovim/nvim-lspconfig",
    event = { "BufRead" },
    config = function()
      local lspconfig = require("lspconfig")
      lspconfig.lua_ls.setup({})
    end,
  },
  {
    "j-hui/fidget.nvim",
    opts = {},
    event = { "BufRead" },
  },
  {
    "folke/trouble.nvim",
    dependencies = { "nvim-tree/nvim-web-devicons", "neovim/nvim-lspconfig" },
    opts = {},
    event = { "BufRead" },
  },
}
