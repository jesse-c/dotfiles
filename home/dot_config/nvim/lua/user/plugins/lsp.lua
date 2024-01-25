return {
  {
    "neovim/nvim-lspconfig",
    event = { "BufRead" },
    config = function()
      local lspconfig = require("lspconfig")
      lspconfig.lua_ls.setup({})
    end,
  },
}
