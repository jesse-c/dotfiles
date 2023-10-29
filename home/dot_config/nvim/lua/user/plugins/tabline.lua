return {
  -- A snazzy bufferline for Neovim
  {
    "akinsho/bufferline.nvim",
    branch = "main",
    dependencies = "kyazdani42/nvim-web-devicons",
    config = function()
      require("bufferline").setup({
        options = { mode = "tabs", numbers = "ordinal" },
      })
    end,
  },
}
