return {
  {
    "lewis6991/gitsigns.nvim",
    event = { "BufRead" },
    config = function()
      require("gitsigns").setup()
    end,
  },
  {
    "NeogitOrg/neogit",
    event = { "BufRead" },
    dependencies = {
      "nvim-lua/plenary.nvim",
      "sindrets/diffview.nvim",
      "nvim-telescope/telescope.nvim",
    },
    config = true,
  },
}
