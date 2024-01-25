return {
  {
    "nvim-neotest/neotest",
    event = { "BufRead" },
    dependencies = {
      "nvim-lua/plenary.nvim",
      "antoinemadec/FixCursorHold.nvim",
      "nvim-treesitter/nvim-treesitter",
      -- Adapters
      "nvim-neotest/neotest-python",
      "nvim-neotest/neotest-go",
      "jfpedroza/neotest-elixir",
      "rouge8/neotest-rust",
    },
    config = function()
      require("neotest").setup({
        adapters = {
          require("neotest-python"),
          require("neotest-go"),
          require("neotest-elixir"),
          require("neotest-rust"),
        },
      })
    end,
  },
}
