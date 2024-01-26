return {
  {
    "nvim-tree/nvim-tree.lua",
    event = { "BufRead" },
    version = "*",
    dependencies = {
      "nvim-tree/nvim-web-devicons",
    },
    opts = {},
    cmd = { "NvimTreeToggle", "NvimTreeFocus" },
    config = function(_, opts)
      require("nvim-tree").setup(opts)
    end,
  },
}
