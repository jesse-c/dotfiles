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
  {
    "tpope/vim-eunuch",
    event = { "BufRead" },
    cmd = {
      "Remove",
      "Delete",
      "Move",
      "Rename",
      "Copy",
      "Duplicate",
      "Chmod",
      "Mkdir",
      "SudoWrite",
      "SudoEdit",
    },
  },
}
