return {
  {
    "nvim-neo-tree/neo-tree.nvim",
    branch = "v3.x",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "MunifTanjim/nui.nvim",
    },
    cmd = "Neotree",
    opts = {
      filesystem = {
        follow_current_file = { enabled = true },
        hijack_netrw_behavior = "open_default",
      },
    },
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
