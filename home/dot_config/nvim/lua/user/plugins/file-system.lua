return {
  {
    "nvim-neo-tree/neo-tree.nvim",
    branch = "v3.x",
    event = { "BufRead" },
    cmd = "Neotree",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "nvim-tree/nvim-web-devicons",
      "MunifTanjim/nui.nvim",
      "3rd/image.nvim",
    },
  },
}
