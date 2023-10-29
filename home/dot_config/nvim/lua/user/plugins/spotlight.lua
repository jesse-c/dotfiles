return {
  {
    "nvim-telescope/telescope.nvim",
    dependencies = "nvim-telescope/telescope-fzf-native.nvim",
  },
  { "nvim-telescope/telescope-fzf-native.nvim", build = "make" },
  {
    "jvgrootveld/telescope-zoxide",
    dependencies = "nvim-telescope/telescope.nvim",
  },
  {
    "gbrlsnchs/telescope-lsp-handlers.nvim",
    dependencies = "nvim-telescope/telescope.nvim",
  },
  {
    "nvim-telescope/telescope-file-browser.nvim",
    dependencies = "nvim-telescope/telescope.nvim",
  },
}
