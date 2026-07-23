return {
  {
    -- Tip: ys<motion><char> to add, cs<old><new> to change, ds<char> to delete
    --      e.g. ysiw` wraps word in backticks, cs`' swaps them, ds` removes
    "kylechui/nvim-surround",
    event = "VeryLazy",
    opts = {},
  },
  {
    "gregorias/nvim-surround-wk",
    dependencies = { "kylechui/nvim-surround", "folke/which-key.nvim" },
    config = true,
  },
  {
    -- Tip: searches TODO/FIXME/HACK/NOTE/PERF across the project; <leader>st to list with Telescope
    "folke/todo-comments.nvim",
    dependencies = { "nvim-lua/plenary.nvim" },
    event = "VeryLazy",
    opts = {},
  },
  {
    "windwp/nvim-autopairs",
    event = "InsertEnter",
    opts = {}, -- this is equalent to setup({}) function
  },
  {
    "eraserhd/parinfer-rust",
    event = "InsertEnter",
    build = "cargo build --release",
  },
}
