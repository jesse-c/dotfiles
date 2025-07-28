return {
  {
    "lewis6991/gitsigns.nvim",
    event = { "BufRead" },
    config = function()
      require("gitsigns").setup({
        signcolumn = true, -- Toggle with `:Gitsigns toggle_signs`
        numhl = false, -- Toggle with `:Gitsigns toggle_numhl`
        linehl = false, -- Toggle with `:Gitsigns toggle_linehl`
        word_diff = false, -- Toggle with `:Gitsigns toggle_word_diff`
        signs = {
          add = { text = "█" },
          change = { text = "█" },
          delete = { text = "█" },
          topdelete = { text = "█" },
          changedelete = { text = "█" },
          untracked = { text = "░" },
        },
        signs_staged_enable = true,
        signs_staged = {
          add = { text = "█" },
          change = { text = "█" },
          delete = { text = "█" },
          topdelete = { text = "█" },
          changedelete = { text = "█" },
        },
        watch_gitdir = {
          interval = 1000,
          follow_files = true,
        },
        attach_to_untracked = true,
        current_line_blame = false,
        update_debounce = 100,
      })
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
    cmd = { "Neogit" },
  },
}
