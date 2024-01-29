return {
  {
    "catppuccin/nvim",
    name = "catppuccin",
    event = { "BufRead" },
    priority = 1000,
    config = function()
      require("catppuccin").setup({
        flavour = "mocha", -- latte, frappe, macchiato, mocha
        background = { -- :h background
          light = "latte",
          dark = "mocha",
        },
        integrations = {
          alpha = true,
          neogit = true,
          neotest = false,
          cmp = true,
          gitsigns = true,
          nvimtree = true,
          treesitter = true,
          notify = true,
          treesitter = true,
          which_key = true,
          dropbar = {
            enabled = false,
            color_mode = true, -- enable color for kind's texts, not just kind's icons
          },
          fidget = true,
          neotest = true,
          lsp_trouble = true,
        },
      })
    end,
  },
  {
    "Bekaboo/dropbar.nvim",
    event = { "BufRead" },
  },
  {
    "lukas-reineke/indent-blankline.nvim",
    main = "ibl",
    opts = {},
  },
  {
    "freddiehaddad/feline.nvim",
    opts = {},
    config = function(_, opts)
      require("feline").setup()
      require("feline").winbar.setup()
      require("feline").statuscolumn.setup()
    end,
  },
}
