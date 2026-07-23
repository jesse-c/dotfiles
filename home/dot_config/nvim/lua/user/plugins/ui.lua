return {
  {
    "catppuccin/nvim",
    name = "catppuccin",
    lazy = false,
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
          blink_cmp = true,
          gitsigns = true,
          nvimtree = true,
          treesitter = true,
          notify = false,
          which_key = true,
          dropbar = {
            enabled = false,
            color_mode = true, -- enable color for kind's texts, not just kind's icons
          },
          neotest = true,
          lsp_trouble = true,
        },
      })
    end,
  },
  {
    "Bekaboo/dropbar.nvim",
    event = { "BufRead" },
    opts = {
      bar = {
        enable = function(buf, win, _)
          return vim.fn.winwidth(win) > 80 and vim.bo[buf].buftype == "" and vim.api.nvim_buf_get_name(buf) ~= ""
        end,
      },
      icons = {
        ui = {
          bar = {
            separator = " 󰁕 ",
          },
        },
      },
    },
  },
  {
    "folke/snacks.nvim",
    priority = 1000,
    lazy = false,
    opts = {
      indent = {
        enabled = true,
        char = "│",
        animate = {
          enabled = true,
          duration = {
            step = 10,
            total = 150,
          },
        },
        scope = {
          enabled = true,
          char = "│",
          animate = true,
        },
      },
      scroll = {
        enabled = true,
        animate = {
          duration = { step = 8, total = 120 },
          easing = "linear",
        },
      },
      notifier = {
        enabled = true,
        timeout = 3000,
      },
    },
    init = function()
      vim.api.nvim_create_autocmd("User", {
        pattern = "VeryLazy",
        callback = function()
          _G.dd = function(...)
            Snacks.debug.inspect(...)
          end
          vim.print = _G.dd
        end,
      })
    end,
  },
  {
    "nvim-lualine/lualine.nvim",
    dependencies = { "catppuccin/nvim" },
    config = function()
      require("lualine").setup({
        options = { theme = "catppuccin-mocha", globalstatus = true, section_separators = "", component_separators = "" },
        extensions = { "neo-tree", "lazy" },
      })
    end,
  },
  {
    "echasnovski/mini.icons",
    version = false,
    opts = {},
    config = function(_, opts)
      require("mini.icons").setup(opts)
      MiniIcons.mock_nvim_web_devicons()
    end,
  },
  {
    "nvim-neotest/nvim-nio",
  },
}
