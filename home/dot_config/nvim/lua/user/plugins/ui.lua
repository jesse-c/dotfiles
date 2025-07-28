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
    dependencies = { "nvim-tree/nvim-web-devicons", "catppuccin/nvim" },
    config = function()
      require("lualine").setup({
        options = {
          theme = "catppuccin",
          icons_enabled = true,
          component_separators = { left = "", right = "" },
          section_separators = { left = "", right = "" },
          disabled_filetypes = {
            statusline = {},
            winbar = {},
          },
          ignore_focus = {},
          always_divide_middle = true,
          refresh = {
            statusline = 1000,
            tabline = 1000,
            winbar = 1000,
          },
        },
        extensions = { "neo-tree", "lazy" },
        sections = {
          lualine_a = { "mode" },
          lualine_b = { "branch", "diff", "diagnostics" },
          lualine_c = { "filename" },
          lualine_x = {
            {
              function()
                local ft = vim.bo.filetype
                local clients = vim.lsp.get_clients({ bufnr = 0 })

                -- Expected LSP servers by filetype
                local expected_servers = {
                  clojure = "clojure_lsp",
                  elixir = "elixir_ls",
                  go = "gopls",
                  javascript = "denols",
                  typescript = "denols",
                  tex = "texlab",
                  latex = "texlab",
                  yaml = "yamlls",
                  swift = "sourcekit",
                  rust = "rust_analyzer",
                  python = "basedpyright",
                  markdown = "marksman",
                  typst = "tinymist",
                  toml = "taplo",
                  flix = "flix_lsp",
                  ["vespa-schema"] = "vespa_lsp",
                }

                local expected = expected_servers[ft]

                -- Check for connecting clients (not yet ready)
                local connecting_clients = 0
                for _, client in pairs(clients) do
                  if not client.server_capabilities then
                    connecting_clients = connecting_clients + 1
                  end
                end

                if connecting_clients > 0 then
                  return "󰔟" -- Connecting/loading
                elseif #clients > 0 then
                  return "󰌘 " .. #clients -- Connected + count
                elseif expected then
                  return "󰌙" -- Disconnected
                else
                  return "󰿦" -- Disabled/unavailable
                end
              end,
              color = function()
                local ft = vim.bo.filetype
                local clients = vim.lsp.get_clients({ bufnr = 0 })
                local expected_servers = {
                  clojure = "clojure_lsp",
                  elixir = "elixir_ls",
                  go = "gopls",
                  javascript = "denols",
                  typescript = "denols",
                  tex = "texlab",
                  latex = "texlab",
                  yaml = "yamlls",
                  swift = "sourcekit",
                  rust = "rust_analyzer",
                  python = "basedpyright",
                  markdown = "marksman",
                  typst = "tinymist",
                  toml = "taplo",
                  flix = "flix_lsp",
                  ["vespa-schema"] = "vespa_lsp",
                }
                local expected = expected_servers[ft]

                -- Check for connecting clients
                local connecting_clients = 0
                for _, client in pairs(clients) do
                  if not client.server_capabilities then
                    connecting_clients = connecting_clients + 1
                  end
                end

                if connecting_clients > 0 then
                  return { fg = "#f5a97f" } -- catppuccin peach for connecting
                elseif #clients > 0 then
                  return { fg = "#a6da95" } -- catppuccin green for connected
                elseif expected then
                  return { fg = "#ed8796" } -- catppuccin red for disconnected
                else
                  return { fg = "#6e738d" } -- catppuccin overlay1 (dim) for no LSP
                end
              end,
            },
            {
              function()
                local encoding = vim.bo.fileencoding or vim.bo.encoding
                if encoding == "utf-8" then
                  return "󰉿"
                elseif encoding == "ascii" then
                  return "󰌪"
                elseif encoding == "latin1" then
                  return "󰉾"
                else
                  return encoding -- fallback to text for unknown encodings
                end
              end,
            },
            "fileformat",
            {
              function()
                local ft = vim.bo.filetype
                if ft == "" then
                  return ""
                end

                -- Get the icon from nvim-web-devicons
                local devicons = require("nvim-web-devicons")
                local icon = devicons.get_icon_by_filetype(ft)

                -- Return just the icon if available, otherwise fallback to filetype name
                return icon or ft
              end,
            },
          },
          lualine_y = { "progress" },
          lualine_z = { "location" },
        },
        inactive_sections = {
          lualine_a = {},
          lualine_b = {},
          lualine_c = { "filename" },
          lualine_x = { "location" },
          lualine_y = {},
          lualine_z = {},
        },
        winbar = {},
        inactive_winbar = {},
      })
    end,
  },
  {
    "nvim-neotest/nvim-nio",
  },
}
