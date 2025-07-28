return {
  -- LSP servers and clients communicate what features they support through "capabilities".
  --  By default, Neovim support a subset of the LSP specification.
  --  With blink.cmp, Neovim has *more* capabilities which are communicated to the LSP servers.
  --  Explanation from TJ: https://youtu.be/m8C0Cq9Uv9o?t=1275
  --
  -- This can vary by config, but in general for nvim-lspconfig:
  {
    "neovim/nvim-lspconfig",
    dependencies = { "saghen/blink.cmp" },
    config = function(_, opts)
      local lspconfig = require("lspconfig")

      -- Setup LSP attach autocmd for keymaps and other settings
      vim.api.nvim_create_autocmd("LspAttach", {
        desc = "LSP actions",
        callback = function(event)
          local opts_keymap = { buffer = event.buf }
          vim.keymap.set("n", "K", vim.lsp.buf.hover, opts_keymap)
          vim.keymap.set("n", "gd", vim.lsp.buf.definition, opts_keymap)
          vim.keymap.set("n", "gD", vim.lsp.buf.declaration, opts_keymap)
          vim.keymap.set("n", "gi", vim.lsp.buf.implementation, opts_keymap)
          vim.keymap.set("n", "go", vim.lsp.buf.type_definition, opts_keymap)
          vim.keymap.set("n", "gs", vim.lsp.buf.signature_help, opts_keymap)
          vim.keymap.set("n", "<F2>", vim.lsp.buf.rename, opts_keymap)
          vim.keymap.set({ "n", "x" }, "<F3>", vim.lsp.buf.format, opts_keymap)
          vim.keymap.set("n", "<F4>", vim.lsp.buf.code_action, opts_keymap)

          -- Enable inlay hints if supported
          local client = vim.lsp.get_client_by_id(event.data.client_id)
          if client and client.supports_method("textDocument/inlayHint") then
            vim.lsp.inlay_hint.enable(true, { bufnr = event.buf })
          end

          -- Enable and refresh code lens if supported
          if client and client.supports_method("textDocument/codeLens") then
            vim.lsp.codelens.refresh()
            vim.keymap.set("n", "<leader>cr", vim.lsp.codelens.run, opts_keymap)
          end
        end,
      })

      -- Configure LSP handlers with better styling
      vim.lsp.handlers["textDocument/hover"] = vim.lsp.with(vim.lsp.handlers.hover, {
        border = "rounded",
      })

      vim.lsp.handlers["textDocument/signatureHelp"] = vim.lsp.with(vim.lsp.handlers.signature_help, {
        border = "rounded",
      })

      -- Configure diagnostics with modern sign approach and styling
      vim.diagnostic.config({
        virtual_text = {
          spacing = 2,
          prefix = "●",
          suffix = "",
          format = function(diagnostic)
            -- Show full message on current line, truncate others
            local current_line = vim.fn.line(".") - 1 -- Convert to 0-based
            local message = diagnostic.message

            if diagnostic.lnum ~= current_line and #message > 50 then
              message = message:sub(1, 47) .. "..."
            end
            return message
          end,
        },
        signs = {
          text = {
            [vim.diagnostic.severity.ERROR] = "󰅚",
            [vim.diagnostic.severity.WARN] = "󰀪",
            [vim.diagnostic.severity.HINT] = "󰌶",
            [vim.diagnostic.severity.INFO] = "󰋽",
          },
        },
        underline = true,
        update_in_insert = false,
        severity_sort = true,
        float = {
          border = "rounded",
          source = "always",
          header = "",
          prefix = "",
        },
      })

      -- Make virtual text more subtle with custom highlight
      vim.api.nvim_set_hl(0, "DiagnosticVirtualTextError", { fg = "#ed8796", italic = true, blend = 20 })
      vim.api.nvim_set_hl(0, "DiagnosticVirtualTextWarn", { fg = "#f5a97f", italic = true, blend = 20 })
      vim.api.nvim_set_hl(0, "DiagnosticVirtualTextInfo", { fg = "#8aadf4", italic = true, blend = 20 })
      vim.api.nvim_set_hl(0, "DiagnosticVirtualTextHint", { fg = "#8bd5ca", italic = true, blend = 20 })

      -- Refresh diagnostics on cursor movement to update truncation
      vim.api.nvim_create_autocmd("CursorMoved", {
        callback = function()
          vim.diagnostic.show(nil, 0, nil, { virtual_text = true })
        end,
      })

      -- Configure LSP servers with explicit commands
      local capabilities = require("blink.cmp").get_lsp_capabilities()

      -- Define filetypes for each server
      local server_filetypes = {
        clojure_lsp = { "clojure" },
        elixir_ls = { "elixir" },
        gopls = { "go" },
        denols = { "javascript", "typescript" },
        texlab = { "tex", "latex" },
        yamlls = { "yaml" },
        sourcekit = { "swift" },
        rust_analyzer = { "rust" },
        basedpyright = { "python" },
        marksman = { "markdown" },
        tinymist = { "typst" },
        taplo = { "toml" },
      }

      -- Configure each server
      for server_name, config in pairs(opts.servers or {}) do
        local server_config = vim.tbl_deep_extend("force", {
          capabilities = capabilities,
          filetypes = server_filetypes[server_name],
        }, config)

        vim.lsp.config(server_name, server_config)
        vim.lsp.enable(server_name)
      end

      -- Custom LSP configurations (not in nvim-lspconfig)
      local lsp_util = require("lspconfig.util")
      local custom_capabilities = require("blink.cmp").get_lsp_capabilities()

      -- Flix LSP (matches Emacs Eglot config)
      local flix_config = {
        name = "flix_lsp",
        cmd = { "flix", "lsp" },
        filetypes = { "flix" },
        root_dir = lsp_util.find_git_ancestor,
        capabilities = custom_capabilities,
      }
      vim.api.nvim_create_autocmd("FileType", {
        pattern = "flix",
        callback = function()
          vim.lsp.start(flix_config)
        end,
      })

      -- Vespa Schema LSP (matches Emacs Eglot config)
      local vespa_jar_path = vim.fn.expand("~/.local/bin/vespa-language-server.jar")
      if vim.fn.filereadable(vespa_jar_path) == 1 then
        local vespa_config = {
          name = "vespa_lsp",
          cmd = { "java", "-jar", vespa_jar_path },
          filetypes = { "vespa-schema" },
          root_dir = lsp_util.find_git_ancestor,
          capabilities = custom_capabilities,
        }
        vim.api.nvim_create_autocmd("FileType", {
          pattern = "vespa-schema",
          callback = function()
            vim.lsp.start(vespa_config)
          end,
        })
      end
    end,
    opts = {
      servers = {
        -- Languages from your Emacs Eglot config (exact commands)
        clojure_lsp = {
          cmd = { "clojure-lsp" },
        },
        elixir_ls = {
          cmd = { "elixir-ls" },
          root_markers = { "mix.exs", ".git" },
        },
        gopls = {
          cmd = { "gopls" },
        },
        denols = {
          cmd = { "deno", "lsp" },
        },
        texlab = {
          cmd = { "texlab" },
        },
        yamlls = {
          cmd = { "yaml-language-server", "--stdio" },
        },
        sourcekit = {
          cmd = { "sourcekit-lsp" },
        },
        rust_analyzer = {
          cmd = { "rust-analyzer" },
        },
        basedpyright = {
          cmd = { "basedpyright-langserver", "--stdio" },
          settings = {
            basedpyright = {
              analysis = {
                autoSearchPaths = true,
                useLibraryCodeForTypes = true,
                diagnosticMode = "workspace",
              },
            },
          },
        },
        marksman = {
          cmd = { "marksman" },
        },
        tinymist = {
          cmd = { "tinymist", "lsp" },
        },
        taplo = {
          cmd = { "taplo", "lsp", "stdio" },
        },
      },
    },
  },
  {
    "folke/trouble.nvim",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    opts = {
      focus = false,
      auto_close = false,
      auto_open = false,
      auto_preview = true,
      auto_refresh = true,
      auto_jump = false,
      restore = true,
      follow = true,
      indent_guides = true,
      max_items = 200,
      multiline = true,
      pinned = false,
      preview = {
        type = "split",
        relative = "win",
        position = "right",
        size = 0.3,
      },
      icons = {
        indent = {
          middle = " ",
          last = " ",
          top = " ",
          ws = "│  ",
        },
      },
      modes = {
        diagnostics = {
          groups = {
            { "filename", format = "{file_icon} {basename:Title} {count}" },
          },
        },
      },
      throttle = {
        refresh = 20,
        update = 10,
        render = 10,
        follow = 100,
        preview = { ms = 100, debounce = true },
      },
      keys = {
        ["?"] = "help",
        r = "refresh",
        R = "toggle_refresh",
        q = "close",
        o = "jump_close",
        ["<esc>"] = "cancel",
        ["<cr>"] = "jump",
        ["<2-leftmouse>"] = "jump",
        ["<c-s>"] = "jump_split",
        ["<c-v>"] = "jump_vsplit",
        ["}"] = "next",
        ["{{"] = "prev",
        dd = "delete",
        d = { action = "delete", mode = "v" },
        i = "inspect",
        p = "preview",
        P = "toggle_preview",
        zo = "fold_open",
        zO = "fold_open_recursive",
        zc = "fold_close",
        zC = "fold_close_recursive",
        za = "fold_toggle",
        zA = "fold_toggle_recursive",
        zm = "fold_more",
        zM = "fold_close_all",
        zr = "fold_reduce",
        zR = "fold_open_all",
        zx = "fold_update",
        zX = "fold_update_all",
        zn = "fold_disable",
        zN = "fold_enable",
        zi = "fold_toggle_enable",
        gb = { action = "toggle_fold", desc = "Toggle Fold" },
      },
    },
    cmd = "Trouble",
    keys = {
      {
        "<leader>xx",
        "<cmd>Trouble diagnostics toggle<cr>",
        desc = "Diagnostics (Trouble)",
      },
      {
        "<leader>xX",
        "<cmd>Trouble diagnostics toggle filter.buf=0<cr>",
        desc = "Buffer Diagnostics (Trouble)",
      },
      {
        "<leader>cs",
        "<cmd>Trouble symbols toggle focus=false<cr>",
        desc = "Symbols (Trouble)",
      },
      {
        "<leader>cl",
        "<cmd>Trouble lsp toggle focus=false win.position=right<cr>",
        desc = "LSP Definitions / references / ... (Trouble)",
      },
      {
        "<leader>xL",
        "<cmd>Trouble loclist toggle<cr>",
        desc = "Location List (Trouble)",
      },
      {
        "<leader>xQ",
        "<cmd>Trouble qflist toggle<cr>",
        desc = "Quickfix List (Trouble)",
      },
    },
  },
}
