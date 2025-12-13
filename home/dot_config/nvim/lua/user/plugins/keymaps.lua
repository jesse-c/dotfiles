return {
  {
    "folke/which-key.nvim",
    event = "VeryLazy",
    init = function()
      vim.o.timeout = true
      vim.o.timeoutlen = 300
    end,
    opts = {
      spec = {
        -- Tree-sitter text objects
        { "af", desc = "Around function" },
        { "if", desc = "Inside function" },
        { "ac", desc = "Around class" },
        { "ic", desc = "Inside class" },
        { "aa", desc = "Around parameter" },
        { "ia", desc = "Inside parameter" },
        { "ab", desc = "Around block" },
        { "ib", desc = "Inside block" },

        -- Tree-sitter navigation
        { "]m", desc = "Next function start" },
        { "[m", desc = "Previous function start" },
        { "]M", desc = "Next function end" },
        { "[M", desc = "Previous function end" },
        { "]]", desc = "Next class start" },
        { "[[", desc = "Previous class start" },
        { "][", desc = "Next class end" },
        { "[]", desc = "Previous class end" },

        -- Tree-sitter incremental selection
        { "gnn", desc = "Init selection" },
        { "grn", desc = "Increment node" },
        { "grc", desc = "Increment scope" },
        { "grm", desc = "Decrement node" },

        -- LSP mappings
        { "K", desc = "Hover documentation" },
        { "gd", desc = "Go to definition" },
        { "gD", desc = "Go to declaration" },
        { "gi", desc = "Go to implementation" },
        { "go", desc = "Go to type definition" },
        { "gs", desc = "Signature help" },
        { "<F2>", desc = "Rename symbol" },
        { "<F3>", desc = "Format code" },
        { "<F4>", desc = "Code actions" },

        -- Code lens
        { "<leader>cr", desc = "Run code lens" },

        -- Trouble mappings
        { "<leader>xx", desc = "Diagnostics (Trouble)" },
        { "<leader>xX", desc = "Buffer Diagnostics (Trouble)" },
        { "<leader>cs", desc = "Symbols (Trouble)" },
        { "<leader>cl", desc = "LSP Definitions (Trouble)" },
        { "<leader>xL", desc = "Location List (Trouble)" },
        { "<leader>xQ", desc = "Quickfix List (Trouble)" },

        -- Parameter swapping
        { "<leader>a", desc = "Swap parameter next" },
        { "<leader>A", desc = "Swap parameter prev" },

        -- Project menu (matching Emacs s-p)
        { "<D-p>", group = "project" },
        { "<D-b>", "<cmd>Telescope buffers<cr>", desc = "Buffers" },
        { "<D-p>s", "<cmd>Telescope live_grep<cr>", desc = "Search (ripgrep)" },
        { "<D-p>f", "<cmd>Telescope find_files<cr>", desc = "Files" },
        { "<D-p>l", "<cmd>Telescope current_buffer_fuzzy_find<cr>", desc = "Line (current buffer)" },
        { "<D-p>d", "<cmd>NvimTreeToggle<cr>", desc = "Dired (file explorer)" },

        -- Embark-like DWIM (Do What I Mean) - C-; equivalent
        {
          "<C-;>",
          function()
            local clients = vim.lsp.get_clients({ bufnr = 0 })

            -- If LSP is available and cursor is on a symbol, go to definition
            if #clients > 0 then
              local word = vim.fn.expand("<cword>")
              if word ~= "" and word:match("[%a_][%w_]*") then
                vim.lsp.buf.definition()
                return
              end
            end

            -- If in visual mode, search for selection
            local mode = vim.fn.mode()
            if mode == "v" or mode == "V" then
              vim.cmd("normal! y")
              local selection = vim.fn.getreg('"')
              vim.cmd("Telescope live_grep default_text=" .. vim.fn.shellescape(selection))
              return
            end

            -- If on a URL, open it
            local line = vim.fn.getline(".")
            local col = vim.fn.col(".")
            local url = line:match("https?://[%w%.%-_/:?#@!$&'()*+,;=]+")
            if url then
              vim.fn.system("open " .. vim.fn.shellescape(url))
              return
            end

            -- If on a file path, open it
            local word = vim.fn.expand("<cfile>")
            if vim.fn.filereadable(word) == 1 then
              vim.cmd("edit " .. word)
              return
            end

            -- Default: search for word under cursor
            local cword = vim.fn.expand("<cword>")
            if cword ~= "" then
              vim.cmd("Telescope live_grep default_text=" .. cword)
            else
              vim.cmd("Telescope live_grep")
            end
          end,
          desc = "DWIM (Embark-like)",
        },

        -- LSP transient menu (matching Emacs eglot-transient-menu)
        { "<D-l>", group = "lsp" },

        -- Navigation
        { "<D-l>.", "<cmd>lua vim.lsp.buf.definition()<cr>", desc = "Find definitions" },
        { "<D-l>,", "<C-o>", desc = "Pop back" },
        { "<D-l>/", "<cmd>lua vim.lsp.buf.references()<cr>", desc = "Find references" },
        { "<D-l>i", "<cmd>lua vim.lsp.buf.implementation()<cr>", desc = "Find implementations" },
        { "<D-l>t", "<cmd>lua vim.lsp.buf.type_definition()<cr>", desc = "Find type definition" },
        { "<D-l>D", "<cmd>lua vim.lsp.buf.declaration()<cr>", desc = "Find declaration" },

        -- Code actions
        { "<D-l>a", "<cmd>lua vim.lsp.buf.code_action()<cr>", desc = "Code actions" },
        { "<D-l>f", "<cmd>lua vim.lsp.buf.format()<cr>", desc = "Format buffer" },
        { "<D-l>F", "<cmd>lua vim.lsp.buf.format()<cr>", desc = "Format region", mode = "v" },
        { "<D-l>R", "<cmd>lua vim.lsp.buf.rename()<cr>", desc = "Rename symbol" },

        -- Diagnostics
        { "<D-l>d", "<cmd>lua vim.diagnostic.open_float()<cr>", desc = "Show diagnostics" },
        { "<D-l>x", "<cmd>Trouble diagnostics toggle<cr>", desc = "Diagnostics (Trouble)" },
        { "<D-l>X", "<cmd>Trouble diagnostics toggle filter.buf=0<cr>", desc = "Buffer diagnostics" },

        -- Window menu
        { "<D-j>", group = "windows" },
        { "<D-j>v", "<cmd>vsplit<cr>", desc = "Vsplit" },
        { "<D-j>s", "<cmd>split<cr>", desc = "Split" },
        { "<D-j>c", "<cmd>close<cr>", desc = "Close window" },
        { "<D-j>o", "<cmd>only<cr>", desc = "Only window" },

        -- Navigation transient menu (matching Emacs nav-transient-menu)
        { "<D-u>", group = "navigation" },

        -- Changes
        { "<D-u>c", "g;", desc = "Last change" },
        { "<D-u>p", "<cmd>Gitsigns prev_hunk<cr>", desc = "Previous hunk" },
        { "<D-u>n", "<cmd>Gitsigns next_hunk<cr>", desc = "Next hunk" },

        -- Things
        { "<D-u>g", "<cmd>Telescope lsp_document_symbols<cr>", desc = "Imenu" },
        { "<D-u>m", "<cmd>Telescope marks<cr>", desc = "Marks" },
        { "<D-u>t", "<cmd>Telescope grep_string search=TODO<cr>", desc = "TODOs" },

        -- Position
        { "<D-u>b", "<C-o>", desc = "Jump backward" },
        { "<D-u>f", "<C-i>", desc = "Jump forward" },
        {
          "<D-u>w",
          function()
            require("flash").jump()
          end,
          desc = "Word (flash jump)",
        },

        -- Xref
        { "<D-u>a", "<C-o>", desc = "Back" },
        { "<D-u>o", "<C-i>", desc = "Forward" },

        -- Comment mappings
        { "gcc", desc = "Toggle line comment" },
        { "gbc", desc = "Toggle block comment" },
        { "gc", desc = "Comment line/selection", mode = { "n", "v" } },
        { "gb", desc = "Comment block", mode = { "n", "v" } },
        { "gcO", desc = "Add comment above" },
        { "gco", desc = "Add comment below" },
        { "gcA", desc = "Add comment at end of line" },

        -- Surround mappings
        { "sa", desc = "Add surrounding", mode = { "n", "v" } },
        { "sd", desc = "Delete surrounding" },
        { "sr", desc = "Replace surrounding" },
        { "sf", desc = "Find surrounding (right)" },
        { "sF", desc = "Find surrounding (left)" },
        { "sh", desc = "Highlight surrounding" },
        { "sn", desc = "Update n_lines" },
      },
    },
  },
}
