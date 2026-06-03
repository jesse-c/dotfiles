return {
  {
    "nvim-treesitter/nvim-treesitter",
    event = { "BufRead" },
    cmd = {
      "TSInstall",
      "TSInstallFromGrammar",
      "TSLog",
      "TSUninstall",
      "TSUpdate",
    },
    build = ":TSUpdate",
    config = function()
      local languages = {
        -- Core languages (always useful)
        "lua",
        "vim",
        "vimdoc",
        "query",
        "markdown",
        "markdown_inline",
        -- Your languages from Emacs/LSP config
        "bash",
        "c",
        "clojure",
        "elixir",
        "go",
        "javascript",
        "typescript",
        "python",
        "rust",
        "swift",
        "yaml",
        "toml",
        "latex",
        "json",
        "html",
        "css",
        -- Additional useful parsers
        "diff",
        "regex",
        "printf",
      }

      require("nvim-treesitter").setup()
      require("nvim-treesitter.configs").setup({ ensure_installed = languages })

      vim.api.nvim_create_autocmd("FileType", {
        group = vim.api.nvim_create_augroup("user-treesitter", { clear = true }),
        callback = function()
          local ok = pcall(vim.treesitter.start)

          if not ok then
            return
          end

          vim.wo.foldmethod = "expr"
          vim.wo.foldexpr = "v:lua.vim.treesitter.foldexpr()"

          if vim.bo.filetype ~= "python" then
            vim.bo.indentexpr = "v:lua.require'nvim-treesitter'.indentexpr()"
          end
        end
      })
    end,
  },
  {
    "nvim-treesitter/nvim-treesitter-textobjects",
    dependencies = "nvim-treesitter/nvim-treesitter",
    event = { "BufRead" },
    config = function()
      require("nvim-treesitter-textobjects").setup({
        select = {
          lookahead = true,
        },
        move = {
          set_jumps = true,
        },
      })

      local select = require("nvim-treesitter-textobjects.select")
      local move = require("nvim-treesitter-textobjects.move")
      local swap = require("nvim-treesitter-textobjects.swap")

      local function select_textobject(capture)
        return function()
          select.select_textobject(capture, "textobjects")
        end
      end

      local function goto_textobject(fn, capture)
        return function()
          move[fn](capture, "textobjects")
        end
      end

      vim.keymap.set({ "x", "o" }, "af", select_textobject("@function.outer"))
      vim.keymap.set({ "x", "o" }, "if", select_textobject("@function.inner"))
      vim.keymap.set({ "x", "o" }, "ac", select_textobject("@class.outer"))
      vim.keymap.set({ "x", "o" }, "ic", select_textobject("@class.inner"))
      vim.keymap.set({ "x", "o" }, "aa", select_textobject("@parameter.outer"))
      vim.keymap.set({ "x", "o" }, "ia", select_textobject("@parameter.inner"))
      vim.keymap.set({ "x", "o" }, "ab", select_textobject("@block.outer"))
      vim.keymap.set({ "x", "o" }, "ib", select_textobject("@block.inner"))

      vim.keymap.set({ "n", "x", "o" }, "]m", goto_textobject("goto_next_start", "@function.outer"))
      vim.keymap.set({ "n", "x", "o" }, "]]", goto_textobject("goto_next_start", "@class.outer"))
      vim.keymap.set({ "n", "x", "o" }, "]M", goto_textobject("goto_next_end", "@function.outer"))
      vim.keymap.set({ "n", "x", "o" }, "][", goto_textobject("goto_next_end", "@class.outer"))
      vim.keymap.set({ "n", "x", "o" }, "[m", goto_textobject("goto_previous_start", "@function.outer"))
      vim.keymap.set({ "n", "x", "o" }, "[[", goto_textobject("goto_previous_start", "@class.outer"))
      vim.keymap.set({ "n", "x", "o" }, "[M", goto_textobject("goto_previous_end", "@function.outer"))
      vim.keymap.set({ "n", "x", "o" }, "[]", goto_textobject("goto_previous_end", "@class.outer"))

      vim.keymap.set("n", "<leader>a", function()
        swap.swap_next("@parameter.inner", "textobjects")
      end)
      vim.keymap.set("n", "<leader>A", function()
        swap.swap_previous("@parameter.inner", "textobjects")
      end)
    end,
  },
}
