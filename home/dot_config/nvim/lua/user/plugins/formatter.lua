return {
  {
    "stevearc/conform.nvim",
    event = { "BufWritePre" },
    cmd = { "ConformInfo" },
    keys = {
      {
        "<leader>f",
        function()
          require("conform").format({ async = true, lsp_fallback = true })
        end,
        mode = "",
        desc = "Format buffer",
      },
    },
    opts = {
      formatters_by_ft = {
        lua = { "stylua" },
        python = { "black" },
        javascript = { "deno_fmt" },
        typescript = { "deno_fmt" },
        rust = { "rustfmt" },
        go = { "gofmt" },
        elixir = { "mix" },
        clojure = { "cljfmt" },
        yaml = { "yamlfmt" },
        toml = { "taplo" },
        json = { "deno_fmt" },
        markdown = { "deno_fmt" },
        sh = { "shfmt" },
        swift = { "swift_format" },
      },
      format_on_save = {
        timeout_ms = 500,
        lsp_fallback = true,
      },
      formatters = {
        shfmt = {
          prepend_args = { "-i", "2" },
        },
      },
    },
    init = function()
      vim.o.formatexpr = "v:lua.require'conform'.formatexpr()"
    end,
  },
}
