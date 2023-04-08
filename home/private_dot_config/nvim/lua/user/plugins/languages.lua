return {
  -- All
  {
    "zbirenbaum/copilot.lua",
    cmd = "Copilot",
    event = "InsertEnter",
    config = function()
      require("copilot").setup({})
    end,
  },
  -- LaTeX
  "lervag/vimtex",
  -- plist
  "darfink/vim-plist",
  -- Elixir
  "elixir-editors/vim-elixir",
  -- Clojure
  -- Infer parentheses for Clojure, Lisp and Scheme.
  {"eraserhd/parinfer-rust", build = "cargo build --release"},
  -- Interactive evaluation for Neovim (Clojure, Fennel, Janet, Racket, Hy, MIT Scheme, Guile)
  "Olical/conjure",
  -- Rust
  "simrat39/rust-tools.nvim",
  "rust-lang/rust.vim",
  -- Markdown
  "npxbr/glow.nvim",
  -- Lua
  -- Interactive real time neovim scratchpad for embedded lua engine - type and watch!
  {
      "rafcamlet/nvim-luapad",
      config = function() require("luapad").setup({}) end
  },
}
