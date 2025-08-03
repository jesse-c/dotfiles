return {
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
