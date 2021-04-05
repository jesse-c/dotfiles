require('nvim-treesitter.configs').setup {
  ensure_installed = "maintained", -- One of "all", "maintained" (parsers with maintainers), or a list of languages
  highlight = {
    enable = true                  -- False will disable the whole extension
  },
}
