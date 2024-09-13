local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", -- latest stable release
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

vim.g.mapleader = " "

local spec = {
  { import = "user.plugins" },
}

require("lazy").setup({
  spec = spec,
  defaults = { lazy = false },
  performance = {
    rtp = {
      disabled_plugins = {
        "tohtml",
        "netrw",
        "netrwPlugin",
        "netrwSettings",
        "netrwFileHandlers",
        "gzip",
        "zip",
        "zipPlugin",
        "tar",
        "tarPlugin",
        "getscript",
        "getscriptPlugin",
        "vimball",
        "vimballPlugin",
        "2html_plugin",
        "logipat",
        "rrhelper",
        "spellfile_plugin",
        "matchit",
      },
    },
  },
  checker = {
    enabled = true,
    frequency = 60 * 24 * 7,
  },
  lockfile = vim.fn.stdpath("config") .. "/lazy-lock.json",
  install = {
    missing = true,
    colorscheme = { "catppuccin" },
  },
})
