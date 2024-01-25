local configuration_folder = vim.fn.expand("~") .. "/.config/nvim"

vim.o.undodir = configuration_folder .. "/undodir.nvim"
vim.o.undofile = true
vim.o.undolevels = 10000
