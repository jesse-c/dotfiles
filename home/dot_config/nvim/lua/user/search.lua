local o = vim.o -- For the globals options

o.incsearch = true
o.hlsearch = true
o.showmatch = true
o.ignorecase = true
o.smartcase = true
vim.keymap.set("n", "<esc>", "<cmd>noh<cr><esc>", { silent = true })
