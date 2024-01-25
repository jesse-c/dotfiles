-- Line numbers

vim.o.number = true
vim.wo.relativenumber = true

-- Syntax highlighting

vim.cmd([[ filetype plugin indent on ]])
vim.cmd([[ syntax enable ]])

-- Splits

vim.api.nvim_create_autocmd("VimResized", {
  pattern = "*",
  callback = function(_args)
    vim.cmd([[ wincmd = ]])
  end,
  desc = "Auto resize splits when window is resized",
})

vim.o.splitbelow = true
vim.o.splitright = true

vim.api.nvim_set_keymap("n", "<C-X>", "<cmd>split<cr>", { silent = true, noremap = true })
vim.api.nvim_set_keymap("n", "<C-V>", "<cmd>vsplit<cr>", { silent = true, noremap = true })

-- Scrolling

vim.o.scrolloff = 4

-- Current line

vim.o.ruler = true
vim.o.cursorline = true
