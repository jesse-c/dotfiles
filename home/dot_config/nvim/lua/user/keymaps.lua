vim.api.nvim_set_keymap("n", "<C-k>", ":wincmd k<CR>",
                        {silent = true, noremap = true})
vim.api.nvim_set_keymap("n", "<C-j>", ":wincmd j<CR>",
                        {silent = true, noremap = true})
vim.api.nvim_set_keymap("n", "<C-h>", ":wincmd h<CR>",
                        {silent = true, noremap = true})
vim.api.nvim_set_keymap("n", "<C-l>", ":wincmd l<CR>",
                        {silent = true, noremap = true})

vim.api.nvim_set_keymap("n", "<C-q>", ":q<CR>", {silent = true, noremap = true})
vim.api.nvim_set_keymap("n", "<C-a>", ":bd!<CR>",
                        {silent = true, noremap = true})
