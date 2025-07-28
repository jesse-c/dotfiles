-- Commands

vim.o.cmdheight = 0

-- Line numbers

vim.opt.number = true
vim.opt.relativenumber = true
vim.opt.signcolumn = "yes:2" -- Allow 2 signs per line
vim.opt.numberwidth = 4
vim.opt.foldcolumn = "0" -- Hide fold column entirely
vim.opt.statuscolumn =
  "%s%l %{foldlevel(v:lnum) > foldlevel(v:lnum - 1) ? (foldclosed(v:lnum) == -1 ? '󰅀' : '󰅂') : foldclosed(v:lnum) != -1 ? '󰅂' : ' '} "
vim.opt.foldenable = true
vim.opt.foldlevel = 99 -- Open all folds by default
vim.opt.foldlevelstart = 99
vim.opt.fillchars = {
  foldopen = "󰅀",
  foldclose = "󰅂",
  fold = "·",
  foldsep = "·",
}

-- Custom fold text function for cleaner display
function _G.custom_fold_text()
  local line = vim.fn.getline(vim.v.foldstart)
  local line_count = vim.v.foldend - vim.v.foldstart + 1
  return line .. " ··· " .. line_count .. " lines"
end

vim.opt.foldtext = "v:lua.custom_fold_text()"

-- Syntax highlighting

vim.cmd([[ filetype plugin indent on ]])
vim.cmd([[ syntax enable ]])

-- Splits

vim.api.nvim_set_keymap("n", "<C-k>", ":wincmd k<CR>", { silent = true, noremap = true })
vim.api.nvim_set_keymap("n", "<C-j>", ":wincmd j<CR>", { silent = true, noremap = true })
vim.api.nvim_set_keymap("n", "<C-h>", ":wincmd h<CR>", { silent = true, noremap = true })
vim.api.nvim_set_keymap("n", "<C-l>", ":wincmd l<CR>", { silent = true, noremap = true })

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

-- Quitting

vim.api.nvim_set_keymap("n", "<C-q>", ":q<CR>", { silent = true, noremap = true })
vim.api.nvim_set_keymap("n", "<C-a>", ":bd!<CR>", { silent = true, noremap = true })
