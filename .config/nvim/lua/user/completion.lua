vim.g.coq_settings = {
	auto_start = "shut-up",
	keymap = {
		jump_to_mark = "<c-x>",
		bigger_preview = "<c-x>",
	},
}

vim.cmd([[ set updatetime=300 ]])
-- Don't give |ins-completion-menu| messages.
vim.cmd([[ set shortmess+=c ]])
