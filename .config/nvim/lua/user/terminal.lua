local o = vim.o -- For the globals options

local autocmd = require("user.autocmd")

-- Enable mouse support
o.mouse = "a"
-- Restore cursor on exit
autocmd({
	events = { "VimLeave" },
	pattern = "*",
	command = "set guicursor=a:ver100-blinkon0",
})
