-- Bootstrap user.nvim
local user_install_path = vim.fn.stdpath("data").."/site/pack/user/opt/faerryn/user.nvim/default/default"
if vim.fn.empty(vim.fn.glob(user_install_path)) > 0 then
	os.execute([[git clone --depth 1 https://github.com/faerryn/user.nvim.git ']]..user_install_path..[[']])
end
vim.api.nvim_command("packadd faerryn/user.nvim/default/default")

require('settings')
require('plugins')
require('ui')
require('completion')
require('languages')
require('mappings')
