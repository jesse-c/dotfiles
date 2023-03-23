local ok, nvim_tree = pcall(require, "nvim-tree")
if not ok then return end

nvim_tree.setup({
	open_on_setup = true,
	view = {
		width = 60,
	},
	renderer = {
		group_empty = true,
	},
	filters = {
		dotfiles = false,
		git_clean = false,
	},
  git = {
    ignore = false,
  },
  modified = {
    enable = true,
  },
})
