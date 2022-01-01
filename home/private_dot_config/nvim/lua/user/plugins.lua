-- Disable unused built-in plugins
local disabled_built_ins = {
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
}

for _, plugin in pairs(disabled_built_ins) do
	vim.g["loaded_" .. plugin] = 1
end

-- Bootstrap packer.nvim
local fn = vim.fn
local install_path = fn.stdpath("data") .. "/site/pack/packer/start/packer.nvim"
if fn.empty(fn.glob(install_path)) > 0 then
	PACKER_BOOTSTRAP = fn.system({
		"git",
		"clone",
		"--depth",
		"1",
		"https://github.com/wbthomason/packer.nvim",
		install_path,
	})
end

-- Use a protected call so we don't error out on first use
local ok, packer = pcall(require, "packer")
if not ok then
	return
end

-- Have packer use a popup window
packer.init({
	display = {
		open_fn = function()
			return require("packer.util").float({ border = "rounded" })
		end,
	},
})

return packer.startup(function(use)
	-- Packer can manage itself
	use({
		"wbthomason/packer.nvim",
		event = "VimEnter",
	})

	--------------------------------------------------------------------------------

	-- Performance
	use("lewis6991/impatient.nvim")
	use("nathom/filetype.nvim")

	-- UI
	use("nvim-lua/popup.nvim")
	use("nvim-lua/plenary.nvim")
	use({
		"rcarriga/nvim-notify",
		config = function()
			vim.notify = require("notify")
		end,
	})
	use({
		"folke/todo-comments.nvim",
		event = "BufRead",
		config = function()
			require("todo-comments").setup({})
		end,
	})
	use({
		"kyazdani42/nvim-web-devicons",
		config = function()
			require("nvim-web-devicons").setup({})
		end,
	})
	use({
		"luochen1990/rainbow",
		event = "BufRead",
	})
	use({ "andymass/vim-matchup", requires = "nvim-treesitter/nvim-treesitter" })
	use({
		"windwp/nvim-autopairs",
		event = "BufRead",
		config = function()
			require("nvim-autopairs").setup({
				disable_filetype = { "TelescopePrompt", "vim" },
			})
		end,
	})
	use({
		"norcalli/nvim-colorizer.lua",
		event = "BufRead",
		config = function()
			require("colorizer").setup()
		end,
	})
	use({ "lukas-reineke/indent-blankline.nvim", branch = "master", event = "BufRead" })
	use("jeffkreeftmeijer/vim-numbertoggle")
	-- Peek lines just when you intend
	use({
		"nacro90/numb.nvim",
		event = "BufRead",
		config = function()
			require("numb").setup()
		end,
	})
	use({
		"folke/which-key.nvim",
		config = function()
			require("which-key").setup()
		end,
	})
	use({
		"VonHeikemen/fine-cmdline.nvim",
		requires = { "MunifTanjim/nui.nvim" },
	})
	use("sindrets/winshift.nvim")
	use({
		"akinsho/toggleterm.nvim",
		config = function()
			require("toggleterm").setup({
				direction = "float",
			})
		end,
	})

	-- UI / Themes
	use("sainnhe/edge")

	-- UI / Marks
	use("kshenoy/vim-signature")

	-- Buffers
	use({
		"stevearc/stickybuf.nvim",
		config = function()
			require("stickybuf").setup()
		end,
	})
	use({
		"luukvbaal/stabilize.nvim",
		config = function()
			require("stabilize").setup()
		end,
	})

	-- Windows
	use("troydm/zoomwintab.vim")

	-- Bufferline
	use({
		"akinsho/bufferline.nvim",
		requires = "kyazdani42/nvim-web-devicons",
		config = function()
			require("bufferline").setup()
		end,
	})

	-- Statusline
	use({
		"nvim-lualine/lualine.nvim",
		requires = { "kyazdani42/nvim-web-devicons", opt = true },
		config = function()
			local function condensed_path()
				local path = vim.fn.pathshorten(vim.fn.fnamemodify(vim.fn.expand("%:p"), ":p:."))

				return path
			end

			require("lualine").setup({
				options = {
					icons_enabled = true,
					theme = "auto",
					component_separators = { left = "", right = "" },
					section_separators = { left = "", right = "" },
					disabled_filetypes = {},
					always_divide_middle = true,
				},
				sections = {
					lualine_a = { "mode" },
					lualine_b = { "branch", "diff", { "diagnostics", sources = { "nvim_diagnostic" } } },
					lualine_c = { condensed_path },
					lualine_x = { "encoding", "fileformat", "filetype" },
					lualine_y = { "progress" },
					lualine_z = { "location" },
				},
				inactive_sections = {
					lualine_a = {},
					lualine_b = {},
					lualine_c = { condensed_path },
					lualine_x = { "location" },
					lualine_y = {},
					lualine_z = {},
				},
				tabline = {},
				extensions = { "nvim-tree" },
			})
		end,
	})

	-- Undo
	use("mbbill/undotree")

	-- Search
	use({ "google/vim-searchindex", event = "BufRead" })
	-- Smart substitution
	use({ "tpope/vim-abolish", event = "BufRead" })
	-- sandwich.vim is a set of operator and textobject plugins to add/delete/replace surroundings of a sandwiched textobject, like (foo), "bar".
	use({ "machakann/vim-sandwich", event = "BufRead" })
	use({
		"phaazon/hop.nvim",
		branch = "master",
		event = "BufRead",
		config = function()
			require("hop").setup({})
			vim.api.nvim_set_keymap("n", "<Leader>z", "<cmd>lua require'hop'.hint_words()<cr>", {})
		end,
	})

	-- Spotlight
	use({ "nvim-telescope/telescope.nvim", requires = "nvim-telescope/telescope-fzf-native.nvim" })
	use({ "nvim-telescope/telescope-fzf-native.nvim", run = "make" })
	use({ "jvgrootveld/telescope-zoxide" })

	-- VCS
	use({
		"TimUntersberger/neogit",
		event = "BufRead",
		requires = {
			"nvim-lua/plenary.nvim",
			"sindrets/diffview.nvim",
		},
		config = function()
			require("neogit").setup({
				integrations = {
					diffview = true,
				},
			})
		end,
	})
	use({ "sindrets/diffview.nvim", event = "BufRead" })
	-- Super fast git decorations implemented purely in lua/teal.
	use({
		"lewis6991/gitsigns.nvim",
		requires = "nvim-lua/plenary.nvim",
		tag = "release", -- To use the latest release
		event = "BufRead",
		config = function()
			require("gitsigns").setup()
		end,
	})
	use({ "APZelos/blamer.nvim", event = "BufRead" })
	use({ "tpope/vim-fugitive", event = "BufRead" })

	-- LSP
	use("neovim/nvim-lspconfig")
	use({
		"williamboman/nvim-lsp-installer",
		requires = { "neovim/nvim-lspconfig", "stevearc/aerial.nvim" },
	})

	use({
		"jose-elias-alvarez/null-ls.nvim",
		requires = "nvim-lua/plenary.nvim",
		event = "VimEnter",
		config = function()
			local null_ls = require("null-ls")

			null_ls.setup({
				sources = {
					-- Diagnostics
					null_ls.builtins.diagnostics.credo,
					null_ls.builtins.diagnostics.flake8,
					null_ls.builtins.diagnostics.proselint,
					null_ls.builtins.diagnostics.shellcheck,
					-- Code actions
					null_ls.builtins.code_actions.gitsigns,
					null_ls.builtins.code_actions.proselint,
					null_ls.builtins.code_actions.shellcheck,
					-- Formatters
					null_ls.builtins.formatting.prettier,
					null_ls.builtins.formatting.joker,
					null_ls.builtins.formatting.stylua,
					null_ls.builtins.formatting.swiftformat,
					null_ls.builtins.formatting.black,
					null_ls.builtins.formatting.isort,
					null_ls.builtins.formatting.erlfmt,
					null_ls.builtins.formatting.fish_indent,
					null_ls.builtins.formatting.fnlfmt,
					null_ls.builtins.formatting.shfmt,
					null_ls.builtins.formatting.shellharden,
				},
			})
		end,
	})
	use({
		"folke/trouble.nvim",
		requires = "kyazdani42/nvim-web-devicons",
		config = function()
			require("trouble").setup()
		end,
	})
	use({
		"stevearc/aerial.nvim",
		requires = "stevearc/stickybuf.nvim",
	})

	-- File system
	use("tpope/vim-eunuch")
	use("airblade/vim-rooter")
	use("kevinhwang91/rnvimr")
	use({
		"kyazdani42/nvim-tree.lua",
		requires = "kyazdani42/nvim-web-devicons",
	})

	-- Comments
	use({
		"numToStr/Comment.nvim",
		config = function()
			require("Comment").setup()
		end,
	})

	-- Tree-sitter
	use({
		"nvim-treesitter/nvim-treesitter",
		event = "BufRead",
		run = ":TSUpdate",
		config = function()
			require("nvim-treesitter.configs").setup({
				ensure_installed = "maintained",
				ignore_install = {},
				highlight = {
					enable = true, -- False will disable the whole extension
				},
				incremental_selection = {
					enable = true,
					keymaps = {
						init_selection = "<CR>",
						scope_incremental = "<CR>",
						node_incremental = "<TAB>",
						node_decremental = "<S-TAB>",
					},
				},
				textobjects = {
					swap = {
						enable = true,
						swap_next = {
							["<Leader>s"] = "@parameter.inner",
						},
						swap_previous = {
							["<Leader>S"] = "@parameter.inner",
						},
					},
					move = {
						enable = true,
						set_jumps = true, -- whether to set jumps in the jumplist
						goto_next_start = {
							["]m"] = "@function.outer",
							["]]"] = "@class.outer",
						},
						goto_next_end = {
							["]M"] = "@function.outer",
							["]["] = "@class.outer",
						},
						goto_previous_start = {
							["[m"] = "@function.outer",
							["[["] = "@class.outer",
						},
						goto_previous_end = {
							["[M"] = "@function.outer",
							["[]"] = "@class.outer",
						},
					},
					context_commentstring = {
						enable = true,
					},
				},
				playground = {
					enable = true,
					disable = {},
					updatetime = 25, -- Debounced time for highlighting nodes in the playground from source code
					persist_queries = false, -- Whether the query persists across vim sessions
					keybindings = {
						toggle_query_editor = "o",
						toggle_hl_groups = "i",
						toggle_injected_languages = "t",
						toggle_anonymous_nodes = "a",
						toggle_language_display = "I",
						focus_language = "f",
						unfocus_language = "F",
						update = "R",
						goto_node = "<cr>",
						show_help = "?",
					},
				},
				matchup = {
					enable = true,
					disable = {},
				},
			})
		end,
	})
	use({
		"nvim-treesitter/playground",
		requires = "nvim-treesitter/nvim-treesitter",
		event = "BufRead",
	})
	use({
		"nvim-treesitter/nvim-treesitter-textobjects",
		requires = "nvim-treesitter/nvim-treesitter",
		event = "BufRead",
	})
	use({
		"JoosepAlviste/nvim-ts-context-commentstring",
		requires = "nvim-treesitter/nvim-treesitter",
		event = "BufRead",
	})

	-- Sessions
	use("tpope/vim-obsession")

	-- Completion
	use({
		"ms-jpq/coq_nvim",
		branch = "coq",
		-- event = "InsertEnter",
	})
	use({
		"ms-jpq/coq.artifacts",
		branch = "artifacts",
		requires = "ms-jpq/coq_nvim",
	}) -- 9000+ Snippets

	-- Formatting
	use("sbdchd/neoformat")

	-- Languages

	-- Languages / Elixir
	use("elixir-editors/vim-elixir")

	-- Languages / Clojure
	use({
		"eraserhd/parinfer-rust",
		run = "cargo build --release",
	})

	-- Languages / Rust
	use("simrat39/rust-tools.nvim")
	use("rust-lang/rust.vim")

	-- Languages / Markdown
	use("npxbr/glow.nvim")

	-- Languages / Lua
	use({
		"rafcamlet/nvim-luapad",
		config = function()
			require("luapad").setup({})
		end,
	})

	--------------------------------------------------------------------------------

	if PACKER_BOOTSTRAP then
		require("packer").sync()
	end
end)
