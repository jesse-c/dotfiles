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
	preview_updates = true,
	display = {
		open_fn = function()
			return require("packer.util").float({ border = "rounded" })
		end,
		compact = true,
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

	-- Meta
	use({
		"williamboman/mason.nvim",
		config = function()
			require("mason").setup()
		end,
	})

	-- UI
	use("nvim-lua/popup.nvim")
	use("nvim-lua/plenary.nvim")
	use({
		"rcarriga/nvim-notify",
		config = function()
			-- Here's a quick fix for this warning:
			--
			--  WARN Highlight group 'Normal' has no background highlight
			-- Please provide an RGB hex value or highlight group with a background value for 'background_colour' option.
			-- This is the colour that will be used for 100% transparency.
			require("notify").setup({
				background_colour = "#000000",
			})

			vim.notify = require("notify")
		end,
	})
	-- ✅ Highlight, list and search todo comments in your projects
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
	-- Multiple cursors plugin for vim/neovim
	use({
		"mg979/vim-visual-multi",
		-- event = "BufRead",
		config = function()
			-- For some reason, this is only working within vim.cmd

			-- Test 1
			-- local mappings = {}
			-- mappings["Add Cursor Down"] = "<S-Down>"
			-- mappings["Add Cursor Up"] = "<S-Up>"
			-- mappings["Undo"] = "u"
			-- mappings["Redo"] = "<C-r>"
			--
			-- vim.g.VM_default_mappings = 0
			-- vim.g.VM_mouse_mappings = 0
			-- vim.g.VM_maps = mappings

			-- Test 2
			-- vim.g.VM_mouse_mappings = 0

			-- vim.g.VM_maps = {
			-- 	["Add Cursor Down"] = "<Down>",
			-- 	["Add Cursor Up"] = "<Up>",
			-- 	Undo = "u",
			-- 	Redo = "<C-r>",
			-- 	Exit = "<C-c>",
			-- }

			-- Test 3
			vim.cmd([[
			  let g:VM_default_mappings = 0
			  let g:VM_mouse_mappings = 0
			  let g:VM_maps = {}
			  let g:VM_maps["Add Cursor Down"] = '<S-Down>'
			  let g:VM_maps["Add Cursor Up"] = '<S-Up>'
			  let g:VM_maps["Undo"] = 'u'
			  let g:VM_maps["Redo"] = '<C-r>'
			  let g:VM_maps["Exit"] = '<C-c>'
			]])
		end,
	})
	-- Rainbow Parentheses Improved, shorter code, no level limit, smooth and fast, powerful configuration
	use({
		"luochen1990/rainbow",
		event = "BufRead",
	})
	-- match-up is a plugin that lets you highlight, navigate, and operate on sets of matching text. It extends vim's % key to language-specific words instead of just single characters.
	use({ "andymass/vim-matchup", requires = "nvim-treesitter/nvim-treesitter" })
	-- A super powerful autopair plugin for Neovim that supports multiple characters.
	use({
		"windwp/nvim-autopairs",
		event = "BufRead",
		config = function()
			require("nvim-autopairs").setup({
				disable_filetype = { "TelescopePrompt", "vim" },
			})
		end,
	})
	-- A high-performance color highlighter
	use({
		"norcalli/nvim-colorizer.lua",
		event = "BufRead",
		config = function()
			require("colorizer").setup()
		end,
	})
	-- Toggles between hybrid and absolute line numbers automatically
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
	use("linty-org/key-menu.nvim")
	use({
		"folke/which-key.nvim",
		config = function()
			require("which-key").setup()
		end,
		disable = true,
	})
	use({
		"VonHeikemen/fine-cmdline.nvim",
		requires = { "MunifTanjim/nui.nvim" },
	})
	use("sindrets/winshift.nvim")
	use({
		"voldikss/vim-floaterm",
		branch = "master",
		config = function()
			vim.g.floaterm_shell = "fish"
			vim.g.floaterm_width = 0.9
			vim.g.floaterm_height = 0.9

			function _G.set_terminal_keymaps()
				vim.api.nvim_buf_set_keymap(
					0,
					"t",
					[[<C-\>]],
					"<CMD>FloatermToggle<CR>",
					{ noremap = true, silent = true }
				)
				vim.api.nvim_buf_set_keymap(
					0,
					"t",
					[[<C-k>]],
					"<CMD>FloatermKill<CR>",
					{ noremap = true, silent = true }
				)
				vim.api.nvim_buf_set_keymap(
					0,
					"t",
					[[<C-g>]],
					"<CMD>FloatermUpdate --title=tig tig<CR>",
					{ noremap = true, silent = true }
				)
			end

			-- if you only want these mappings for toggle term use term://*toggleterm#* instead
			vim.cmd("autocmd! TermOpen term://* lua set_terminal_keymaps()")

			vim.api.nvim_set_keymap("n", [[<C-\>]], "<CMD>FloatermToggle<CR>", { noremap = true, silent = true })
		end,
	})
	-- Extensible Neovim Scrollbar
	use({
		"petertriho/nvim-scrollbar",
		requires = "kevinhwang91/nvim-hlslens",
		config = function()
			require("scrollbar").setup()
			require("scrollbar.handlers.search").setup()
		end,
		disable = true,
	})
	use({
		"lewis6991/satellite.nvim",
		config = function()
			require("satellite").setup()
		end,
		disable = true,
	})
	-- Hlsearch Lens for Neovim
	use({ "kevinhwang91/nvim-hlslens" })
	use({
		"https://git.sr.ht/~whynothugo/lsp_lines.nvim",
		requires = "neovim/nvim-lspconfig",
		config = function()
			require("lsp_lines").setup()
		end,
	})

	-- UI / Marks
	use({
		"chentoast/marks.nvim",
		config = function()
			require("marks").setup({})
		end,
	})

	-- UI / Folds
	use({
		"anuvyklack/pretty-fold.nvim",
		requires = "anuvyklack/nvim-keymap-amend", -- only for preview
		config = function()
			require("pretty-fold").setup()
		end,
	})
	use({
		"anuvyklack/fold-preview.nvim",
		requires = { "anuvyklack/keymap-amend.nvim", "anuvyklack/pretty-fold.nvim" },
		config = function()
			require("fold-preview").setup()
		end,
	})

	-- UI / Themes
	use({
		"ishan9299/modus-theme-vim",
		config = function()
			vim.g.modus_faint_syntax = true

			vim.cmd('colorscheme modus-vivendi')
		end,
	})
	use({
		"projekt0n/github-nvim-theme",
		config = function()
			-- Default theme
			local theme = "dark_default"
			-- The "user.ui.theme" module may contain `return $THEME`
			theme = require("user.ui.theme")
			local ok, local_theme = pcall(require, "user.ui.theme")
			if ok then
				theme = local_theme
			end

			require("github-theme").setup({
				theme_style = theme,
				-- Overwrite the highlight groups
				overrides = function(_)
					return {
						DiagnosticVirtualTextError = { fg = "#db4b4b", bg = "#2D202A" },
						DiagnosticVirtualTextWarn = { fg = "#e0af68", bg = "#2E2A2D" },
						DiagnosticVirtualTextInfo = { fg = "#1abc9c", bg = "#1A2B3" },
						DiagnosticVirtualTextHint = { fg = "#1abc9c", bg = "#1A2B3" },
					}
				end,
			})
		end,
		disable = true,
	})

	-- Buffers
	-- Neovim plugin for locking a buffer to a window
	use({
		"stevearc/stickybuf.nvim",
		config = function()
			require("stickybuf").setup()
		end,
	})
	-- Neovim plugin to stabilize window open/close events
	use({
		"luukvbaal/stabilize.nvim",
		config = function()
			require("stabilize").setup()
		end,
	})

	-- Windows
	use("troydm/zoomwintab.vim")

	-- Tabline
	-- A snazzy bufferline for Neovim
	use({
		"akinsho/bufferline.nvim",
		branch = "main",
		requires = "kyazdani42/nvim-web-devicons",
		config = function()
			require("bufferline").setup({ options = { mode = "tabs", numbers = "ordinal" } })
		end,
	})

	-- Statusline
	use({
		"feline-nvim/feline.nvim",
		requires = "lewis6991/gitsigns.nvim",
		config = function()
			local feline = require("feline")

			local config = require("user.plugins.feline-nvim")

			feline.setup(config.statusline)
			feline.winbar.setup(config.winbar)
		end,
	})

	-- Undo
	use("mbbill/undotree")

	-- Search
	-- display number of search matches & index of a current match
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
		end,
	})

	-- Spotlight
	use({ "nvim-telescope/telescope.nvim", requires = "nvim-telescope/telescope-fzf-native.nvim" })
	use({ "nvim-telescope/telescope-fzf-native.nvim", run = "make" })
	use({ "jvgrootveld/telescope-zoxide", requires = "nvim-telescope/telescope.nvim" })
	use({ "gbrlsnchs/telescope-lsp-handlers.nvim", requires = "nvim-telescope/telescope.nvim" })
	use({ "nvim-telescope/telescope-file-browser.nvim", requires = "nvim-telescope/telescope.nvim" })

	-- VCS
	-- magit for neovim
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
	-- Single tabpage interface for easily cycling through diffs for all modified files for any git rev.
	use({ "sindrets/diffview.nvim", event = "BufRead" })
	-- Super fast git decorations implemented purely in lua/teal.
	use({
		"lewis6991/gitsigns.nvim",
		requires = "nvim-lua/plenary.nvim",
		-- tag = "release", -- To use the latest release
		event = "BufRead",
		config = function()
			require("gitsigns").setup({
				current_line_blame = true,
			})
		end,
	})
	-- A Git wrapper so awesome, it should be illegal
	use({ "tpope/vim-fugitive", event = "BufRead" })

	-- LSP
	use("neovim/nvim-lspconfig")
	use({
		"williamboman/mason-lspconfig.nvim",
		requires = {
			"williamboman/mason.nvim",
			"neovim/nvim-lspconfig",
			"stevearc/aerial.nvim",
		},
		config = function()
			require("mason-lspconfig").setup({
				ensure_installed = {
					"bashls",
					"clojure_lsp",
					"elixirls",
					"erlangls",
					"html",
					"gopls",
					"jsonls",
					"lemminx",
					"ltex",
					"prosemd_lsp",
					"pylsp",
					"rust_analyzer",
					"solargraph",
					"sorbet",
					"sourcekit",
					"sqls",
					"sumneko_lua",
					"tsserver",
					"yamlls",
				},
			})
		end,
	})
	-- vscode-like pictograms for neovim lsp completion items
	use("onsails/lspkind.nvim")
	-- Use Neovim as a language server to inject LSP diagnostics, code actions, and more via Lua
	use({
		"jose-elias-alvarez/null-ls.nvim",
		requires = { "nvim-lua/plenary.nvim", "lewis6991/gitsigns.nvim" },
		event = "VimEnter",
	})
	-- 🚦 A pretty diagnostics, references, telescope results, quickfix and location list to help you solve all the trouble your code is causing.
	use({
		"folke/trouble.nvim",
		requires = "kyazdani42/nvim-web-devicons",
		config = function()
			require("trouble").setup()
		end,
	})
	-- Neovim plugin for a code outline window
	use({
		"stevearc/aerial.nvim",
		requires = "stevearc/stickybuf.nvim",
		config = function()
			require("aerial").setup()
		end,
	})
	-- Standalone UI for nvim-lsp progress. Eye candy for the impatient.
	use({
		"j-hui/fidget.nvim",
		requires = "neovim/nvim-lspconfig",
		config = function()
			require("fidget").setup()
		end,
	})

	-- File system
	-- Helpers for UNIX
	use("tpope/vim-eunuch")
	-- Changes Vim working directory to project root
	use("airblade/vim-rooter")
	use({
		"kyazdani42/nvim-tree.lua",
		requires = "kyazdani42/nvim-web-devicons",
	})

	-- Documentation
	-- A better annotation generator. Supports multiple languages and annotation conventions.
	use({
		"danymat/neogen",
		config = function()
			require("neogen").setup({})
		end,
		requires = "nvim-treesitter/nvim-treesitter",
	})

	-- Comments
	-- 🧠 💪 // Smart and powerful comment plugin for neovim. Supports treesitter, dot repeat, left-right/up-down motions, hooks, and more
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
				ensure_installed = "all",
				ignore_install = { "phpdoc" },
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
	-- Syntax aware text-objects, select, move, swap, and peek support
	use({
		"nvim-treesitter/nvim-treesitter-textobjects",
		requires = "nvim-treesitter/nvim-treesitter",
		event = "BufRead",
	})
	-- Neovim treesitter plugin for setting the commentstring based on the cursor location in a file.
	use({
		"JoosepAlviste/nvim-ts-context-commentstring",
		requires = "nvim-treesitter/nvim-treesitter",
		event = "BufRead",
	})

	-- Sessions
	-- continuously updated session files
	use("tpope/vim-obsession")

	-- Completion
	-- A completion plugin for neovim coded in Lua.
	use({
		"hrsh7th/nvim-cmp",
		requires = {
			"neovim/nvim-lspconfig",
			"onsails/lspkind.nvim",
		},
		config = function()
			require("user.completion").setup()
		end,
	})
	use({ "hrsh7th/cmp-path", requires = "hrsh7th/nvim-cmp" })
	use({ "ray-x/cmp-treesitter", requires = "hrsh7th/nvim-cmp" })
	use({ "hrsh7th/cmp-nvim-lsp", requires = "hrsh7th/nvim-cmp" })
	use({ "hrsh7th/cmp-buffer", requires = "hrsh7th/nvim-cmp" })
	use({ "hrsh7th/cmp-cmdline", requires = "hrsh7th/nvim-cmp" })
	use({ "saadparwaiz1/cmp_luasnip", requires = "L3MON4D3/LuaSnip" })

	-- Snippets
	use("L3MON4D3/LuaSnip")

	-- Formatting
	-- ✨ A (Neo)vim plugin for formatting code.
	-- Backup if there's no LSP (incl. null-ls)
	use("sbdchd/neoformat")

	-- Testing
	-- An extensible framework for interacting with tests within NeoVim.
	use({
		"rcarriga/neotest",
		requires = {
			"nvim-lua/plenary.nvim",
			"nvim-treesitter/nvim-treesitter",
			"antoinemadec/FixCursorHold.nvim",
			-- Adapters
			"jfpedroza/neotest-elixir",
			"rouge8/neotest-rust",
			"nvim-neotest/neotest-python",
		},
		config = function()
			require("neotest").setup({
				adapters = {
					require("neotest-elixir"),
					require("neotest-rust"),
					require("neotest-python"),
				},
			})
		end,
	})

	-- Languages

	-- Debugging
	use("mfussenegger/nvim-dap")

	-- Interactive Repl Over Neovim
	use({
		"hkupty/iron.nvim",
		config = function()
			require("iron.core").setup({
				config = {
					-- Highlights the last sent block with bold
					highlight_last = "IronLastSent",

					-- Toggling behavior is on by default.
					-- Other options are: `single` and `focus`
					visibility = require("iron.visibility").toggle,

					-- Scope of the repl
					-- By default it is one for the same `pwd`
					-- Other options are `tab_based` and `singleton`
					scope = require("iron.scope").path_based,

					-- Whether the repl buffer is a "throwaway" buffer or not
					scratch_repl = false,

					-- Automatically closes the repl window on process end
					close_window_on_exit = true,
					repl_definition = {
						elixir = {
							command = { "iex", "-S", "mix", "run", "--no-start" },
						},
					},
					-- Whether iron should map the `<plug>(..)` mappings
					should_map_plug = false,

					-- Repl position. Check `iron.view` for more options,
					-- currently there are four positions: left, right, bottom, top,
					-- the param is the width/height of the float window
					repl_open_cmd = require("iron.view").curry.bottom(40),
					-- Alternatively, pass a function, which is evaluated when a repl is open.
					-- iron.view.curry will open a float window for the REPL.
					-- alternatively, pass a string of vimscript for opening a fixed window:

					-- If the repl buffer is listed
					buflisted = false,
				},

				-- All the keymaps are set individually
				-- Below is a suggested default
				keymaps = {
					-- send_motion = "<space>sc",
					-- visual_send = "<space>sc",
					-- send_file = "<space>sf",
					-- send_line = "<space>sl",
					-- send_mark = "<space>sm",
					-- mark_motion = "<space>mc",
					-- mark_visual = "<space>mc",
					-- remove_mark = "<space>md",
					-- cr = "<space>s<cr>",
					-- interrupt = "<space>s<space>",
					-- exit = "<space>sq",
					-- clear = "<space>cl",
				},

				-- If the highlight is on, you can change how it looks
				-- For the available options, check nvim_set_hl
				highlight = {
					italic = true,
				},
			})
		end,
		disable = true,
	})

	-- Languages / LaTeX
	use("lervag/vimtex")

	-- Languages / plist
	use("darfink/vim-plist")

	-- Languages / Elixir
	use("elixir-editors/vim-elixir")

	-- Languages / Clojure
	-- Infer parentheses for Clojure, Lisp and Scheme.
	use({
		"eraserhd/parinfer-rust",
		run = "cargo build --release",
	})
	-- Interactive evaluation for Neovim (Clojure, Fennel, Janet, Racket, Hy, MIT Scheme, Guile)
	use("Olical/conjure")

	-- Languages / Rust
	use("simrat39/rust-tools.nvim")
	use("rust-lang/rust.vim")

	-- Languages / Markdown
	use("npxbr/glow.nvim")

	-- Languages / Lua
	-- Interactive real time neovim scratchpad for embedded lua engine - type and watch!
	use({
		"rafcamlet/nvim-luapad",
		config = function()
			require("luapad").setup({})
		end,
	})

	-- Languages / SQL
	-- Neovim plugin for sqls that leverages the built-in LSP client
	use("nanotee/sqls.nvim")

	--------------------------------------------------------------------------------

	if PACKER_BOOTSTRAP then
		require("packer").sync()
	end
end)
