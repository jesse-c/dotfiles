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

return require("packer").startup(function()
	config = {
		-- Move to lua dir so impatient.nvim can cache it
		compile_path = vim.fn.stdpath("config") .. "/lua/packer_compiled.lua",
	}

	-- Packer can manage itself
	use({
		"wbthomason/packer.nvim",
		event = "VimEnter",
	})

	-- Miscellaneous
	use("lewis6991/impatient.nvim")
	use("nvim-lua/popup.nvim")
	use("nvim-lua/plenary.nvim")

	use("nathom/filetype.nvim")

	-- Theme
	use("sainnhe/edge")
	use({
		"bluz71/vim-moonfly-colors",
		disable = true,
	})

	-- VCS
	use({
		"sindrets/diffview.nvim",
		event = "BufRead",
	})
	use({
		"tpope/vim-fugitive",
		event = "BufRead",
	})
	use({
		"TimUntersberger/neogit",
		event = "BufRead",
		requires = {
			"nvim-lua/plenary.nvim",
			"sindrets/diffview.nvim",
		},
		config = function()
			local neogit = require("neogit")

			neogit.setup({
				integrations = {
					diffview = true,
				},
			})
		end,
	})
	use("mhinz/vim-signify")
	use({
		"APZelos/blamer.nvim",
		config = function()
			vim.g.blamer_enabled = true
		end,
	})
	-- Edit and review GitHub issues and pull requests from the comfort of your favorite editor
	use({
		"pwntester/octo.nvim",
		requires = "nvim-telescope/telescope.nvim",
		config = function()
			require("telescope").load_extension("octo")
		end,
		disable = true,
	})

	-- LSP
	use({
		"neovim/nvim-lspconfig",
		config = function()
			local lspconfig = require("lspconfig")

			lspconfig.sourcekit.setup({})
		end,
	})
	use({
		"williamboman/nvim-lsp-installer",
		requires = { "neovim/nvim-lspconfig", "stevearc/aerial.nvim" },
		config = function()
			local lsp_installer = require("nvim-lsp-installer")

			-- Register a handler that will be called for all installed servers.
			-- Alternatively, you may also register handlers on specific server instances instead (see example below).
			lsp_installer.on_server_ready(function(server)
				local opts = {}
				local aerial = require("aerial")

				if server.name == "efm" then
					opts = {
						on_attach = aerial.on_attach,
						init_options = { documentFormatting = true },
						filetypes = { "elixir" },
						settings = {
							rootMarkers = { ".git/", "mix.exs" },
							languages = {
								elixir = {
									{ formatCommand = "mix format -", formatStdin = true },
								},
							},
						},
					}
				end

				-- This setup() function is exactly the same as lspconfig's setup function.
				-- Refer to https://github.com/neovim/nvim-lspconfig/blob/master/doc/server_configurations.md
				server:setup(opts)
			end)
		end,
		disable = false,
	})
	use({
		"glepnir/lspsaga.nvim",
		branch = "main",
		config = function()
			local saga = require("lspsaga")
			saga.init_lsp_saga()
		end,
		disable = true,
	})
	use({
		"kosayoda/nvim-lightbulb",
		config = function()
			vim.cmd([[autocmd CursorHold,CursorHoldI * lua require'nvim-lightbulb'.update_lightbulb()]])
		end,
		disable = true,
	})
	use({
		"stevearc/stickybuf.nvim",
		config = function()
			require("stickybuf").setup({})
		end,
	})
	use({
		"stevearc/aerial.nvim",
		requires = "stevearc/stickybuf.nvim",
		config = function()
			local aerial = require("aerial")

			-- Aerial does not set any mappings by default, so you'll want to set some up
			aerial.register_attach_cb(function(bufnr)
				-- Toggle the aerial window with <leader>a
				vim.api.nvim_buf_set_keymap(bufnr, "n", "<leader>a", "<cmd>AerialToggle!<CR>", {})
				-- Jump forwards/backwards with '{' and '}'
				vim.api.nvim_buf_set_keymap(bufnr, "n", "{", "<cmd>AerialPrev<CR>", {})
				vim.api.nvim_buf_set_keymap(bufnr, "n", "}", "<cmd>AerialNext<CR>", {})
				-- Jump up the tree with '[[' or ']]'
				vim.api.nvim_buf_set_keymap(bufnr, "n", "[[", "<cmd>AerialPrevUp<CR>", {})
				vim.api.nvim_buf_set_keymap(bufnr, "n", "]]", "<cmd>AerialNextUp<CR>", {})
			end)
		end,
	})

	use({
		"onsails/lspkind-nvim",
		config = function()
			require("lspkind").init({
				-- with_text = true,
				-- symbol_map = {
				--   Text = '',
				--   Method = 'ƒ',
				--   Function = '',
				--   Constructor = '',
				--   Variable = '',
				--   Class = '',
				--   Interface = 'ﰮ',
				--   Module = '',
				--   Property = '',
				--   Unit = '',
				--   Value = '',
				--   Enum = '了',
				--   Keyword = '',
				--   Snippet = '﬌',
				--   Color = '',
				--   File = '',
				--   Folder = '',
				--   EnumMember = '',
				--   Constant = '',
				--   Struct = ''
				-- },
			})
		end,
		disable = true,
	})
	use({
		"folke/lsp-colors.nvim",
		disable = true,
	})
	use({
		"folke/lsp-trouble.nvim",
		config = function()
			require("trouble").setup({
				auto_open = true, -- automatically open the list when you have diagnostics
				auto_close = true, -- automatically close the list when you have no diagnostics
			})
		end,
		disable = true,
	})
	use({
		"rmagatti/goto-preview",
		config = function()
			require("goto-preview").setup({})
		end,
		disable = true,
	})

	-- Testing
	-- TODO

	-- UI
	use({
		"nvim-telescope/telescope.nvim",
		requires = "nvim-telescope/telescope-fzf-native.nvim",
		config = function()
			require("telescope").setup({
				extensions = {
					fzf = {
						fuzzy = true, -- false will only do exact matching
						override_generic_sorter = true, -- override the generic sorter
						override_file_sorter = true, -- override the file sorter
						case_mode = "smart_case", -- or "ignore_case" or "respect_case"
						-- the default case_mode is "smart_case"
					},
				},
			})
			require("telescope").load_extension("fzf")
		end,
	})
	use({ "nvim-telescope/telescope-fzf-native.nvim", run = "make" })

	use({
		"folke/todo-comments.nvim",
		config = function()
			require("todo-comments").setup({})
		end,
	})
	use({
		"luukvbaal/stabilize.nvim",
		config = function()
			require("stabilize").setup()
		end,
	})
	-- Vim plugin for automatically highlighting other uses of the word under the cursor. Integrates with Neovim's LSP client for intelligent highlighting.
	use({
		"RRethy/vim-illuminate",
		disable = true,
	})
	use({
		"machakann/vim-highlightedyank",
		disable = true,
	})
	use("kyazdani42/nvim-web-devicons")
	use({
		"tommcdo/vim-lion",
		disable = true,
	})
	use({
		"mbbill/undotree",
		config = function()
			vim.cmd([[ nnoremap <F5> :UndotreeToggle<cr> ]])
		end,
	})
	use({
		"luochen1990/rainbow",
		config = function()
			vim.g.rainbow_active = true
		end,
	})
	use("google/vim-searchindex")
	use("troydm/zoomwintab.vim")
	use({
		"nvim-lualine/lualine.nvim",
		requires = { "kyazdani42/nvim-web-devicons", opt = true },
		config = function()
			local function condensed_path()
				path = vim.fn.pathshorten(vim.fn.fnamemodify(vim.fn.expand("%:p"), ":p:."))

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
					lualine_b = { "branch", "diff", { "diagnostics", sources = { "nvim_lsp" } } },
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
	use({
		"akinsho/bufferline.nvim",
		requires = "kyazdani42/nvim-web-devicons",
		config = function()
			require("bufferline").setup({})
		end,
	})
	use("chrisbra/Colorizer")
	use({
		"lukas-reineke/indent-blankline.nvim",
		branch = "master",
		event = "BufRead",
		config = function()
			vim.g.indent_blankline_enabled = true
			vim.g.indent_blankline_char = "┆"
			vim.g.indent_blankline_char_list = { "|", "¦", "┆", "┊" }
			vim.g.indent_blankline_use_treesitter = true
			vim.g.indent_blankline_show_first_indent_level = false
		end,
	})
	use({
		"psliwka/vim-smoothie",
		disable = true,
		config = function()
			vim.g.smoothie_enabled = false
		end,
		disable = true,
	})
	use("tpope/vim-abolish")
	use({
		"phaazon/hop.nvim",
		branch = "master",
		config = function()
			require("hop").setup({})
			vim.api.nvim_set_keymap("n", "<leader>a", "<cmd>lua require'hop'.hint_words()<cr>", {})
		end,
		disable = false,
	})
	use({
		"norcalli/nvim-colorizer.lua",
		config = function()
			require("colorizer").setup()
		end,
	})
	use({
		"kevinhwang91/nvim-hlslens",
		disable = true,
	})
	use("jeffkreeftmeijer/vim-numbertoggle")
	-- Peek lines just when you intend
	use({
		"nacro90/numb.nvim",
		config = function()
			require("numb").setup()
		end,
	})
	use({
		"folke/which-key.nvim",
		config = function()
			require("which-key").setup({
				-- your configuration comes here
				-- or leave it empty to use the default settings
				-- refer to the configuration section below
			})
		end,
	})
	use({
		"sudormrfbin/cheatsheet.nvim",
		event = "VimEnter",
		requires = {
			"nvim-telescope/telescope.nvim",
			"nvim-lua/popup.nvim",
			"nvim-lua/plenary.nvim",
		},
		config = function()
			vim.cmd([[ nnoremap <leader>c <cmd>Cheatsheet<cr> ]])
		end,
	})

	-- Marks
	use("kshenoy/vim-signature")

	-- Registers
	-- Alternatives:
	-- - 'junegunn/vim-peekaboo'
	use("gennaro-tedesco/nvim-peekup")

	-- Sessions
	use("tpope/vim-obsession")
	use({
		"folke/persistence.nvim",
		event = "BufReadPre", -- this will only start session saving when an actual file was opened
		module = "persistence",
		config = function()
			require("persistence").setup()
		end,
	})
	use({
		"goolord/alpha-nvim",
		requires = { "kyazdani42/nvim-web-devicons" },
		config = function()
			require("alpha").setup(require("alpha.themes.dashboard").opts)
		end,
		disable = true,
	})

	-- Buffers
	use("jeetsukumaran/vim-buffergator")

	-- Comments
	use({
		"numToStr/Comment.nvim",
		config = function()
			require("Comment").setup({})
		end,
	})

	-- Completion
	use({
		"ms-jpq/coq_nvim",
		branch = "coq",
		-- event = "InsertEnter",
		config = function()
			vim.g.coq_settings = {
				auto_start = "shut-up",
				keymap = {
					jump_to_mark = "<c-x>",
					bigger_preview = "<c-x>",
				},
			}
		end,
	}) -- main one
	use({
		"ms-jpq/coq.artifacts",
		branch = "artifacts",
		requires = "ms-jpq/coq_nvim",
	}) -- 9000+ Snippets

	-- File system
	use("tpope/vim-eunuch")
	use("airblade/vim-rooter")
	-- Alternatives:
	-- - 'francoiscabrol/ranger.vim'
	use({
		"kevinhwang91/rnvimr",
		config = function()
			vim.cmd([[ let g:rnvimr_enable_picker = 1 ]])
			vim.cmd([[ let g:rnvimr_draw_border = 0 ]])
			vim.cmd([[ nnoremap <leader>f <cmd>RnvimrToggle<cr> ]])
		end,
	})
	use({
		"kyazdani42/nvim-tree.lua",
		requires = "kyazdani42/nvim-web-devicons",
		config = function()
			require("nvim-tree").setup({})
			vim.cmd([[ nnoremap <leader>v :NvimTreeFindFile<CR> ]])
			vim.cmd([[ nnoremap <leader>n :NvimTreeToggle<CR> ]])
			vim.g.nvim_tree_width = 60
		end,
	})

	-- Tree-sitter
	-- Nvim Treesitter configurations and abstraction layer
	use({
		"nvim-treesitter/nvim-treesitter",
		event = "BufRead",
		config = function()
			require("nvim-treesitter.configs").setup({
				ensure_installed = "maintained",
				highlight = {
					enable = true, -- False will disable the whole extension
				},
				--[[ incremental_selection = {
          enable = true,
          keymaps = {
            init_selection = "gnn",
            node_incremental = "grn",
            scope_incremental = "grc",
            node_decremental = "grm",
          },
        }, ]]
			})
		end,
		run = ":TSUpdate",
	})
	use({
		"David-Kunz/treesitter-unit",
		requires = "nvim-treesitter/nvim-treesitter",
		event = "BufRead",
		config = function()
			-- vim.api.nvim_set_keymap('v', 'x', ':lua require"treesitter-unit".select()<CR>', {noremap=true})
			-- vim.api.nvim_set_keymap('o', 'x', ':<c-u>lua require"treesitter-unit".select()<CR>', {noremap=true})
		end,
		disable = true,
	})
	use({
		"romgrk/nvim-treesitter-context",
		requires = "nvim-treesitter/nvim-treesitter",
		event = "BufRead",
		config = function()
			require("treesitter-context").setup({
				enable = true,
				throttle = true,
			})
		end,
		disable = true,
	})

	-- Development
	use({
		"haringsrob/nvim_context_vt",
		disable = true,
	})
	use({
		"machakann/vim-sandwich",
		disable = true,
	})
	use({
		"code-biscuits/nvim-biscuits",
		disable = true,
		config = function()
			require("nvim-biscuits").setup({})
		end,
		disable = true,
	})

	-- Snippets
	use({
		"hrsh7th/vim-vsnip",
		disable = true,
	})
	use({
		"hrsh7th/vim-vsnip-integ",
		disable = true,
	})

	-- Formatting
	use({
		"sbdchd/neoformat",
		config = function()
			vim.g.neoformat_enabled_yaml = {}
		end,
		disable = false,
	})

	-- Languages
	use({
		"mrjones2014/dash.nvim",
		requires = {
			"nvim-telescope/telescope.nvim",
			"nvim-lua/plenary.nvim",
		},
		rocks = { "xml2lua" },
		disable = true,
	})

	-- Elixir
	use("elixir-editors/vim-elixir")

	-- Clojure
	use({
		"eraserhd/parinfer-rust",
		run = "cargo build --release",
	})

	-- Rust
	use({
		"simrat39/rust-tools.nvim",
		config = function()
			require("rust-tools").setup({})
		end,
	})

	-- Markdown
	use("npxbr/glow.nvim")
end)
