return {
    "nvim-lua/popup.nvim",
    "nvim-lua/plenary.nvim",
    -- ✅ highlight, list and search todo comments in your projects
    {
        "folke/todo-comments.nvim",
        event = "BufRead",
        config = function() require("todo-comments").setup({}) end
    }, {
        "kyazdani42/nvim-web-devicons",
        config = function() require("nvim-web-devicons").setup({}) end
    },
    -- Rainbow Parentheses Improved, shorter code, no level limit, smooth and fast, powerful configuration
    {"luochen1990/rainbow", event = "BufRead"},
    -- match-up is a plugin that lets you highlight, navigate, and operate on sets of matching text. It extends vim's % key to language-specific words instead of just single characters.
    {"andymass/vim-matchup", dependencies = "nvim-treesitter/nvim-treesitter"},
    -- A super powerful autopair plugin for Neovim that supports multiple characters.
    {
        "windwp/nvim-autopairs",
        event = "BufRead",
        config = function()
            require("nvim-autopairs").setup({
                disable_filetype = {"TelescopePrompt", "vim"}
            })
        end
    }, -- A high-performance color highlighter
    {
        "norcalli/nvim-colorizer.lua",
        event = "BufRead",
        config = function() require("colorizer").setup() end
    }, -- Toggles between hybrid and absolute line numbers automatically
    {
        "lukas-reineke/indent-blankline.nvim",
        branch = "master",
        event = "BufRead"
    }, "jeffkreeftmeijer/vim-numbertoggle",
    -- Peek lines just when you intend
    {
        "nacro90/numb.nvim",
        event = "BufRead",
        config = function() require("numb").setup() end
    }, "linty-org/key-menu.nvim", {
        "folke/which-key.nvim",
        config = function() require("which-key").setup() end,
        disable = true
    },
    {"VonHeikemen/fine-cmdline.nvim", dependencies = {"MunifTanjim/nui.nvim"}},
    "sindrets/winshift.nvim",
    {
        "voldikss/vim-floaterm",
        branch = "master",
        config = function()
            vim.g.floaterm_shell = "fish"
            vim.g.floaterm_width = 0.9
            vim.g.floaterm_height = 0.9

            function _G.set_terminal_keymaps()
                vim.api.nvim_buf_set_keymap(0, "t", [[<C-\>]],
                                            "<CMD>FloatermToggle<CR>",
                                            {noremap = true, silent = true})
                vim.api.nvim_buf_set_keymap(0, "t", [[<C-k>]],
                                            "<CMD>FloatermKill<CR>",
                                            {noremap = true, silent = true})
                vim.api.nvim_buf_set_keymap(0, "t", [[<C-g>]],
                                            "<CMD>FloatermUpdate --title=tig tig<CR>",
                                            {noremap = true, silent = true})
            end

            -- if you only want these mappings for toggle term use term://*toggleterm#* instead
            vim.cmd("autocmd! TermOpen term://* lua set_terminal_keymaps()")

            vim.api.nvim_set_keymap("n", [[<C-\>]], "<CMD>FloatermToggle<CR>",
                                    {noremap = true, silent = true})
        end
    }, -- Extensible Neovim Scrollbar
    {
        "petertriho/nvim-scrollbar",
        dependencies = "kevinhwang91/nvim-hlslens",
        config = function()
            require("scrollbar").setup()
            require("scrollbar.handlers.search").setup()
        end,
        disable = true
    }, {
        "lewis6991/satellite.nvim",
        config = function() require("satellite").setup() end,
        disable = true
    }, -- Hlsearch Lens for Neovim
    "kevinhwang91/nvim-hlslens", {
        "https://git.sr.ht/~whynothugo/lsp_lines.nvim",
        dependencies = "neovim/nvim-lspconfig",
        config = function() require("lsp_lines").setup() end
    }, -- Marks
    {"chentoast/marks.nvim", config = function() require("marks").setup({}) end},
    -- Folds
    {
        "anuvyklack/pretty-fold.nvim",
        dependencies = "anuvyklack/nvim-keymap-amend", -- only for preview
        config = function() require("pretty-fold").setup() end
    }, {
        "anuvyklack/fold-preview.nvim",
        dependencies = {
            "anuvyklack/keymap-amend.nvim", "anuvyklack/pretty-fold.nvim"
        },
        config = function() require("fold-preview").setup() end
    },
    -- Themes
    {
    	"ishan9299/modus-theme-vim",
    	config = function()
    		vim.g.modus_faint_syntax = true

    		vim.cmd('colorscheme modus-vivendi')
    	end,
    },
    {
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
    	enabled = false,
    },
}
