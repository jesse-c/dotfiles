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
    "matchit"
}

for _, plugin in pairs(disabled_built_ins) do
    vim.g["loaded_" .. plugin] = 1
end

return require('packer').startup(function(use)
  -- Packer can manage itself
  use {
    'wbthomason/packer.nvim',
    event = "VimEnter",
  }

  -- Miscellaneous
  use 'nvim-lua/popup.nvim'
  use 'nvim-lua/plenary.nvim'

  -- Theme
  use 'sainnhe/edge'
  use 'bluz71/vim-moonfly-colors'

  -- VCS
  use {
    'sindrets/diffview.nvim',
    event = "BufRead",
  }
  use {
    'tpope/vim-fugitive',
    event = "BufRead",
  }
  use {
    'TimUntersberger/neogit',
    event = "BufRead",
    requires = {
      'nvim-lua/plenary.nvim',
      'sindrets/diffview.nvim',
    },
    config = function()
      local neogit = require('neogit')

      neogit.setup({
        integrations = {
          diffview = true
        },
      })

    end,
  }
  use 'mhinz/vim-signify'
  use {
    'APZelos/blamer.nvim',
    config = function()
      vim.g.blamer_enabled = true
    end
  }
  use {
    'pwntester/octo.nvim',
    requires = 'nvim-telescope/telescope.nvim',
    config = function()
      require('telescope').load_extension('octo')
    end
  }

  -- LSP
  use {
    'neovim/nvim-lspconfig',
    config = function()
      local lspconfig = require('lspconfig')

      lspconfig.gopls.setup{}
      lspconfig.clojure_lsp.setup{}
      lspconfig.sourcekit.setup{}
      lspconfig.elixirls.setup{
        cmd = { '/Users/jesse/src/github.com/elixir-lsp/elixir-ls/rel/language_server.sh' };
      }
      lspconfig.rust_analyzer.setup{}
      lspconfig.efm.setup{
        init_options = {documentFormatting = true},
        settings = {
            rootMarkers = {".git/"},
            languages = {
                elixir = {
                    {formatCommand = "mix format -", formatStdin = true}
                }
            }
        }
      }
    end
  }
  use {
    'glepnir/lspsaga.nvim',
    branch = 'main',
    config = function()
      local saga = require 'lspsaga'
      saga.init_lsp_saga()
    end
  }
  use {
    'kosayoda/nvim-lightbulb',
    config = function()
      vim.cmd [[autocmd CursorHold,CursorHoldI * lua require'nvim-lightbulb'.update_lightbulb()]]
    end
  }
  use {
    'liuchengxu/vista.vim',
    config = function()
      vim.cmd [[ nmap <F8> :Vista!!<CR> ]]
      vim.cmd [[ let g:vista#renderer#enable_icon = 1 ]]
    end
  }
  use {
    'onsails/lspkind-nvim',
    config = function()
      require('lspkind').init({
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
    end
  }
  use 'folke/lsp-colors.nvim'
  use {
    'folke/lsp-trouble.nvim',
    config = function()
      require("trouble").setup({
    auto_open = true, -- automatically open the list when you have diagnostics
        auto_close = true, -- automatically close the list when you have no diagnostics
      })
    end
  }
  use {
    'rmagatti/goto-preview',
    config = function()
      require('goto-preview').setup {}
    end
  }

  -- Testing
  -- use 'vim-test/vim-test'
  -- use 'rcarriga/vim-ultest' -- :UpdateRemotePlugins

  -- UI
  use {
    "nvim-telescope/telescope.nvim",
    requires = "nvim-telescope/telescope-fzy-native.nvim",
    config = function()
      require('telescope').setup({})
      require('telescope').load_extension('fzy_native')
    end
  }
  use "nvim-telescope/telescope-fzy-native.nvim"
  use {
    "folke/todo-comments.nvim",
    config = function()
      require("todo-comments").setup()
    end
  }
  use 'RRethy/vim-illuminate'
  use 'machakann/vim-highlightedyank'
  use 'kyazdani42/nvim-web-devicons'
  use 'tommcdo/vim-lion'
  use {
    'mbbill/undotree',
    config = function()
      vim.cmd [[ nnoremap <F5> :UndotreeToggle<cr> ]]
    end
  }
  use {
    'luochen1990/rainbow',
    config = function()
      vim.g.rainbow_active = true
    end
  }
  use 'google/vim-searchindex'
  use 'troydm/zoomwintab.vim'
  use {
    'hoob3rt/lualine.nvim',
    config = function()
      require('lualine').setup()
    end
  }
  use {
    'akinsho/nvim-bufferline.lua',
    requires = 'kyazdani42/nvim-web-devicons',
    config = function()
      require('bufferline').setup{}
    end
  }
  use 'chrisbra/Colorizer'
  use {
    'lukas-reineke/indent-blankline.nvim',
    branch = 'master',
    event = "BufRead",
    config = function()
      vim.g.indent_blankline_enabled = true
      vim.g.indent_blankline_char = '┆'
      vim.g.indent_blankline_char_list = {'|', '¦', '┆', '┊'}
      vim.g.indent_blankline_use_treesitter = true
      vim.g.indent_blankline_show_first_indent_level = false
    end,
  }
  use {
    'psliwka/vim-smoothie',
    disable = true,
    config = function()
      vim.g.smoothie_enabled = false
    end
  }
  use 'tpope/vim-abolish'
  use {
    'easymotion/vim-easymotion',
    disable = true,
  }
  use {
    'phaazon/hop.nvim',
    branch = 'master',
    config = function()
      require('hop').setup({})
      vim.api.nvim_set_keymap('n', '<leader>a', "<cmd>lua require'hop'.hint_words()<cr>", {})
    end,
    disable = false
  }
  use {
    'norcalli/nvim-colorizer.lua',
    config = function()
      require('colorizer').setup()
    end
  }
  use 'kevinhwang91/nvim-hlslens'
  use 'jeffkreeftmeijer/vim-numbertoggle'
  use {
    'nacro90/numb.nvim',
    config = function()
      require('numb').setup()
    end
  }
  use {
    "folke/which-key.nvim",
    config = function()
      require("which-key").setup {
        -- your configuration comes here
        -- or leave it empty to use the default settings
        -- refer to the configuration section below
      }
    end
  }
  use {
    'sudormrfbin/cheatsheet.nvim',
    event = "VimEnter",
    requires = {
      'nvim-telescope/telescope.nvim',
      'nvim-lua/popup.nvim',
      'nvim-lua/plenary.nvim',
    },
    config = function()
      vim.cmd [[ nnoremap <leader>c <cmd>Cheatsheet<cr> ]]
    end
  }

  -- Marks
  use 'kshenoy/vim-signature'
  -- Registers
  -- use 'junegunn/vim-peekaboo'
  use 'gennaro-tedesco/nvim-peekup'

  -- Session
  use 'tpope/vim-obsession'
  use {
    "folke/persistence.nvim",
    event = "BufReadPre", -- this will only start session saving when an actual file was opened
    module = "persistence",
    config = function()
      require("persistence").setup()
    end,
  }
  use {
    'goolord/alpha-nvim',
    requires = { 'kyazdani42/nvim-web-devicons' },
    config = function ()
      require('alpha').setup(require'alpha.themes.dashboard'.opts)
    end,
    disable = true,
  }

  -- Buffers
  use 'jeetsukumaran/vim-buffergator'

  -- Comments
  use { 'b3nj5m1n/kommentary', branch = 'main' }

  -- Completion
  use {
    'ms-jpq/coq_nvim',
    branch = 'coq',
    -- event = "InsertEnter",
    config = function()
      vim.g.coq_settings = {
        auto_start = 'shut-up',
        keymap = {
          jump_to_mark = "<c-x>",
          bigger_preview = "<c-x>",
        }
      }
    end,
  } -- main one
  use {
    'ms-jpq/coq.artifacts',
    branch = 'artifacts',
    requires = 'ms-jpq/coq_nvim',
  } -- 9000+ Snippets

  -- File system
  use 'tpope/vim-eunuch'
  use 'airblade/vim-rooter'
  -- use 'francoiscabrol/ranger.vim'
  use {
    'kevinhwang91/rnvimr',
    config = function() 
      vim.cmd [[ let g:rnvimr_enable_picker = 1 ]]
      vim.cmd [[ let g:rnvimr_draw_border = 0 ]]
      vim.cmd [[ nnoremap <leader>f <cmd>RnvimrToggle<cr> ]]
    end
  }
  use {
    'kyazdani42/nvim-tree.lua',
    branch = 'master',
    requires = 'kyazdani42/nvim-web-devicons',
    config = function()
      -- vim.cmd [[ nnoremap <leader>v <cmd>NvimTreeToggle<cr> ]]
      vim.cmd [[ nnoremap <leader>v <cmd>NvimTreeFindFile<cr> ]]
    end,
    disable = false
  }

  -- Tree-sitter
  use {
    'nvim-treesitter/nvim-treesitter',
    event = "BufRead",
    config = function()
      require('nvim-treesitter.configs').setup {
        ensure_installed = "maintained", -- One of "all", "maintained" (parsers with maintainers), or a list of languages
        highlight = {
          enable = true                  -- False will disable the whole extension
        },
        incremental_selection = {
          enable = true,
          keymaps = {
            init_selection = "gnn",
            node_incremental = "grn",
            scope_incremental = "grc",
            node_decremental = "grm",
          },
        },
      }
    end
    -- run = [[ :TSUpdate ]]
  }
  use {
    'David-Kunz/treesitter-unit',
    requires = 'nvim-treesitter/nvim-treesitter',
    event = "BufRead",
    config = function()
      -- vim.api.nvim_set_keymap('v', 'x', ':lua require"treesitter-unit".select()<CR>', {noremap=true})
      -- vim.api.nvim_set_keymap('o', 'x', ':<c-u>lua require"treesitter-unit".select()<CR>', {noremap=true})
    end,
  }
  use {
    'romgrk/nvim-treesitter-context',
    requires = 'nvim-treesitter/nvim-treesitter',
    event = "BufRead",
    config = function()
      require('treesitter-context').setup({
        enable = true,
        throttle = true,
    })
    end,
    disable = false,
  }

  -- Development
  use {
    'haringsrob/nvim_context_vt',
    disable = true,
  }
  use 'machakann/vim-sandwich'
  use {
    'code-biscuits/nvim-biscuits',
    disable = true,
    config = function()
      require('nvim-biscuits').setup({})
    end
  }
  -- Replaced with treesitter
  -- use 'sheerun/vim-polyglot'
  use 'elixir-editors/vim-elixir'

  -- Snippets
  use 'hrsh7th/vim-vsnip'
  use 'hrsh7th/vim-vsnip-integ'

  -- Formatting
  use {
    'sbdchd/neoformat',
    config = function()
      vim.g.neoformat_enabled_yaml = {}
    end,
    disable = false,
  }

  -- Languages
  use {
    'mrjones2014/dash.nvim',
    requires = {
      'nvim-telescope/telescope.nvim',
      'nvim-lua/plenary.nvim'
    },
    rocks = { 'xml2lua' }
  }

  -- Clojure
  use {
    'eraserhd/parinfer-rust',
    run = "cargo build --release",
  }

  -- Rust
  use {
    'simrat39/rust-tools.nvim',
    config = function()
      require('rust-tools').setup({})
    end
  }

  -- Markdown
  use 'npxbr/glow.nvim'
end)
