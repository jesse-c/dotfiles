local user = require("user")
user.setup()
local use = user.use

-- user.nvim needs to manage itself!
use 'faerryn/user.nvim'

-- Miscellaneous
use 'nvim-lua/popup.nvim'
use 'nvim-lua/plenary.nvim'

-- Theme
use 'bluz71/vim-moonfly-colors'
use {
  'sainnhe/edge',
  config = function()
    local o = vim.o -- For the globals options

    -- o.edge_style = 'default'
    -- o.edge_enable_italic = false
    -- o.edge_disable_italic_comment = true
  end
}

-- VCS
use 'tpope/vim-fugitive'
use 'mhinz/vim-signify'
use {
  'APZelos/blamer.nvim',
  config = function()
    vim.g.blamer_enabled = true
  end
}
use {
  'pwntester/octo.nvim',
  after = 'nvim-telescope/telescope.nvim',
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
    require("trouble").setup()
  end
}

-- Testing
use 'vim-test/vim-test'
use 'rcarriga/vim-ultest' -- :UpdateRemotePlugins

-- UI
use 'nvim-telescope/telescope.nvim'
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
  after = 'kyazdani42/nvim-web-devicons',
  config = function()
    require('bufferline').setup{}
  end
}
use 'chrisbra/Colorizer'
use {
  'Yggdroot/indentLine',
  config = function()
    vim.g.indentLine_enabled = true
    -- vim.g.indentLine_showFirstIndentLevel = 1
    vim.g.indentLine_setColors = false
    --"vim.g.indentLine_char = '┆'
  end
}
use {
  'psliwka/vim-smoothie',
  disabled = true,
  config = function()
    vim.g.smoothie_enabled = false
  end
}
use 'tpope/vim-abolish'
use 'easymotion/vim-easymotion'
use {
  'norcalli/nvim-colorizer.lua',
  config = function()
    require('colorizer').setup()
  end
}
use 'kevinhwang91/nvim-hlslens'
-- use 'jeffkreeftmeijer/vim-numbertoggle'
use {
  'nacro90/numb.nvim',
  config = function()
    require('numb').setup()
  end
}
-- Marks
use 'kshenoy/vim-signature'
-- Registers
use 'junegunn/vim-peekaboo'
-- use 'gennaro-tedesco/nvim-peekup'

-- Session
use 'tpope/vim-obsession'

-- Buffers
use 'jeetsukumaran/vim-buffergator'

-- Comments
use { 'b3nj5m1n/kommentary', branch = 'main' }

-- Completion
use 'hrsh7th/nvim-compe'

-- File system
use 'tpope/vim-eunuch'
use 'airblade/vim-rooter'
use 'francoiscabrol/ranger.vim'
use {
  'ms-jpq/chadtree',
  branch = 'chad',
  config = function()
    vim.cmd [[ nnoremap <leader>v <cmd>CHADopen<cr> ]]
  end
  -- run = [[ python3 -m chadtree deps ]] }
}

-- Tree-sitter
use {
  'nvim-treesitter/nvim-treesitter',
  config = function()
    require('nvim-treesitter.configs').setup {
      ensure_installed = "maintained", -- One of "all", "maintained" (parsers with maintainers), or a list of languages
      highlight = {
        enable = true                  -- False will disable the whole extension
      },
    }
  end
  -- run = [[ :TSUpdate ]]
}

-- Development
-- Replaced with treesitter
-- use 'sheerun/vim-polyglot'
use 'elixir-editors/vim-elixir'

-- Snippets
use 'hrsh7th/vim-vsnip'
use 'hrsh7th/vim-vsnip-integ'

-- Formatting
use 'sbdchd/neoformat'

-- Languages
-- Clojure
use 'eraserhd/parinfer-rust' -- run = [[ cargo build --release ]] }

user.startup()
