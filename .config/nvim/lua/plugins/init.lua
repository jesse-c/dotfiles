vim.cmd 'packadd paq-nvim'           -- Load package
local paq = require('paq-nvim').paq  -- Import module and bind `paq` function
paq{ 'savq/paq-nvim', opt = true }   -- Let Paq manage itself

-- Miscellaneous
paq 'nvim-lua/popup.nvim'
paq 'nvim-lua/plenary.nvim'

-- Theme
paq 'bluz71/vim-moonfly-colors'

-- VCS
paq 'tpope/vim-fugitive'
paq 'mhinz/vim-signify'
paq 'APZelos/blamer.nvim'

-- LSP
paq 'neovim/nvim-lspconfig'
paq{ 'glepnir/lspsaga.nvim', branch = 'main' }
paq 'kosayoda/nvim-lightbulb'
paq 'liuchengxu/vista.vim'
paq 'onsails/lspkind-nvim'

-- Testing
paq 'vim-test/vim-test'
paq{'rcarriga/vim-ultest', run = [[ :UpdateRemotePlugins ]] }

-- UI
paq 'nvim-telescope/telescope.nvim'
paq 'kyazdani42/nvim-web-devicons'
paq 'tommcdo/vim-lion'
paq 'mbbill/undotree'
paq 'luochen1990/rainbow'
paq 'google/vim-searchindex'
paq 'troydm/zoomwintab.vim'
paq 'hoob3rt/lualine.nvim'
paq 'chrisbra/Colorizer'
paq 'Yggdroot/indentLine'
paq 'psliwka/vim-smoothie'
paq 'tpope/vim-abolish'
paq 'easymotion/vim-easymotion'
paq 'norcalli/nvim-colorizer.lua'
-- paq 'jeffkreeftmeijer/vim-numbertoggle'
paq 'nacro90/numb.nvim'
-- Marks
paq 'kshenoy/vim-signature'
-- Registers
paq 'junegunn/vim-peekaboo'
-- paq 'gennaro-tedesco/nvim-peekup'

-- Session
paq 'tpope/vim-obsession'

-- Buffers
paq 'jeetsukumaran/vim-buffergator'

-- Comments
paq{ 'b3nj5m1n/kommentary', branch = 'main' }

-- Completion
paq 'hrsh7th/nvim-compe'

-- File system
paq 'tpope/vim-eunuch'
paq 'airblade/vim-rooter'
paq 'francoiscabrol/ranger.vim'
paq{ 'ms-jpq/chadtree', branch = 'chad', run = [[ python3 -m chadtree deps ]] }

-- Tree-sitter
paq{ 'nvim-treesitter/nvim-treesitter', run = [[ :TSUpdate ]] }

-- Development
-- Replaced with treesitter
-- paq 'sheerun/vim-polyglot'
paq 'elixir-editors/vim-elixir'

-- Snippets
paq 'hrsh7th/vim-vsnip'
paq 'hrsh7th/vim-vsnip-integ'

-- Formatting
paq 'sbdchd/neoformat'

-- Languages
-- Clojure
paq{ 'eraserhd/parinfer-rust', run = [[ cargo build --release ]] }

----------------
-- Configuration
----------------
require('plugins.psliwka.vim-smoothie')
require('plugins.hoob3rt.lualine-nvim')
require('plugins.norcalli.nvim-colorizer-lua')
require('plugins.APZelos.blamer-nvim')
require('plugins.luochen1990.rainbow')
require('plugins.Yggdroot.indentLine')
require('plugins.nvim-treesitter.nvim-treesitter')
require('plugins.glepnir.lspsaga-nvim')
require('plugins.kosayoda.nvim-lightbulb')
require('plugins.onsails.lspkind-nvim')
require('plugins.neovim.nvim-lspconfig')
require('plugins.ms-jpq.chadtree')
require('plugins.mbbill.undotree')
require('plugins.nacro90.numb-nvim')
