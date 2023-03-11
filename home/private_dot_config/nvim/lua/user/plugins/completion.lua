return {
    -- A completion plugin for neovim coded in Lua.
    {
        "hrsh7th/nvim-cmp",
        dependencies = {"neovim/nvim-lspconfig", "onsails/lspkind.nvim"},
        config = function() require("user.completion").setup() end
    }, {"hrsh7th/cmp-path", dependencies = "hrsh7th/nvim-cmp"},
    {"ray-x/cmp-treesitter", dependencies = "hrsh7th/nvim-cmp"},
    {"hrsh7th/cmp-nvim-lsp", dependencies = "hrsh7th/nvim-cmp"},
    {"hrsh7th/cmp-buffer", dependencies = "hrsh7th/nvim-cmp"},
    {"hrsh7th/cmp-cmdline", dependencies = "hrsh7th/nvim-cmp"},
    {"saadparwaiz1/cmp_luasnip", dependencies = "L3MON4D3/LuaSnip"}
}
