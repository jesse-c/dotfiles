return {
    -- magit for neovim
    {
        "NeogitOrg/neogit",
        event = "BufRead",
        branch = "master",
        dependencies = {"nvim-lua/plenary.nvim", "sindrets/diffview.nvim"},
        config = function()
            require("neogit").setup({integrations = {diffview = true}})
        end
    },
    -- Single tabpage interface for easily cycling through diffs for all modified files for any git rev.
    {"sindrets/diffview.nvim", event = "BufRead"},
    -- Super fast git decorations implemented purely in lua/teal.
    {
        "lewis6991/gitsigns.nvim",
        dependencies = "nvim-lua/plenary.nvim",
        -- tag = "release", -- To use the latest release
        event = "BufRead",
        config = function()
            require("gitsigns").setup({current_line_blame = true})
        end
    }, -- A Git wrapper so awesome, it should be illegal
    {"tpope/vim-fugitive", event = "BufRead"}
}
