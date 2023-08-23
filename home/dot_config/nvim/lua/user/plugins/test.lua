return {
    -- An extensible framework for interacting with tests within NeoVim.
    {
        "rcarriga/neotest",
        dependencies = {
            "nvim-lua/plenary.nvim", "nvim-treesitter/nvim-treesitter",
            "antoinemadec/FixCursorHold.nvim", -- Adapters
            "jfpedroza/neotest-elixir", "rouge8/neotest-rust",
            "nvim-neotest/neotest-python"
        },
        config = function()
            require("neotest").setup({
                adapters = {
                    require("neotest-elixir"), require("neotest-rust"),
                    require("neotest-python")
                }
            })
        end
    }
}
