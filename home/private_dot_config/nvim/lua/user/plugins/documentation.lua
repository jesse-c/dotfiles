return {
    -- A better annotation generator. Supports multiple languages and annotation conventions.
    {
        "danymat/neogen",
        config = function() require("neogen").setup({}) end,
        dependencies = "nvim-treesitter/nvim-treesitter"
    }
}
