return {
    -- Buffers
    -- Neovim plugin for locking a buffer to a window
    {
        "stevearc/stickybuf.nvim",
        config = function() require("stickybuf").setup() end
    }, -- Neovim plugin to stabilize window open/close events
    {
        "luukvbaal/stabilize.nvim",
        config = function() require("stabilize").setup() end
    }
}
