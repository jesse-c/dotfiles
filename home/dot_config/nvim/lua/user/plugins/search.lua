return {
  -- display number of search matches & index of a current match
  { "google/vim-searchindex", event = "BufRead" },
  -- Smart substitution
  { "tpope/vim-abolish", event = "BufRead" },
  -- sandwich.vim is a set of operator and textobject plugins to add/delete/replace surroundings of a sandwiched textobject, like (foo), "bar".
  { "machakann/vim-sandwich", event = "BufRead" },
  {
    "phaazon/hop.nvim",
    branch = "master",
    event = "BufRead",
    config = function()
      require("hop").setup({})
    end,
  },
}
