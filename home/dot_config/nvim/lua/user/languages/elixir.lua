local ok, elixir = pcall(require, "elixir")
if not ok then
  return
end

local ok, elixirls = pcall(require, "elixir.elixirls")
if not ok then
  return
end

elixir.setup({
  nextls = { enable = true },
  credo = { enable = true },
  elixirls = {
    enable = false,
    settings = elixirls.settings({
      dialyzerEnabled = true,
      enableTestLenses = false,
    }),
    on_attach = function(client, bufnr)
      vim.keymap.set("n", "<space>fp", ":ElixirFromPipe<cr>", { buffer = true, noremap = true })
      vim.keymap.set("n", "<space>tp", ":ElixirToPipe<cr>", { buffer = true, noremap = true })
      vim.keymap.set("v", "<space>em", ":ElixirExpandMacro<cr>", { buffer = true, noremap = true })
    end,
  },
})
