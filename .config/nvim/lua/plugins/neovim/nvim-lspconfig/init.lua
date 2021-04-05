local lspconfig = require('lspconfig')

lspconfig.gopls.setup{}
lspconfig.clojure_lsp.setup{}
lspconfig.sourcekit.setup{}
lspconfig.elixirls.setup{
  cmd = { '/Users/jesse/src/github.com/elixir-lsp/elixir-ls/rel/language_server.sh' };
}
