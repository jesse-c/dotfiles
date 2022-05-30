#!/usr/bin/env bash

# A lazy setup to alias LSPs installed through nvim-lsp-installer

a='((~/.local/share/nvim/lsp_servers/ltex/ltex-ls/bin/ltex-ls,ltex-ls), (~/.local/share/nvim/lsp_servers/prosemd_lsp/prosemd-lsp,prosemd-lsp), (~/.local/share/nvim/lsp_servers/sqls/sqls,sqls), (~/.local/share/nvim/lsp_servers/tsserver/node_modules/.bin/typescript-language-server,typescript-language-server), (~/.local/share/nvim/lsp_servers/bashls/node_modules/.bin/bash-language-server,bash-language-server), (~/.local/share/nvim/lsp_servers/pylsp/venv/bin/pylsp,pylsp))'
IFS='()'; for t in $a; do [ -n "$t" ] && { IFS=','; set -- $t; [ -n "$1" ] && ln -s $1 "/usr/local/bin/$2"; }; done
