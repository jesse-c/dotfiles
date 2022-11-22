#!/usr/bin/env bash

# https://unix.stackexchange.com/a/28794/273402
[ "$UID" -eq 0 ] || exec sudo bash "$0" "$@"

# https://unix.stackexchange.com/a/433806/273402

# Now, $dry_run will be true if the user invoked the script with the -n command line option.
dry_run=false

while getopts 'n' opt; do
  case "$opt" in
  n) dry_run=true ;;
  *)
    echo 'error in command line parsing' >&2
    exit 1
    ;;
  esac
done

if "$dry_run"; then
  cmd='echo'
else
  cmd=''
fi

for f in /Users/jesse/.local/share/nvim/mason/bin/*; do
  echo "Linking $(basename "$f")"
  dest=/usr/local/bin/$(basename "$f")
  $cmd ln -s "$f" "$dest"
  chmod u+x "$dest"
done

ls -1 /Users/jesse/.local/share/nvim/mason/bin/ > commands.log

# Example output:
#
# Linking bash-language-server
# ln: /usr/local/bin/bash-language-server: File exists
# Linking clojure-lsp
# ln: /usr/local/bin/clojure-lsp: File exists
# Linking elixir-ls
# Linking gopls
# Linking lemminx
# Linking ltex-cli
# Linking ltex-ls
# Linking lua-language-server
# Linking prosemd-lsp
# Linking pylsp
# Linking rust-analyzer
# Linking solargraph
# Linking sqls
# Linking srb
# Linking typescript-language-server
# Linking vscode-html-language-server
# Linking vscode-json-language-server
# Linking yaml-language-server
