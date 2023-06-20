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
  echo "Unlinking $(basename "$f")"
  dest=/usr/local/bin/$(basename "$f")
  $cmd rm "$dest"
done

ls -1 /Users/jesse/.local/share/nvim/mason/bin/ > commands.log

# Example output:
#
# Unlinking bash-language-server
# ln: /usr/local/bin/bash-language-server: File exists
# Unlinking clojure-lsp
# ln: /usr/local/bin/clojure-lsp: File exists
# Unlinking elixir-ls
# Unlinking gopls
# Unlinking lemminx
# Unlinking ltex-cli
# Unlinking ltex-ls
# Unlinking lua-language-server
# Unlinking prosemd-lsp
# Unlinking pylsp
# Unlinking rust-analyzer
# Unlinking solargraph
# Unlinking sqls
# Unlinking srb
# Unlinking typescript-language-server
# Unlinking vscode-html-language-server
# Unlinking vscode-json-language-server
# Unlinking yaml-language-server
