#!/bin/bash

set -e -o pipefail

IN="$(mktemp -d)"

echo "Downloading to $IN"

wget https://github.com/catppuccin/k9s/archive/main.tar.gz -P "$IN"

echo "Downloaded!"

OUT="$(pwd)/skins/"

echo "Extracting to $OUT"

tar -xzvf "$IN/main.tar.gz" --strip-components=2 --exclude='*.webp' -C "$OUT"

echo "Extracted!"

echo "Cleaning up $IN"

rm -rf "$IN"

echo "Cleaned up!"
