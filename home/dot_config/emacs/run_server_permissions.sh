#!/bin/sh

FILE="$HOME/.config/emacs/server"
if [ -f "$FILE" ]; then
  if [ "$(stat -c %a "$FILE")" != "700" ]; then
    chmod 700 "$FILE"
  fi
fi
