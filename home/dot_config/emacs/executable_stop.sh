#!/bin/bash

/Applications/Emacs.app/Contents/MacOS/bin/emacsclient -e "(kill-emacs)"

if [ $? -eq 0 ]; then
  echo "Emacs daemon stopped successfully"
else
  echo "Failed to stop Emacs daemon (may not be running)"
  exit 1
fi
