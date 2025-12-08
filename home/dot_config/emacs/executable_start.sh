#!/bin/bash

/Applications/Emacs.app/Contents/MacOS/bin/emacs --daemon

if [ $? -eq 0 ]; then
  echo "Emacs daemon started successfully"
  echo "Connect with: emacsclient -c"
else
  echo "Failed to start Emacs daemon"
  exit 1
fi
