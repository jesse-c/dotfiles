#!/bin/bash

/Applications/Emacs.app/Contents/MacOS/bin/emacsclient --socket-name ~/.config/emacs/server/server -n -c "$@"

if [ $? -ne 0 ]; then
  echo "Failed to connect to Emacs daemon"
  echo "Make sure the daemon is running: ./start.sh"
  exit 1
fi
