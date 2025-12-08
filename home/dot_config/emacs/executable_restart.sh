#!/bin/bash

echo "Stopping Emacs daemon..."
~/.config/emacs/stop.sh

# Wait a moment for cleanup
sleep 1

echo "Starting Emacs daemon..."
~/.config/emacs/start.sh
