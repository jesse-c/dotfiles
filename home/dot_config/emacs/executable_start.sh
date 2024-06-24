#!/bin/bash

# Start emacs in the background
/Applications/Emacs.app/Contents/MacOS/bin/emacs &

# Get the process ID of the last background job
emacs_pid=$!

# Disown the process
disown $emacs_pid

echo "Emacs started in the background with PID $emacs_pid"
