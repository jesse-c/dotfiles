#!/bin/sh

# https://superuser.com/questions/618501/changing-an-applications-icon-from-the-terminal-osx

cp ./dracula.icns /Applications/kitty.app/Contents/Resources/kitty.icns
touch /Applications/kitty.app
