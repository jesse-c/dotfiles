#!/usr/bin/env bash

rm -rf ~/.config/emacs/{eln-cache,elpa,elpaca}
fd . ~/.config/emacs -e elc -x rm
