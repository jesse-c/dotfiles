#!/usr/bin/env bash

rm -rf ~/.config/emacs/{eln-cache,elpa,elpaca}
fd -e elc -x rm {} ~/.config/emacs
