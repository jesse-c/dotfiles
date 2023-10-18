#!/usr/bin/env bash

today=$(date +\%Y\%m\%d)

loc=$HOME/Dropbox/backups/arch-linux/packages/$today

mkdir -p $loc

pacman -Qqe | grep -v "$(pacman -Qqm)" > $loc/official.lst
pacman -Qqm > $loc/aur.list
