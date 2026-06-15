#!/usr/bin/env bash

set -euo pipefail

# Refuse to nuke while a live Emacs could race the rm. Deleting elpaca/ out
# from under a running daemon clobbers in-flight git checkouts, leaving repos
# with a valid .git but an empty working tree (s.el etc. missing) -> cascading
# elpaca-check-version failures. Recover with fix.sh.
#
# Detection must be robust: the macOS Emacs.app process is named "Emacs"
# (capital), so pgrep is case-insensitive (-i); and the daemon uses a custom
# socket, so emacsclient needs -s to find it. The old guard missed both and
# nuked elpaca under a live daemon.
EMACS_SOCKET=~/.config/emacs/server/server
if pgrep -ix emacs >/dev/null || emacsclient -s "$EMACS_SOCKET" -e t >/dev/null 2>&1; then
  echo "Emacs/daemon is running, stop it first" >&2
  exit 1
fi

rm -rf ~/.config/emacs/{eln-cache,elpa,elpaca}
fd . ~/.config/emacs -e elc -x rm
