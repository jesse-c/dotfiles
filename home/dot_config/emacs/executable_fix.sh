#!/usr/bin/env bash

set -euo pipefail

# Repair Elpaca package repos that cloned with a valid .git but a damaged
# working tree (tracked files missing -> "Cannot load <pkg>" / "Unhandled
# error in elpaca-queue-dependencies" / "failed dependency: <pkg>"). The .git
# is intact, so we restore the working tree to HEAD instead of re-cloning.
#
# Run this with Emacs stopped, then start Emacs: Elpaca rebuilds the restored
# packages and resolves anything that depended on them.

# Refuse to touch repos while a live Emacs could race the git operations.
# Case-insensitive pgrep catches the macOS "Emacs" app process; -s points
# emacsclient at this config's custom daemon socket.
EMACS_SOCKET=~/.config/emacs/server/server
if pgrep -ix emacs >/dev/null || emacsclient -s "$EMACS_SOCKET" -e t >/dev/null 2>&1; then
  echo "Emacs/daemon is running, stop it first" >&2
  exit 1
fi

elpaca_dir=~/.config/emacs/elpaca

repaired=0
unrecoverable=0

# Elpaca's default clone dir is repos/; this config uses sources/. Check both.
for parent in "$elpaca_dir"/sources "$elpaca_dir"/repos; do
  [ -d "$parent" ] || continue
  for repo in "$parent"/*/; do
    repo=${repo%/}
    [ -d "$repo/.git" ] || continue

    # Can we resolve HEAD? If not, the repo is too broken to restore in place.
    if ! git -C "$repo" rev-parse --verify -q HEAD >/dev/null; then
      echo "UNRECOVERABLE (no HEAD): $repo"
      echo "  -> remove it and let Elpaca re-clone: rm -rf '$repo'"
      unrecoverable=$((unrecoverable + 1))
      continue
    fi

    # Tracked files that exist in HEAD but are missing from the working tree.
    # Catches both staged deletions and plain working-tree deletions.
    if [ -n "$(git -C "$repo" diff --name-only --diff-filter=D HEAD)" ]; then
      echo "Restoring: $(basename "$repo")"
      git -C "$repo" reset --hard HEAD >/dev/null
      repaired=$((repaired + 1))
    fi
  done
done

echo
echo "Repaired $repaired repo(s); $unrecoverable unrecoverable."
if [ "$repaired" -gt 0 ]; then
  echo "Start Emacs to let Elpaca rebuild the restored packages."
fi
