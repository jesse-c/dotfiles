#!/bin/sh
# Update origin's default branch locally, then sync the current checkout.
set -eu

die() {
    printf 'git sync: %s\n' "$*" >&2
    exit 1
}

assert_no_operation() {
    directory=$1
    for operation in rebase-merge rebase-apply MERGE_HEAD CHERRY_PICK_HEAD REVERT_HEAD BISECT_START; do
        state=$(git -C "$directory" rev-parse --path-format=absolute --git-path "$operation") ||
            die "Cannot inspect $directory."
        [ ! -e "$state" ] || die "Finish or abort the operation in $directory first."
    done
}

[ "$#" -eq 0 ] || die 'Usage: git sync (no arguments).'
root=$(git rev-parse --show-toplevel) || die 'Not in a working tree.'
cd "$root"
assert_no_operation "$root"
git rev-parse --verify HEAD >/dev/null 2>&1 || die 'The current checkout has no commit yet.'
current_branch=$(git symbolic-ref --quiet --short HEAD) || current_branch=

git fetch origin || die 'Fetching origin failed.'
# Refresh the symbolic ref too: the remote may have changed its default branch.
git remote set-head origin --auto >/dev/null || die "Cannot resolve origin's default branch."
remote_ref=$(git symbolic-ref --quiet refs/remotes/origin/HEAD) ||
    die "Cannot resolve origin's default branch."
case "$remote_ref" in
    refs/remotes/origin/*) default_branch=${remote_ref#refs/remotes/origin/} ;;
    *) die "Unexpected origin HEAD: $remote_ref" ;;
esac
local_ref=refs/heads/$default_branch
tip=$(git rev-parse --verify "${remote_ref}^{commit}") || die "Cannot resolve $remote_ref."

if old_tip=$(git rev-parse --verify "$local_ref" 2>/dev/null); then
    # Never reset unpublished commits on the default branch.
    git merge-base --is-ancestor "$old_tip" "$tip" ||
        die "$default_branch has local commits or has diverged from origin/$default_branch; refusing to reset it."
    default_worktree=$(git for-each-ref --format='%(worktreepath)' "$local_ref")
    if [ -n "$default_worktree" ]; then
        # Moving a checked-out branch ref alone would leave its index/files stale.
        # This also supports a default branch checked out in another worktree.
        assert_no_operation "$default_worktree"
        git -C "$default_worktree" merge --ff-only --autostash "$remote_ref" ||
            die "Cannot fast-forward $default_branch in $default_worktree."
    elif [ "$old_tip" != "$tip" ]; then
        git update-ref -m "sync: fast-forward to origin/$default_branch" "$local_ref" "$tip" "$old_tip" ||
            die "Cannot update $default_branch (it may have changed concurrently)."
    fi
else
    git branch --track -- "$default_branch" "origin/$default_branch" ||
        die "Cannot create local $default_branch."
fi

if [ -z "$current_branch" ]; then
    # Intentionally stay detached. The previous checkout remains in HEAD's reflog.
    # Local changes come along, unless they clash, in which case nothing moves.
    git switch --detach "$remote_ref" ||
        die "Cannot move to origin/$default_branch without overwriting local changes."
elif [ "$current_branch" != "$default_branch" ]; then
    # Only rewrite this branch, even when rebase.updateRefs is enabled.
    # Local changes are stashed and restored once the rebase finishes or is aborted.
    git rebase --no-update-refs --autostash "$remote_ref" ||
        die "$default_branch is updated; resolve the rebase, or run git rebase --abort."
fi
