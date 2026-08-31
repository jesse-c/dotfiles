# Forget the current commandline value
#
# Deletes from fish, atuin, and ShellHistory
# ✓ = deleted, ○ = not found, ✕ = error
function forget -d "Remove a command from shell history"
    set -l cmd (commandline | string collect)
    if test -z "$cmd"
        commandline -f repaint
        return
    end

    set -l escaped (string replace -a "'" "''" $cmd)
    set -l status_parts
    set -l errors

    # Fish built-in history
    set -l fish_err (history delete --exact --case-sensitive -- $cmd 2>&1)
    if test $status -eq 0
        set -a status_parts "✓ fish"
    else
        set -a status_parts "○ fish"
        test -n "$fish_err"; and set -a errors "fish: $fish_err"
    end

    # Atuin
    set -l atuin_db "$HOME/.local/share/atuin/history.db"
    if not test -f "$atuin_db"
        set -a status_parts "✕ atuin"
        set -a errors "atuin: db not found"
    else
        set -l atuin_err (sqlite3 "$atuin_db" "DELETE FROM history WHERE command = '$escaped'; SELECT changes()" 2>&1)
        if test $status -ne 0
            set -a status_parts "✕ atuin"
            set -a errors "atuin: $atuin_err"
        else if test "$atuin_err" -gt 0
            set -a status_parts "✓ atuin"
        else
            set -a status_parts "○ atuin"
        end
    end

    # macOS ShellHistory app
    set -l shellhistory_db "$HOME/Library/Group Containers/4QE86VV38D.app.loshadki.ShellHistory/Application Support/ShellHistory/db.sqlite"
    if not test -f "$shellhistory_db"
        set -a status_parts "✕ shellhistory"
        set -a errors "shellhistory: db not found"
    else
        set -l sh_err (sqlite3 "$shellhistory_db" "DELETE FROM history WHERE cmd = '$escaped'; SELECT changes()" 2>&1)
        if test $status -ne 0
            set -a status_parts "✕ shellhistory"
            set -a errors "shellhistory: $sh_err"
        else if test "$sh_err" -gt 0
            set -a status_parts "✓ shellhistory"
        else
            set -a status_parts "○ shellhistory"
        end
    end

    set -l msg "# forget: "(string join "  " $status_parts)
    if test (count $errors) -gt 0
        set msg "$msg | "(string join "; " $errors)
    end
    commandline "$msg"
    commandline -f execute
end
