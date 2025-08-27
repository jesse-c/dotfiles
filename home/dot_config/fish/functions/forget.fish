# Forget the current commandline value
#
# https://www.milanvit.net/post/my-ultimate-shell-setup-with-fish-shell-and-tmux/
function forget -d "Remove a command from shell history"
    set -l cmd (commandline | string collect)
    # Skip if command line is empty
    if test -z "$cmd"
        commandline -f repaint
        return
    end
    printf "\nDo you want to forget '%s'? [Y/n]\n" $cmd
    switch (read | tr A-Z a-z)
        case n no
            commandline -f repaint
            return
        case y yes ''
            history delete --exact --case-sensitive -- $cmd
            commandline ""
            commandline -f repaint
    end
end
