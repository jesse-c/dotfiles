function shellhistory_pick --description "Fuzzy-search ShellHistory; Ctrl-R toggles CWD/global"
    set -l query (commandline --current-buffer | string collect)
    set -l scope cwd
    set -lx FZF_DEFAULT_OPTS ''

    # shhist search is line-oriented; multiline entries appear as separate lines.
    while true
        set -l scope_args
        if test "$scope" = cwd
            set scope_args --current-pwd
        end

        set -l result (
            command shhist search $scope_args \
                --no-timestamps --no-highlights --limit 50000 |
            command fzf \
                --height=80% --layout=reverse --border \
                --no-sort --no-multi \
                --query="$query" \
                --prompt="  $scope > " \
                --header='Ctrl-R: CWD/global · Enter: insert · Esc: cancel' \
                --print-query --expect=ctrl-r |
            string split \n
        )

        if test (count $result) -lt 2
            break
        end

        set query "$result[1]"

        if test "$result[2]" = ctrl-r
            if test "$scope" = cwd
                set scope global
            else
                set scope cwd
            end
            continue
        end

        if test (count $result) -ge 3
            commandline --replace -- "$result[3]"
        end
        break
    end

    commandline -f repaint
end
