function pyr -d "Run within Python tooling"
    if test -f pyproject.toml -a -f poetry.lock
        poetry run $argv
    else if test -f pyproject.toml -a -f uv.lock
        uv run $argv
    else if test -f pyproject.toml
        poetry run $argv
    else
        echo "No pyproject.toml found - neither poetry nor uv project detected"
        return 1
    end
end

function pyrp
    pyr python $argv
end
