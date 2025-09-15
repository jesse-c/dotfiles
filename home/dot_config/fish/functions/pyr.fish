function pyr -d "Run within Python tooling"
    if test -f pyproject.toml -a -f poetry.lock
        poetry run $argv
    else if test -f pyproject.toml -a -f uv.lock
        uv run $argv
    else
        echo "No pyproject.toml found - neither poetry nor uv project detected"
        return 1
    end
end

function pyrp -d "Run within Python interpreator in Python tooling"
    pyr python $argv
end

function pyri -d "Run IPython interpreator in Python tooling"
    pyr ipython
end
