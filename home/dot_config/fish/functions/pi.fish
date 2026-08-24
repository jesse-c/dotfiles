function pi -d "Run pi under a Node with zlib zstd (22.15+), falling back to a mise-installed one"
    set -l min 22015 # major * 1000 + minor

    set -l v (node --version 2>/dev/null | string trim -l -c v | string split .)
    if test (count $v) -ge 2; and test (math "$v[1] * 1000 + $v[2]") -ge $min
        command pi $argv
        return
    end

    set -l best
    set -l best_n 0
    for ver in (mise ls node --installed --no-header 2>/dev/null | string replace -rf '^\S+\s+(\S+).*' '$1')
        set -l p (string split . $ver)
        test (count $p) -ge 2; or continue

        set -l n (math "$p[1] * 1000 + $p[2]")
        if test $n -ge $min; and test $n -gt $best_n
            set best $ver
            set best_n $n
        end
    end

    if test -n "$best"
        mise exec node@$best -- pi $argv
    else
        command pi $argv
    end
end
