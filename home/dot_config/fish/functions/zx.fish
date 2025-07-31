function zx -d "List Git repositories and navigate to one"
    cd (fd --hidden --no-ignore -d 4 --type d '.git$' ~/src | xargs -I{} dirname {} | sk)
end
