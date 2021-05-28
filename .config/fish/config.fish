set --universal fish_greeting ""

# dots -------------------------------------------------------------------------

alias dots='git --git-dir=$HOME/.dots.git/ --work-tree=$HOME'

# Visual -----------------------------------------------------------------------

alias cls='clear && printf "\e[3J"'

export CLICOLOR=1

# Applications -----------------------------------------------------------------

# Raycast
alias ray='~/src/github.com/raycast-api/api-alpha/cli/x86/ray'

# Git
alias g='git'
alias gsw='git checkout (git branch | fzf | cut -c 3-)'
alias cdg='cd (git rev-parse --show-toplevel)'

export GPG_TTY=(tty)

# Go
export GO111MODULE=on

# ripgrep
# alias rg='rg --smart-case --pretty'

# Emacs
export EMACS_USER_DIRECTORY=~/.emacs.d

# Homebrew
export HOMEBREW_NO_ANALYTICS=1

# asdf
source /usr/local/opt/asdf/asdf.fish

# direnv
direnv hook fish | source

# zoxide
zoxide init fish | source

# https://gist.github.com/zlksnk/81a4993be410586c038f8b3fc140b1c7
function up
  set -l dir_to_be_child $argv[1]

  if [ "$dir_to_be_child" = "" ]
    cd ..
  else
    cd -
    set -l alt_dir $PWD
    cd -
    set -l curr_dir $PWD

    set -l is_found 0
    while [ "$PWD" != "/" ]; and test $is_found -eq 0
      cd ..
      set -l res (find $dir_to_be_child -maxdepth 0 -type d 2>/dev/null | wc -l)
      if test $res -eq 1
        set is_found 1
      end
    end

    if [ "$PWD" = "/" ]
      cd $alt_dir
      cd $curr_dir
      echo "Couldn't find $dir_to_be_child"
      return 1
    end

    set -l found_dir $PWD
    cd $curr_dir
    cd $found_dir
  end
end

# Local ------------------------------------------------------------------------

if test -e ~/local.sh
  source ~/local.sh
end
