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

export GPG_TTY=(tty)

# Go
export GO111MODULE=on

# ripgrep
# alias rg='rg --smart-case --pretty'

# Git
alias cdg='cd (git rev-parse --show-toplevel)'

# Emacs
export EMACS_USER_DIRECTORY=~/.emacs.d

# Homebrew
export HOMEBREW_NO_ANALYTICS=1

# asdf
source /usr/local/opt/asdf/asdf.fish

# direnv
direnv hook fish | source

# Local ------------------------------------------------------------------------

if test -e ~/local.sh
  source ~/local.sh
end
