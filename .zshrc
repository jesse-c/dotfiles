# Change default zim location
export ZIM_HOME=${ZDOTDIR:-${HOME}}/.zim

# Start zim
[[ -s ${ZIM_HOME}/init.zsh ]] && source ${ZIM_HOME}/init.zsh

# zsh
bindkey -v

HISTFILE=$HOME/.zhistory
setopt APPEND_HISTORY
setopt HIST_IGNORE_SPACE
setopt EXTENDED_HISTORY
setopt HIST_EXPIRE_DUPS_FIRST
HISTSIZE=10000
SAVEHIST=10000
bindkey '^R' history-incremental-search-backward

REPORTTIME=30

EDITOR=nvim
VISUAL=nvim
HOMEBREW_EDITOR=nvim

alias bash='/usr/local/bin/bash'
alias zsh='/usr/local/bin/zsh'

. ~/.config/zsh/plugins/vi-mode.plugin.zsh
. ~/.config/zsh/plugins/colored-man-pages.plugin.zsh

# Homebrew
HOMEBREW_NO_ANALYTICS=1

# asdf
. $HOME/.asdf/asdf.sh
. $HOME/.asdf/completions/asdf.bash

# fzf
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
export FZF_DEFAULT_COMMAND='rg --files --no-ignore --hidden --follow --glob "!.git/*"'

# Config
alias dots='$(which git) --git-dir=$HOME/.dots.git/ --work-tree=$HOME'

# Utils
alias cls='clear && printf "\e[3J"'
alias uuid='uuidgen | tr -d '\n' | pbcopy'
alias ears='sudo lsof -i -n -P | grep TCP' # https://stackoverflow.com/questions/4421633/who-is-listening-on-a-given-tcp-port-on-mac-os-x
#alias mostopenfiles="sudo lsof -n | perl -pe '$x = <>; while(<>) { ($cmd, $pid, $rest) = split(/\s+/); $cmds{$pid} = $cmd; $pids{$pid}++;} while( ($key, $val) = each %pids) { if ($val > $max) { $max = $val; $maxpid = $key; } } print "pid: $maxpid ($cmds{$maxpid}) has the most ($max) filedescriptors \n";'" # Prints process with the most open files  - https://www.reddit.com/r/osx/comments/3ewn93/too_many_open_files_in_system_what_is_causing_this/
# Find/Replace in files
# https://stackoverflow.com/questions/9704020/recursive-search-and-replace-in-text-files-on-mac-and-linux
# find . -type f -name '*.elm' -exec sed -i '' s/PNode/PerimeterNode/ {} +
function far() {
  find . -name '*.$1' -print0 | xargs -0 sed -i "" "s/$2/$3/g"
}

# Docker
alias ds='docker ps'
alias dk='docker kill $(docker ps -q)'
alias dr='docker rm -f $(docker ps -q)'
function di() {
  docker inspect -f '{{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}' $1
}

# Dev
alias uuidgenlower='uuidgen | tr -d '\n' | tr "[:upper:]" "[:lower:]"'

# Google Chrome
alias chrome="/Applications/Google\ Chrome.app/Contents/MacOS/Google\ Chrome"
alias chrome-canary="/Applications/Google\ Chrome\ Canary.app/Contents/MacOS/Google\ Chrome\ Canary"
alias chromium="/Applications/Chromium.app/Contents/MacOS/Chromium"

# ripgrep
alias rg="rg --smart-case --pretty"

# git
alias cdg='cd $(git rev-parse --show-toplevel)'

# emacs
export EMACS_USER_DIRECTORY=~/.emacs.d

# local
if [ -f ~/local.sh ]; then
  source ~/local.sh
fi
