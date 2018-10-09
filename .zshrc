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

# setopt clobber

EDITOR=nvim
VISUAL=nvim
HOMEBREW_EDITOR=nvim

alias cls='clear && printf "\e[3J"'

. ~/.config/zsh/plugins/vi-mode.plugin.zsh
. ~/.config/zsh/plugins/colored-man-pages.plugin.zsh

# Homebrew
HOMEBREW_NO_ANALYTICS=1
export PATH="/usr/local/sbin:$PATH"

# asdf
. $HOME/.asdf/asdf.sh
. $HOME/.asdf/completions/asdf.bash

# ssh
#eval "$(ssh-agent -s)"
#ssh-add -K ~/.ssh/id_rsa

# fzf
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
export FZF_DEFAULT_COMMAND='rg --files --no-ignore --hidden --follow --glob "!.git/*"'

# Python
## Use homebrew `python` package
#export PATH="/usr/local/opt/python/libexec/bin:$PATH"
#export PATH="/usr/local/opt/python@2/bin:$PATH"

# JavaScript
#export PATH="$HOME/.asdf/installs/nodejs/9.7.1/.npm/bin:$PATH"

# Rust
#export PATH="$PATH:$HOME/.cargo/bin"
#export RUST_SRC_PATH=$(rustc --print sysroot)/lib/rustlib/src/rust/src

# Golang
#export PATH="$PATH:$HOME/.asdf/shims/go"
#export GOPATH="$HOME/go"
#export GOROOT="$ASDF_DIR/installs/golang/1.10/go"
# OR
# https://github.com/kennyp/asdf-golang/pull/2
# .asdf/plugins/golang/bin/exec-env
# if [ "$GOROOT" = "" ] ; then
#    export GOROOT=$ASDF_INSTALL_PATH/go
#fi
#
#if [ "$GOPATH" = "" ] ; then
#    export GOPATH=$ASDF_INSTALL_PATH/packages
#  fi
#export GOPATH=~/
#export PATH=$PATH:$GOPATH:$GOPATH/bin
# export PATH=$PATH:$HOME/bin

# From `go env`
# GOPATH="/Users/jesse/.asdf/installs/golang/1.11/packages"
# GOROOT="/Users/jesse/.asdf/installs/golang/1.11/go"
export GOPATH=~/src

export PATH=$GOPATH/bin:$GOROOT/bin:$PATH

# terminal
alias dots='$(which git) --git-dir=$HOME/.dots.git/ --work-tree=$HOME'

# Neovim
#export NVIM_TUI_ENABLE_CURSOR_SHAPE=1

# Who's listening
# https://stackoverflow.com/questions/4421633/who-is-listening-on-a-given-tcp-port-on-mac-os-x
alias ears="sudo lsof -i -n -P | grep TCP"

# Prints process with the most open files
# https://www.reddit.com/r/osx/comments/3ewn93/too_many_open_files_in_system_what_is_causing_this/
#alias mostopenfiles="sudo lsof -n | perl -pe '$x = <>; while(<>) { ($cmd, $pid, $rest) = split(/\s+/); $cmds{$pid} = $cmd; $pids{$pid}++;} while( ($key, $val) = each %pids) { if ($val > $max) { $max = $val; $maxpid = $key; } } print "pid: $maxpid ($cmds{$maxpid}) has the most ($max) filedescriptors \n";'"

# Docker
alias ds='docker ps'
alias dk='docker kill $(docker ps -q)'
alias dr='docker rm -f $(docker ps -q)'
function di() {
  docker inspect -f '{{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}' $1
}
#
# Find/Replace in files
# https://stackoverflow.com/questions/9704020/recursive-search-and-replace-in-text-files-on-mac-and-linux
# find . -type f -name '*.elm' -exec sed -i '' s/PNode/PerimeterNode/ {} +
function far() {
  find . -name '*.$1' -print0 | xargs -0 sed -i "" "s/$2/$3/g"
}

# Dev
alias uuidgenlower='uuidgen | tr -d '\n' | tr "[:upper:]" "[:lower:]"'
#
# Google Chrome
alias chrome="/Applications/Google\ Chrome.app/Contents/MacOS/Google\ Chrome"
alias chrome-canary="/Applications/Google\ Chrome\ Canary.app/Contents/MacOS/Google\ Chrome\ Canary"
alias chromium="/Applications/Chromium.app/Contents/MacOS/Chromium"

# iTerm
# $1 = type; 0 - both, 1 - tab, 2 - title
# rest = text
setTerminalText () {
    # echo works in bash & zsh
    local mode=$1 ; shift
    echo -ne "\033]$mode;$@\007"
}
stt_both  () { setTerminalText 0 $@; }
stt_tab   () { setTerminalText 1 $@; }
stt_title () { setTerminalText 2 $@; }

# ripgrep
alias rg="rg --smart-case --pretty"

# Lua
# Generate with luarocks path --bin (https://github.com/luarocks/luarocks/wiki/Using-LuaRocks)
# export LUA_PATH='/Users/jesse/.luarocks/share/lua/5.3/?.lua;/Users/jesse/.luarocks/share/lua/5.3/?/init.lua;/Users/jesse/.asdf/installs/lua/5.3.3/luarocks/share/lua/5.3/?.lua;/Users/jesse/.asdf/installs/lua/5.3.3/luarocks/share/lua/5.3/?/init.lua;/usr/local/share/lua/5.3/?.lua;/usr/local/share/lua/5.3/?/init.lua;/usr/local/lib/lua/5.3/?.lua;/usr/local/lib/lua/5.3/?/init.lua;./?.lua;./?/init.lua;/Users/jesse/.asdf/installs/lua/5.3.3/share/lua/5.3/?.lua;/Users/jesse/.asdf/installs/lua/5.3.3/share/lua/5.3/?/init.lua'
# export LUA_CPATH='/Users/jesse/.luarocks/lib/lua/5.3/?.so;/Users/jesse/.asdf/installs/lua/5.3.3/luarocks/lib/lua/5.3/?.so;/usr/local/lib/lua/5.3/?.so;/usr/local/lib/lua/5.3/loadall.so;./?.so;/Users/jesse/.asdf/installs/lua/5.3.3/lib/lua/5.3/?.so'
# export PATH=$PATH:$HOME/.luarocks/bin:$HOME/.asdf/installs/lua/5.3.3/luarocks/bin


# Work
