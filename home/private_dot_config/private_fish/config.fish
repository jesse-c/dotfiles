set --universal fish_greeting ""

set -gx HOMEBREW_PREFIX "/opt/homebrew";
set -gx HOMEBREW_CELLAR "/opt/homebrew/Cellar";
set -gx HOMEBREW_REPOSITORY "/opt/homebrew";
set -gx HOMEBREW_SHELLENV_PREFIX "/opt/homebrew";
set -q PATH; or set PATH ''; set -gx PATH "/opt/homebrew/bin" "/opt/homebrew/sbin" $PATH;
set -q MANPATH; or set MANPATH ''; set -gx MANPATH "/opt/homebrew/share/man" $MANPATH;
set -q INFOPATH; or set INFOPATH ''; set -gx INFOPATH "/opt/homebrew/share/info" $INFOPATH;

set -gx EDITOR nvim

# Visual -----------------------------------------------------------------------

alias cls='clear && printf "\e[3J"'

export CLICOLOR=1

# Applications -----------------------------------------------------------------

# Git
alias g='git'
alias gsw='git checkout (git branch | fzf | cut -c 3-)'
alias cdg='cd (git rev-parse --show-toplevel)'

export GPG_TTY=(tty)

# Go
export GO111MODULE=on

# Emacs
export EMACS_USER_DIRECTORY=~/.emacs.d

# Homebrew
export HOMEBREW_NO_ANALYTICS=1
export HOMEBREW_NO_ENV_HINTS=1

# asdf
source (brew --prefix asdf)/libexec/asdf.fish

# direnv
direnv hook fish | source

# Rust
# source $HOME/.cargo/env
# set -Ua fish_user_paths $HOME/.cargo/bin

# zoxide
zoxide init fish | source

# https://www.reddit.com/r/fishshell/comments/n5bhka/up_cd_up_till_you_find_a_folder_as_child_dir/gzm48td/?context=3
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

# fzf
#
# Migration:
# fish: Unknown command: fzf_key_bindings
# ~/.config/fish/functions/fish_user_key_bindings.fish (line 2):
#   fzf_key_bindings
#   ^
# in function 'fish_user_key_bindings'
# in function '__fish_reload_key_bindings'
#         called on line 188 of file /usr/local/Cellar/fish/3.4.1/share/fish/functions/__fish_config_interactive.fish
# in function '__fish_config_interactive'
#         called on line 153 of file /usr/local/Cellar/fish/3.4.1/share/fish/config.fish
# in function '__fish_on_interactive'
# in event handler: handler for generic event 'fish_prompt'
# https://github.com/PatrickF1/fzf.fish/wiki/Migration-Guides#v7
fzf_configure_bindings

# Local ------------------------------------------------------------------------

if test -e ~/local.sh
  source ~/local.sh
end
