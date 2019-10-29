# ------------------------------------------------------------------------------

setopt extendedglob
setopt nomatch # if a pattern for filename generation has no matches, print an error, instead of leaving it unchanged in the argument list

setopt no_beep
setopt notify # report the status of background jobs immediately, rather than waiting until just before printing a prompt.
zstyle :compinstall filename '/Users/jesse/.zshrc'
export REPORTTIME=4
ZSH_COMMAND_TIME_MIN_SECONDS=4

# History ----------------------------------------------------------------------

HISTFILE=~/.histfile
HISTSIZE=10000
SAVEHIST=10000
setopt hist_ignore_dups             # ignore duplication command history list
setopt hist_ignore_space            # don't save commands beginning with spaces to history
setopt append_history               # append to the end of the history file
setopt inc_append_history           # always be saving history (not just when the shell exits)
setopt hist_expire_dups_first
setopt extended_history

# Editor -----------------------------------------------------------------------

EDITOR=nvim
VISUAL=nvim
HOMEBREW_EDITOR=nvim

autoload edit-command-line; zle -N edit-command-line
bindkey -M vicmd v edit-command-line

# Completion -------------------------------------------------------------------

zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'
zstyle ':completion:*' menu select

# dots -------------------------------------------------------------------------
alias dots='$(which git) --git-dir=$HOME/.dots.git/ --work-tree=$HOME'

# Visual -----------------------------------------------------------------------

alias cls='clear && printf "\e[3J"'

export CLICOLOR=1

# Applications -----------------------------------------------------------------

# ripgrep
alias rg="rg --smart-case --pretty"

# Git
alias cdg='cd $(git rev-parse --show-toplevel)'

# Emacs
export EMACS_USER_DIRECTORY=~/.emacs.d
# alias emacs="/usr/local/Cellar/emacs-plus/26.3/bin/emacs"
alias emacs="/usr/local/Cellar/emacs-mac/emacs-26.3-z-mac-7.7/bin/emacs"

# Homebrew
HOMEBREW_NO_ANALYTICS=1

# ASDF
autoload -Uz compinit && compinit
. $HOME/.asdf/asdf.sh
. $HOME/.asdf/completions/asdf.bash

# Bindings ---------------------------------------------------------------------

bindkey -v

bindkey '^r' history-incremental-search-backward

export KEYTIMEOUT=1 # Make Vi mode transitions faster (KEYTIMEOUT is in hundredths of a second)

# Updates editor information when the keymap changes.
function zle-keymap-select() {
  zle reset-prompt
  zle -R
}

zle -N zle-keymap-select

function vi_mode_prompt_info() {
  echo "${${KEYMAP/vicmd/[% NORMAL]%}/(main|viins)/[% INSERT]%}"
}

# Plugins ----------------------------------------------------------------------

### Added by Zplugin's installer
source '/Users/jesse/.zplugin/bin/zplugin.zsh'
autoload -Uz _zplugin
(( ${+_comps} )) && _comps[zplugin]=_zplugin
### End of Zplugin's installer chunk

zplugin snippet OMZ::plugins/shrink-path/shrink-path.plugin.zsh
zplugin snippet OMZ::plugins/colored-man-pages/colored-man-pages.plugin.zsh
zplugin light zsh-users/zsh-autosuggestions
zplugin light zdharma/fast-syntax-highlighting
zplugin light softmoth/zsh-vim-mode

# Disabled
# zplugin light popstas/zsh-command-time
# zplugin snippet OMZ::plugins/colored-man-pages/colored-man-pages.plugin.zsh
# zplugin snippet OMZ::plugins/timer/timer.plugin.zsh
# zplugin ice pick"gitstatus.prompt.zsh"
# zplugin light romkatv/gitstatus $GITSTATUS_PROMPT

# FZF --------------------------------------------------------------------------

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
export FZF_DEFAULT_COMMAND='rg --files --no-ignore --hidden --follow --glob "!.git/*"'

# Prompt -----------------------------------------------------------------------

# shrink-path
setopt prompt_subst

# user is red if privileged
local n='%n'
if [[ "${(%):-%#}" == '#' ]]; then
  n="%{$fg[red]%}%{$reset_color%}"
  n+="%{$fg_bold[red]%}%n%{$reset_color%}"
  n+="%{$fg[red]%}%{$reset_color%}"
fi

# exit code
# https://zenbro.github.io/2015/07/23/show-exit-code-of-last-command-in-zsh
function check_last_exit_code() {
  local LAST_EXIT_CODE=$?
  if [[ $LAST_EXIT_CODE -ne 0 ]]; then
    local EXIT_CODE_PROMPT=' '
    EXIT_CODE_PROMPT+="%{$fg[red]%}%{$reset_color%}"
    EXIT_CODE_PROMPT+="%{$fg_bold[red]%}$LAST_EXIT_CODE%{$reset_color%}"
    EXIT_CODE_PROMPT+="%{$fg[red]%}%{$reset_color%}"
    echo "$EXIT_CODE_PROMPT"
  fi
}

# Git status
# https://github.com/subnixr/minimal
MNML_OK_COLOR="${MNML_OK_COLOR:-2}"
MNML_ERR_COLOR="${MNML_ERR_COLOR:-1}"

function mnml_git {
    local statc="%{\e[0;3${MNML_OK_COLOR}m%}" # assume clean
    local bname="$(git rev-parse --abbrev-ref HEAD 2> /dev/null)"

    if [ -n "$bname" ]; then
        if [ -n "$(git status --porcelain 2> /dev/null)" ]; then
            statc="%{\e[0;3${MNML_ERR_COLOR}m%}"
        fi
        printf '%b' "$statc$bname%{\e[0m%}"
    fi
}

# Prompt symbols: λ ❯
PROMPT='$(shrink_path -f) $(mnml_git) ❯ '
RPROMPT='$(check_last_exit_code) $n@%m ∙ %j ∙ %T $(vi_mode_prompt_info)'

# Local ------------------------------------------------------------------------

if [ -f ~/local.sh ]; then
  source ~/local.sh
fi
