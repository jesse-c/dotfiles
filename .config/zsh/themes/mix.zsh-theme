# vim:et sts=2 sw=2 ft=zsh
#
# Gitster theme
# https://github.com/shashankmehta/dotfiles/blob/master/thesetup/zsh/.oh-my-zsh/custom/themes/gitster.zsh-theme
#
# Requires the `git-info` zmodule to be included in the .zimrc file.

prompt_gitster_pwd() {
  prompt_short_dir=$(short_pwd)
  git_root=$(command git rev-parse --show-toplevel 2> /dev/null) && prompt_short_dir=${prompt_short_dir#${$(short_pwd $git_root):h}/}
  print -n "%F{250}${prompt_short_dir}"
}

prompt_gitster_git() {
  [[ -n ${git_info} ]] && print -n "${(e)git_info[prompt]}"
}

# https://adrian-philipp.com/post/shell-show-time-last-command-took
prompt_gitster_preexec() {
  timer=${timer:-$SECONDS}
}

prompt_gitster_precmd() {
  (( ${+functions[git-info]} )) && git-info

  [[ ! -z $timer ]] && timer_show=$(($SECONDS - $timer)); unset timer
}

elapsed_time() {
  [[ -z $timer_show ]] || return

  print -n "%F{245}↻ \${timer_show}s%{$reset_color%} "
}

current_jobs() {
  local n=$( (jobs) | wc -l | xargs )

  [[ "$n" -gt 0 ]] || return

  print -n "%F{245}⌽ ${n} "
}

function asdf_local() {
  [[ "$(pwd)" != "$HOME" ]] || return

  local f="$(pwd)/.tool-versions"
  [[ -f "$f" ]] || return

  local versions=""

  # https://www.zsh.org/mla/users/2007/msg00288.html
  while read i
  do
    local v=${i/ /:}
    versions+="%F{176}$v%F{174}, "
  done < .tool-versions

  print -n "%F{174}[${versions[1,-3]}%F{174}] "
}

prompt_gitster_setup() {
  local prompt_gitster_status='%(?:%F{002}:%F{red}%? )→'

  autoload -Uz add-zsh-hook \
    && add-zsh-hook precmd prompt_gitster_precmd \
    && add-zsh-hook preexec prompt_gitster_preexec

  prompt_opts=(cr percent sp subst)

  zstyle ':zim:git-info:branch' format '%b'
  zstyle ':zim:git-info:commit' format '%c'
  zstyle ':zim:git-info:clean' format '%F{002}✓'
  zstyle ':zim:git-info:dirty' format '%F{216}✗'
  zstyle ':zim:git-info:diverged' format '%F{216}⇕'
  zstyle ':zim:git-info:behind' format '%F{216}⇣'
  zstyle ':zim:git-info:ahead' format '%F{216}⇡'
  zstyle ':zim:git-info:keys' format \
    'prompt' ' %F{cyan}%b%c %C%D'

  local hostname="%F{245}%m"
  local timestamp="%F{245}%*"

  PS1="${hostname} \$(asdf_local)\$(prompt_gitster_pwd)\$(prompt_gitster_git) ${prompt_gitster_status}%f "
  RPS1="$(elapsed_time)\$(current_jobs)${timestamp}"
 }

prompt_gitster_setup "${@}"
