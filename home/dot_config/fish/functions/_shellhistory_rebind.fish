function _shellhistory_rebind --on-variable fish_bind_mode --on-variable fish_key_bindings
    type -q shhist; and type -q fzf; or return
    bind ctrl-r shellhistory_pick
    bind -M insert ctrl-r shellhistory_pick
end
