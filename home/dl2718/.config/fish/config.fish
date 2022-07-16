if status is-interactive
    # Commands to run in interactive sessions can go here

    # remove greeting    
    set -U fish_greeting

    # zoxide
    zoxide init fish --cmd cd | source

end

