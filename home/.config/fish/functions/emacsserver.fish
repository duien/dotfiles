function emacsserver --description "find the running emacs server"
    argparse -i 'p/with-profile=' -- $argv
    set -l server
    if test -n "$_flag_p"
        set server $_flag_p
    else
        set server (cat ~/.emacs-profile | string trim)
    end
    lsof -c Emacs | grep "$server\$" | tr -s " " | cut -d' ' -f8
end
