function ec --description "open emacs client, starting server if needed"
    argparse -i 'p/with-profile=' -- $argv
    if test -n "$_flag_p"
        set -l socket_file (emacsserver -p$_flag_p)
    else
        set -l socket_file (emacsserver)
    end

    if test -z "$socket_file"
        echo "Starting emacs server..."
        if test -n "$_flag_p"
            emacs --with-profile $_flag_p --chdir $PWD --execute '(server-start)' $argv &
        else
            emacs --chdir $PWD --execute '(server-start)' $argv &
        end
        disown
    else
        emacsclient -n $argv --socket-name $socket_file
    end
end
