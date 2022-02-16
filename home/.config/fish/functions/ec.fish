function ec --description "open emacs client, starting server if needed"
    argparse -i 'p/with-profile=' -- $argv
    argparse -i 't/tty' -- $argv
    set -l socket_file
    if test -n "$_flag_p"
        set socket_file (emacsserver -p$_flag_p)
    else
        set socket_file (emacsserver)
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
        set -l clientflag "-n"
        if test -n "$_flag_tty"
            set clientflag "-t"
        end
        emacsclient $clientflag $argv --socket-name $socket_file
    end
end
