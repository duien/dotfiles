function ec --description "open emacs client, starting server if needed"
    set -l socket_file (emacsserver)

    if test -z "$socket_file"
	echo "Starting emacs server..."
	emacs --chdir $PWD --execute '(server-start)' $argv &
  disown
    else
	emacsclient -n $argv --socket-name $socket_file
    end
end
