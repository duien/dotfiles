function emacsserver --description "find the running emacs server"
    set -l default_server (cat ~/.emacs-profile | string trim)-server
    lsof -c Emacs | grep $default_server | tr -s " " | cut -d' ' -f8
end
