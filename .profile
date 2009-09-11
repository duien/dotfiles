case "$TERM" in xterm-color)
   	PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
    ;;
xterm-256color)
   	PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
        ;;
*)
    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
    ;;
esac

if [ "$TERM" != "dumb" ]; then
    LSCOLORS='ExGxFxdxCxDxDxHbaDacHe'
    export LSCOLORS
fi

function hspec {
  if [ -z "$1" ]; then
    echo usage: hspec file [browser]
  else
    BROWSER=$2
    if [ -z "$BROWSER" ]; then
      BROWSER=Safari
    fi
    spec -f h $1 > /tmp/hspec.html
    open -a $BROWSER /tmp/hspec.html
  fi
}

alias ls='ls -GF'
alias la='ls -a'
alias ll='ls -l'
alias flip="perl -pi -e 's/\r\n?/\n/g'"
alias hide="SetFile -a V"
alias show="SetFile -a v"

export PATH="/usr/local/bin:/usr/local/graphviz-2.14/bin:/usr/local/mysql/bin:$PATH"
export PATH="/opt/local/bin:/opt/local/sbin:$PATH"
export MANPATH="/opt/local/share/man:/usr/local/man:$MANPATH"

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi
