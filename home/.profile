source ~/.bashrc

 rvm_version_string () {
  if [[ -s /Users/eprice/.rvm/scripts/rvm ]] ; then
    test "$(~/.rvm/bin/rvm-prompt i)" != "ruby" && echo "$(~/.rvm/bin/rvm-prompt i v g)" || echo "$(~/.rvm/bin/rvm-prompt v g)"
  fi
}

rvm_prompt () {
  test "$(rvm_version_string)" != "" && printf "[\e[0;33m$(rvm_version_string)\e[m]"
}

function parse_git_dirty {
  [[ $(git status 2> /dev/null | tail -n1) != "nothing to commit (working directory clean)" ]] && echo "*"
}

case "$TERM" in xterm-*color)
    PS1='\[\e]2;\u@\h\a\]\[\e[30;44m\]\w\[\e[00m\]$(rvm_prompt)$(__git_ps1 "[\[\e[0;32m\]%s\[\e[0m\]\[\e[0;33m\]$(parse_git_dirty)\[\e[0m\]]") \$ '
    ;;
xterm*)
   	PS1='\[\e]2;\u@\h\a\]\[\[0;34m\]\w \[\[00m\]\$ '
        ;;
*)
    PS1='\u@\h:\w \$ '
    ;;
esac

if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

case "`uname`" in
    Darwin)
        alias ls='ls -GF'
	alias hide="SetFile -a V"
	alias show="SetFile -a v"
	;;
    *) alias ls='ls --color=auto -F';;
esac

# GIT
alias gst='git status'
alias gpr='git pull --rebase'

# RAILS
alias sc='script/console'
alias ss='script/server'
alias rs='rake spec'

# BASICS
alias ll='ls -l'
alias la='ls -A'
alias lc="clear;ls"
alias grep='grep --color=auto'
alias flip="perl -pi -e 's/\r\n?/\n/g'"
alias gemi='sudo gem install --no-ri'

# rvm-install added line:
if [[ -s /Users/eprice/.rvm/scripts/rvm ]] ; then source /Users/eprice/.rvm/scripts/rvm ; fi
