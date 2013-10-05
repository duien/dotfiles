# http://zanshin.net/2013/02/02/zsh-configuration-from-the-ground-up/

export DEFAULT_USER='eprice'

export RUBYOPT='rubygems'
export VISUAL=subl
export GEMEDITOR=subl
export CC=gcc-4.2
export CDPATH=.:~:~/Code:~/Code/Work
export CLICOLOR=1

# RDS CLI Configuration
export AWS_RDS_HOME='/usr/local/rds'
export JAVA_HOME=`/usr/libexec/java_home`
# EC2_CERT and EC2_PRIVATE_KEY need to bet set up in ~/.private.sh

# If `.private.sh` exsits, load it
# This file is for ENV variables that shouldn't be checked in, such as tokens and API keys
[[ -s "$HOME/.private.sh" ]] && source "$HOME/.private.sh"

alias pgstart='pg_ctl -D /usr/local/var/postgres -l /usr/local/var/postgres/server.log start'
alias pgstop='pg_ctl -D /usr/local/var/postgres stop -s -m fast'
alias mark='open -a Marked'
alias ql='qlmanage -p'
alias 'gst'='git status'

alias -g ...='../..'
alias -g ....='../../..'
alias -g .....='../../../..'

# Wrap `git` in `hub` if it's installed
[[ -s "$HOME/bin/hub" ]] && function git(){hub "$@"}

PATH="/usr/local/heroku/bin:/usr/local/share/npm/bin:/usr/local/bin:$PATH:/usr/local/mysql/bin"
if [ -d "$AWS_RDS_HOME" ] ; then
  PATH="$AWS_RDS_HOME/bin:$PATH"
fi
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

# case $TERM in
#   xterm*)
#     chpwd () {print -Pn "\e]0;%n@%m: %~\a"}
#     ;;
# esac

[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # This loads RVM into a shell session.

PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting

if declare -f rbenv >/dev/null ; then eval "$(rbenv init -)"; fi

source "$HOME/.zsh/setopt.zsh"
source "$HOME/.zsh/prompt.zsh"
source "$HOME/.zsh/completion.zsh"
