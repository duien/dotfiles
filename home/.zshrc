# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="duien"

# Theme Notes
# kolo is cool for git status
# miloshadzic has nice design
# wedisagree has crazy right prompt
# juanghurtado is kinda nice

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Comment this out to disable weekly auto-update checks
DISABLE_AUTO_UPDATE="true"

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
# COMPLETION_WAITING_DOTS="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git brew gem github heroku pow rails rails3 ruby rvm vi-mode)

source $ZSH/oh-my-zsh.sh

# Customize to your needs...

export RUBYOPT='rubygems'
export VISUAL=mvim
export GEMEDITOR=mvim
export CC=gcc-4.2

alias pgstart='pg_ctl -D /usr/local/var/postgres -l /usr/local/var/postgres/server.log start'
alias pgstop='pg_ctl -D /usr/local/var/postgres stop -s -m fast'

alias mark='open -a Marked'
[[ -s "$HOME/bin/hub" ]] && function git(){hub "$@"}

PATH="/usr/local/bin:$PATH"
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

case $TERM in
  xterm*)
    chpwd () {print -Pn "\e]0;%n@%m: %~\a"}
    ;;
esac

[[ -s "$HOME/.rvm/scripts/rvm" ]] && . "$HOME/.rvm/scripts/rvm" # Load RVM function
