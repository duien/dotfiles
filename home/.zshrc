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

export RUBYOPT='rubygems'
export VISUAL=subl
export GEMEDITOR=subl
export CC=gcc-4.2

# If `private.sh` exsits, load it
# This file is for ENV variables that shouldn't be checked in, such as tokens and API keys
[[ -s "$HOME/.private.sh" ]] && source "$HOME/.private.sh"

alias pgstart='pg_ctl -D /usr/local/var/postgres -l /usr/local/var/postgres/server.log start'
alias pgstop='pg_ctl -D /usr/local/var/postgres stop -s -m fast'
alias mark='open -a Marked'

# Wrap `git` in `hub` if it's installed
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

[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # This loads RVM into a shell session.
