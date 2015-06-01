alias gst "git status"
alias d todo2

alias ... ../..
alias .... ../../..
alias ..... ../../../..

# echo $PWD | sed -e "s|^$HOME|~|" -e 's|^/private||'

# __fish_git_prompt_showstashstate

. $HOME/.config/fish/solarized.fish

set CDPATH . $HOME $HOME/Code $CDPATH
set PATH /usr/local/bin $PATH
if test -d "$HOME/bin"
  set PATH $HOME/bin $PATH
end

# apparently necessary for larger node builds, since it tries
# to open all files at once
ulimit -n 4096

set -x EDITOR vim
set -x VISUAL atom
set -x GEMEDITOR atom

# set -x DOCKER_HOST 127.0.0.1:32000
if type boot2docker >/dev/null 2>&1
  # set -x DOCKER_HOST tcp://(boot2docker ip 2>/dev/null):2376
  # set -x DOCKER_CERT_PATH /Users/ehyland/.boot2docker/certs/boot2docker-vm
  # set -x DOCKER_TLS_VERIFY 1

  # Do boot2docker setup by using its `shellinit` function, which adapts to the
  # actual env that it's setting up
  eval (boot2docker shellinit | tr \n \;)
end

set -x RUBY_CONFIGURE_OPTS --with-readline-dir=(brew --prefix readline)
set fish_color_virtualenv red

# If `.private.sh` exsits, load it
# This file is for ENV variables that shouldn't be checked in, such as tokens and API keys
if test -s "$HOME/.private.sh" ; source "$HOME/.private.sh" ; end

# Enable RBENV
# set PATH $HOME/.rbenv/bin $PATH
# set PATH $HOME/.rbenv/shims $PATH
# rbenv rehash >/dev/null ^&1

set -gx RBENV_ROOT /usr/local/var/rbenv
. (rbenv init -|psub)
