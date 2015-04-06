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

set -x EDITOR vim
set -x VISUAL atom
set -x GEMEDITOR atom

set -x DOCKER_HOST 127.0.0.1:32000

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
