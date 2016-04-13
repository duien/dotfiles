# Additonal fish customizations, mostly alises and env

# Aliases
alias gst "git status"
alias jk jekyll

# Basic environment
set CDPATH . $HOME $HOME/Code $CDPATH
set PATH /usr/local/bin /usr/local/sbin $PATH
if test -d "$HOME/bin"
  set PATH $HOME/bin $PATH
end

if test -d "$HOME/.bin"
  set PATH $HOME/.bin $PATH
end

set -x EDITOR nvim
set -x VISUAL nvim
set -x GEMEDITOR nvim

# Setting up other random crap

# apparently necessary for larger node builds, since it tries
# to open all files at once
ulimit -n 4096

# if type boot2docker >/dev/null 2>&1
#   set -x DOCKER_HOST tcp://192.168.59.103:2375
#   # eval (boot2docker shellinit | tr \n \;)
# end

if test -s  ~/.config/fish/nvm-wrapper/nvm.fish
  source ~/.config/fish/nvm-wrapper/nvm.fish
end

set -x RUBY_CONFIGURE_OPTS --with-readline-dir=(brew --prefix readline)

# If `.private.sh` exsits, load it
# This file is for ENV variables that shouldn't be checked in, such as tokens and API keys
if test -s "$HOME/.private.sh" ; source "$HOME/.private.sh" ; end


# Enable rbenv
set -gx RBENV_ROOT /usr/local/var/rbenv
. (rbenv init -|psub)


set -x GOPATH /Users/ehyland/Code/Go
set -x GOBIN /Users/ehyland/Code/Go/bin

set -x PATH $PATH $GOBIN
