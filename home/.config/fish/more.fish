# Additonal fish customizations, mostly alises and env


# Aliases
alias gst "git status"

# Basic environment
set CDPATH . $HOME $HOME/Code $CDPATH
set PATH /usr/local/bin $PATH
if test -d "$HOME/bin"
  set PATH $HOME/bin $PATH
end

if test -d "$HOME/.bin"
  set PATH $HOME/.bin $PATH
end

set -x EDITOR vim
set -x VISUAL atom
set -x GEMEDITOR atom

# Setting up other random crap

# apparently necessary for larger node builds, since it tries
# to open all files at once
ulimit -n 4096

if type boot2docker >/dev/null 2>&1
  eval (boot2docker shellinit | tr \n \;)
end

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
