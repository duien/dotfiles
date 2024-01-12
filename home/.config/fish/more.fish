# Additonal fish customizations, mostly alises and env

# Aliases
alias gst        "git status"
alias gits       "git switch"
alias jk         "bundle exec jekyll"
alias rehash     "rbenv rehash"
alias more-fish  "source ~/.config/fish/more.fish"

# Experiment with exa as ls alternative
# only install these aliases if exa actually exists
if test -x /opt/homebrew/bin/exa
  alias ls         "exa --group-directories-first -sName"
  alias ll         "ls --long --all --git"
else
  alias ll         "ls -lA"
  alias la         "ls -A"
end

# Basic environment
prepend_if_exists CDPATH . $HOME $HOME/Code $HOME/.homesick/repos

prepend_if_exists PATH "$HOME/bin"
prepend_if_exists PATH "$HOME/.bin"
prepend_if_exists PATH "$HOME/.doom/bin"
prepend_if_exists PATH "$HOME/.cargo/bin"

set -x DOOMDIR "$HOME/.doom-config"

set -x EDITOR ew
set -x VISUAL ew
set -x GEMEDITOR ew

# Source color theme setup
source ~/.config/fish/color-theme.fish

# Setting up other random crap

# Set up ruby gems to use Homebrew's readline
set -x RUBY_CONFIGURE_OPTS --with-readline-dir=(brew --prefix readline) --with-openssl-dir=(brew --prefix openssl@1.1)
# set -x RUBY_CONFIGURE_OPTS --with-readline-dir=(brew --prefix readline) -W0

# If `.private.sh` exists, load it
# This file is for ENV variables that shouldn't be checked in, such as tokens and API keys
if test -s "$HOME/.private.sh" ; source "$HOME/.private.sh" ; end


# Enable rbenv
if [ -x /usr/local/bin/brew ]
  set -gx RBENV_ROOT /usr/local/var/rbenv
end
rbenv init - fish | source

# Set up NVM
# if test -d "$HOME/.nvm"
#   nvm use node
# end
