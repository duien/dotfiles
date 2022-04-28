# Additonal fish customizations, mostly alises and env

# Aliases
alias gst        "git status"
alias jk         "jekyll"
alias rehash     "rbenv rehash"
alias ll         "ls -lA"
alias la         "ls -A"
alias more-fish "source ~/.config/fish/more.fish"

# Basic environment
prepend_if_exists CDPATH . $HOME $HOME/Code $HOME/Code/Dox $HOME/.homesick/repos
prepend_if_exists PATH /usr/local/bin /usr/local/sbin
prepend_if_exists PATH "/Applications/Sublime Text.app/Contents/SharedSupport/bin"

prepend_if_exists PATH "$HOME/bin"
prepend_if_exists PATH "$HOME/.bin"
prepend_if_exists PATH "$HOME/Code/Dox/dox-compose/bin"
prepend_if_exists PATH "$HOME/.doom/bin"

set -x DOOMDIR "$HOME/.doom-config"
# Set up homebrew packages that need to be prepended to path
prepend_if_exists fish_user_paths "/usr/local/opt/postgresql@9.6/bin" "/usr/local/opt/openssl@1.1/bin"
# This was in universal variables, which are not a great idea for my setup
prepend_if_exists fish_user_paths "/usr/local/opt/openssl/bin" "/usr/local/opt/postgresql@9.4/bin" "/Users/duien/.yarn/bin"

set -x EDITOR em -w
set -x VISUAL em -w
set -x GEMEDITOR em -w

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
set -gx RBENV_ROOT /usr/local/var/rbenv
status --is-interactive; and source (rbenv init -|psub)

# Set up NVM
# if test -d "$HOME/.nvm"
#   nvm use node
# end
