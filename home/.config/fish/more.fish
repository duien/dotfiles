# Additonal fish customizations, mostly alises and env

# Aliases
alias gst    "git status"
alias jk     "jekyll"
alias rehash "rbenv rehash"
alias ll     "ls -lA"
alias la     "ls -A"
alias more-fish "source ~/.config/fish/more.fish"

# Basic environment
set CDPATH . $HOME $HOME/Code $HOME/.homesick/repos $CDPATH
set PATH /usr/local/bin /usr/local/sbin $PATH
if test -d "$HOME/bin"
  set PATH $HOME/bin $PATH
end

if test -d "$HOME/.bin"
  set PATH $HOME/.bin $PATH
end

set -x EDITOR vim
set -x VISUAL code
set -x GEMEDITOR code

# Source color theme setup
source ~/.config/fish/color-theme.fish

# Setting up other random crap

# Set up ruby gems to use Homebrew's readline
set -x RUBY_CONFIGURE_OPTS --with-readline-dir=(brew --prefix readline) --with-openssl-dir=(brew --prefix openssl@1.1)
# set -x RUBY_CONFIGURE_OPTS --with-readline-dir=(brew --prefix readline) -W0

# If `.private.sh` exsits, load it
# This file is for ENV variables that shouldn't be checked in, such as tokens and API keys
if test -s "$HOME/.private.sh" ; source "$HOME/.private.sh" ; end


# Enable rbenv
set -gx RBENV_ROOT /usr/local/var/rbenv
status --is-interactive; and source (rbenv init -|psub)

if test -d "/usr/local/opt/postgresql@9.6/bin"
  set -g fish_user_paths "/usr/local/opt/postgresql@9.6/bin" $fish_user_paths
end

if test -d "/usr/local/opt/openssl@1.1/bin"
  set -g fish_user_paths "/usr/local/opt/openssl@1.1/bin" $fish_user_paths
end

# Enable nvm
if test -x "(which nvm)"
  export NVM_DIR="$HOME/.nvm"
  bass source /usr/local/opt/nvm/nvm.sh --no-use ';' nvm use > /dev/null
end
