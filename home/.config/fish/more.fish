# Additonal fish customizations, mostly alises and env

# Aliases
alias gst    "git status"
alias jk     "jekyll"
alias rehash "rbenv rehash"
alias ll     "ls -lA"
alias la     "ls -A"
alias more-fish "source ~/.config/fish/more.fish"

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

# Source color theme setup
source ~/.config/fish/color-theme.fish

# Setting up other random crap

# Set up ruby gems to use Homebrew's readline
set -x RUBY_CONFIGURE_OPTS --with-readline-dir=(brew --prefix readline)

# If `.private.sh` exsits, load it
# This file is for ENV variables that shouldn't be checked in, such as tokens and API keys
if test -s "$HOME/.private.sh" ; source "$HOME/.private.sh" ; end


# Enable rbenv
set -gx RBENV_ROOT /usr/local/var/rbenv
status --is-interactive; and source (rbenv init -|psub)
set -g fish_user_paths "/usr/local/opt/postgresql@9.4/bin" $fish_user_paths

# Enable nvm
export NVM_DIR="$HOME/.nvm"
bass source /usr/local/opt/nvm/nvm.sh --no-use ';' nvm use > /dev/null
