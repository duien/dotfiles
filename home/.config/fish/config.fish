alias gst "git status"

# echo $PWD | sed -e "s|^$HOME|~|" -e 's|^/private||'

# __fish_git_prompt_showstashstate

set CDPATH . $HOME $HOME/Code $CDPATH

# If `.private.sh` exsits, load it
# This file is for ENV variables that shouldn't be checked in, such as tokens and API keys
if test -s "$HOME/.private.sh" ; source "$HOME/.private.sh" ; end

# Enable RBENV
set PATH $HOME/.rbenv/bin $PATH
set PATH $HOME/.rbenv/shims $PATH
rbenv rehash >/dev/null ^&1
