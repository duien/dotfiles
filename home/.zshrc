# Path to your oh-my-zsh configuration.
export ZSH=$HOME/.oh-my-zsh

# Set to the name theme to load.
# Look in ~/.oh-my-zsh/themes/
export ZSH_THEME="duien"

# PROMPT=""
# ─%{$terminfo[bold]$fg[green]%}%n@%m%{$reset_color%} %{$terminfo[bold]$fg[blue]%} %~%{$reset_color%} %{$fg[red]%}‹$(~/.rvm/bin/rvm-prompt i v g)›%{$reset_color%} $(git_prompt_info)%{$reset_color%}
# ╰─%B$%b
# RPROMPT="%(?..%{%}%? ↵%{%})"

# Set to this to use case-sensitive completion
# export CASE_SENSITIVE="true"

# Comment this out to disable weekly auto-update checks
# export DISABLE_AUTO_UPDATE="true"

# Uncomment following line if you want to disable colors in ls
# export DISABLE_LS_COLORS="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git rvm brew cap gem vi-mode)

source $ZSH/oh-my-zsh.sh

# Customize to your needs...
export PATH=/Users/eprice/bin:/usr/local/mongodb/bin:/usr/local/bin:/usr/local/sbin:/usr/local/mysql/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:/usr/X11/bin:/opt/local/bin:/usr/local/git/bin:/Users/eprice/.rvm/bin

export RUBYOPT='rubygems'
export PATH="/usr/local/mongodb/bin:/usr/local/bin:/usr/local/sbin:/usr/local/mysql/bin:$PATH"
export PYTHONPATH='/Library/Frameworks/Python.framework/Versions/2.5/lib/python2.5/site-packages/'
export CDPATH=.:~:~/Code
export VISUAL=mvim
export GEMEDITOR=mvim


# GIT
alias gst='git status'
alias gpr='git pull --rebase'

# RAILS
alias sc='script/console'
alias ss='script/server'
alias rs='rake spec'

# OTHER DEV STUFF
alias heroku-deploy='git push heroku && heroku rake db:migrate && heroku restart'

# BASICS
alias ll='ls -l'
alias la='ls -A'
alias lc="clear;ls"
alias grep='grep --color=auto'
alias flip="perl -pi -e 's/\r\n?/\n/g'"
alias gemi='sudo gem install --no-ri'

alias cdhg='cd ~/Code/Work/Highgroove'

alias pgstart='pg_ctl -D /usr/local/var/postgres -l /usr/local/var/postgres/server.log start'
alias pgstop='pg_ctl -D /usr/local/var/postgres stop -s -m fast'

unsetopt auto_name_dirs
# rvm-install added line:
if [[ -s "$HOME/.rvm/scripts/rvm" ]] ; then source "$HOME/.rvm/scripts/rvm" ; fi

