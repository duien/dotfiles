export RUBYOPT='rubygems'
export PATH="/usr/local/mongodb/bin:/usr/local/bin:/usr/local/sbin:/usr/local/mysql/bin:$PATH"
export PYTHONPATH='/Library/Frameworks/Python.framework/Versions/2.5/lib/python2.5/site-packages/'
export CDPATH=.:~:~/Code/Work/Highgroove
export VISUAL=mvim
export GEMEDITOR=mvim
export CC=gcc-4.2

source ~/.git_completion.sh

# rvm-install added line:
if [[ -s "$HOME/.rvm/scripts/rvm" ]] ; then source "$HOME/.rvm/scripts/rvm" ; fi


PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting
