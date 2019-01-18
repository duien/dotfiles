if [ -f ~/bin/sensible.bash ]; then
   source ~/bin/sensible.bash
fi

if [ -f $(brew --prefix)/etc/bash_completion ]; then
  source $(brew --prefix)/etc/bash_completion
fi

# Trying out a new prompt (oh, and switching to bash)

_path () {
  echo "$PWD" | tr "/" ""
}

_red () { # return value
  _return=$(if [[ $RETURN == 0 ]]; then echo "" ; else echo " $? " ; fi)
  echo "\[\033[48;5;1m\]$_return\[\033[38;5;1m\]\[\033[48;5;3m\]"
}
_yellow () { # host
  # echo "\[\033[38;5;3m\]\[\033[48;5;2m\]\[$(tput sgr0)\]"
  # _host=" $(hostname) "
  echo "\[\033[48;5;3m\]\[\033[38;5;0m\]$_host\[\033[38;5;3m\]\[\033[48;5;2m\]\[$(tput sgr0)\]"
}
_green () { # user
  # _user=$(if [[ $(whoami) == "duien" ]]; then echo "" ; else echo " $(whoami) "; fi)
  _user=" $(whoami) "
  echo "\[\033[48;5;2m\]$_user\[\033[38;5;2m\]\[\033[48;5;6m\]\[$(tput sgr0)\]"
}
_cyan () {
  # _parent_dir=$(dirname $newPWD)
  _parent_dir=$(basename $(dirname $PWD))
  echo "\[\033[48;5;6m\]\[\033[38;5;0m\] $_parent_dir \[\033[38;5;6m\]\[\033[48;5;4m\]\[$(tput sgr0)\]"
}
_blue () {
  _current_dir=$(basename $PWD)
  echo "\[\033[48;5;4m\]\[\033[38;5;0m\] $_current_dir \[\033[38;5;4m\]\[\033[48;5;5m\]\[$(tput sgr0)\]"
}

export PROMPT_COMMAND=__prompt_command
function __prompt_command () {
  RETURN=$?
  # truncate_dir $(dirname $PWD)
  PS1="$(_red)$(_yellow)$(_green)$(_cyan)$(_blue)\[\033[38;5;5m\]\[$(tput sgr0)\] "
}

# Fancy middle truncation
#
# function truncate_pwd () { 
#   if [ $HOME == $PWD ] ; then
#     newPWD="~"
#   elif [ $HOME == ${PWD:0:${#HOME}} ] ; then
#     newPWD="~${PWD:${#HOME}}"
#   else
#     newPWD=$PWD
#   fi
# 
#   local pwdmaxlen=50
#   local pwdstartlen=20
#   local pwdendlen=$(( $pwdmaxlen - $pwdstartlen ))
# 
#   if [ ${#newPWD} -gt $pwdmaxlen ] ; then
#     local pwdoffset=$(( ${#newPWD} - $pwdendlen ))
#     newPWD="${newPWD:0:$pwdstartlen}...${newPWD:$pwdoffset:$pwde ndlen}"
#   fi
# }

function truncate_dir () {
  local dir=$1
  echo "---$dir---"
  if [ $HOME == $dir ]
  then
    newPWD="~"
  elif [ $HOME ==  ${dir:0:${#HOME}} ]
  then
    newPWD="~${dir:${#HOME}}"
  else
    newPWD=$dir
  fi

  local pwdmaxlen=15
  if [ ${#newPWD} -gt $pwdmaxlen ]
  then
    local pwdoffset=$(( ${#newPWD} - $pwdmaxlen  ))
    newPWD=".+${newPWD:$pwdoffset:$pwdmaxlen}"
  fi
}



# export 

# 38 : foreground
# 48 : background


# [0] black
# [1] red    - return
# [3] yellow - host
# [2] green  - user
# [6] teal   - path
# [4] blue   - directory
# [5] purple - git
# [7] gray?

# [8] darker? (bright black)
# [9] bright red?

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
