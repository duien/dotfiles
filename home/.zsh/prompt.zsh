# vim:ft=zsh ts=2 sw=2 sts=2
#
# based on agnoster's Theme - https://gist.github.com/3712874
# A Powerline-inspired theme for ZSH
#
# # README
#
# In order for this theme to render correctly, you will need a
# [Powerline-patched font](https://gist.github.com/1595572).
#
# In addition, I recommend the
# [Solarized theme](https://github.com/altercation/solarized/) and, if you're
# using it on Mac OS X, [iTerm 2](http://www.iterm2.com/) over Terminal.app -
# it has significantly better color fidelity.
#
# # Goals
#
# The aim of this theme is to only show you *relevant* information. Like most
# prompts, it will only show git information when in a git working directory.
# However, it goes a step further: everything from the current user and
# hostname to whether the last call exited with an error to whether background
# jobs are running in this shell will all be displayed automatically when
# appropriate.

### Segment drawing
# A few utility functions to make it easy and re-usable to draw segmented prompts


# Code point  Glyph Description
# U+E0A0   Version control branch
# U+E0A1   LN (line) symbol
# U+E0A2   Closed padlock
# U+E0B0   Rightwards black arrowhead
# U+E0B1   Rightwards arrowhead
# U+E0B2   Leftwards black arrowhead
# U+E0B3   Leftwards arrowhead

CURRENT_BG='NONE'
SEGMENT_SEPARATOR=''
SEGMENT_INNER_SEPARATOR=''

RIGHT_SEGMENT_SEPARATOR=''

# Begin a segment
# Takes two arguments, background and foreground. Both can be omitted,
# rendering default background/foreground.
prompt_segment() {
  local bg fg
  [[ -n $1 ]] && bg="%K{$1}" || bg="%k"
  [[ -n $2 ]] && fg="%F{$2}" || fg="%f"
  if [[ $CURRENT_BG != 'NONE' && $1 != $CURRENT_BG ]]; then
    echo -n " %{$bg%F{$CURRENT_BG}%}$SEGMENT_SEPARATOR%{$fg%} "
  else
    echo -n "%{$bg%}%{$fg%} "
  fi
  CURRENT_BG=$1
  [[ -n $3 ]] && echo -n $3
}

# End the prompt, closing any open segments
prompt_end() {
  if [[ -n $CURRENT_BG ]]; then
    echo -n " %{%k%F{$CURRENT_BG}%}$SEGMENT_SEPARATOR"
  else
    echo -n "%{%k%}"
  fi
  echo -n "%{%f%}"
  CURRENT_BG=''
}

### Prompt components
# Each component will draw itself, and hide itself if no information needs to be shown

prompt_time() {
  if [[ "$(date +%p)" == "AM" ]] ; then
    prompt_segment white black "$(date +%l:%M:%S | tr -d ' ')"
  else
    prompt_segment black white "$(date +%l:%M:%S | tr -d ' ')"
  fi
}

# Context: user@hostname (who am I and where am I)
# Will be hidden if the username is in the space-separated list $DEFAULT_USERS
prompt_context() {
  local user=`whoami`

  if [[ ! $DEFAULT_USERS =~ (^| )$user($| ) || -n "$SSH_CLIENT" ]]; then
    prompt_segment magenta black "%(!.%{%F{yellow}%}.)$user@%m"
  fi
}

# RVM: ruby version info
prompt_rvm() {
  local version
  if [[ -s $HOME/.rvm/scripts/rvm ]] ; then
    if [[ "$(~/.rvm/bin/rvm-prompt i)" == "ruby" ]] ; then
      version=$(~/.rvm/bin/rvm-prompt v g)
    else
      version=$(~/.rvm/bin/rvm-prompt i v g)
    fi
    if [[ -n $version ]] ; then
      # prompt_segment red black
      echo -n "$version"
    fi
  fi
}

# RBENV: ruby version info
prompt_rbenv() {
  local version
  if declare -f rbenv >/dev/null ; then
    # echo -n `rbenv version-name`
    version=$(rbenv version-name)
  fi
  if [[ -n $version ]] ; then
      # prompt_segment red black
      echo -n "$version"
    fi
}

function parse_git_dirty {
  [[ $(git status 2> /dev/null | tail -n1) != "nothing to commit, working directory clean" ]] && echo "*"
}

# Git: branch/detached head, dirty status
prompt_git() {
  local ref dirty
  if $(git rev-parse --is-inside-work-tree >/dev/null 2>&1); then
    dirty=$(parse_git_dirty)
    ref=$(git symbolic-ref HEAD 2> /dev/null) || ref="➦ $(git show-ref --head -s --abbrev |head -n1 2> /dev/null)"
    if [[ -n $dirty ]]; then
      prompt_segment red black
    else
      prompt_segment green black
    fi
    echo -n "${ref/refs\/heads\// }"
  fi
}

# Dir: current working directory
prompt_dir() {
  prompt_segment blue black '%~'
}

status_symbols(){
  local symbols
  symbols=()
  [[ $RETVAL -ne 0 ]] && symbols+="%{%F{red}%}➥ $RETVAL"

  [[ $UID -eq 0 ]] && symbols+="%{%F{yellow}%}⇧"
  [[ $(jobs -l | wc -l) -gt 0 ]] && symbols+="%{%F{cyan}%}◷"

  echo -n $symbols
}

# Status:
# - was there an error
# - am I root
# - are there background jobs?
prompt_status() {
  [[ -n "$(status_symbols)" ]] && prompt_segment black default "$(status_symbols)"
}

## Main prompt
build_prompt() {
  RETVAL=$?
  prompt_status
  prompt_time
  prompt_context
  prompt_dir
  # prompt_rvm
  prompt_git
  prompt_end
}

TMOUT=1
TRAPALRM() {
    zle reset-prompt
}

PROMPT='%{%f%b%k%}$(build_prompt) '
RPROMPT='%{%F{red}%}$(prompt_rbenv)$(prompt_rvm)%{%f%}'
