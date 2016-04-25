# Based on https://geraldkaszuba.com/tweaking-fish-shell/

function _common_section
  printf $c1
  printf $argv[1]
  printf $c0
  printf ":"
  printf $cr
  printf $c2
  printf $argv[2]
  printf $argv[3]
  printf $cr
  printf $c0
  printf ", "
  printf $cr
end

function section
  _common_section $argv[1] $c3 $argv[2] $ce
end

function error
  _common_section $argv[1] $ce $argv[2] $ce
end

# function git_branch
#   set -g git_branch (git rev-parse --abbrev-ref HEAD ^ /dev/null)
#   if [ $status -ne 0 ]
#     set -ge git_branch
#     set -g git_dirty_count 0
#   else
#     set -g git_dirty_count (git status --porcelain  | wc -l | sed "s/ //g")
#   end
# end

function pwd_home
  echo $PWD | sed -e "s|^$HOME|~|" -e 's|^/private||' -e 's|^/||' | tr / \n
end

# magenta : not in git

# white   : git detached (maybe combine with color?)
# red     : git dirty
# yellow  : git staged
# cyan    : git untracked
# blue    : git clean
function fish_prompt_line
  # function fish_prompt

  set my_pwd (pwd_home)

  set -g color_high "white"
  set -g git_root (command git rev-parse --show-toplevel ^/dev/null)
  if test -n "$git_root"
    set -g hl_dir (basename $git_root)

    set -g git_branch (command  git symbolic-ref HEAD ^/dev/null | sed  "s#refs/heads/##")
    if test -n "$git_branch"

      # Set default git state color
      set -g color_base "blue"

      # Set color for untracked files if present
      set -l untracked (command git ls-files --other --exclude-standard)
      if test -n "$untracked"
        set -g color_base "cyan"
      end

      # Set color for staged files if present
      set -l staged (command git diff --cached --no-ext-diff --quiet --exit-code ; or echo "*")
      if test -n "$staged"
        set -g color_base "yellow"
      end

      # Set color for dirty status
      set -l dirty (command git diff --no-ext-diff --quiet --exit-code ; or echo "*")
      if test -n "$dirty"
        set -g color_base "red"
      end
    else
      set -g git_branch (_shish_git ref)
      set -g color_base "white"
      set -g color_high "red"
    end
  else
    set -g hl_dir $my_pwd[-1]
    set -g color_base "magenta"
  end

  if test $color_base = "white"
    set -g color_text "black"
  else
    set -g color_text $color_base
  end

  # c0 to c4 progress from dark to bright
  # ce is the error colour
  set -g c0 (set_color $color_text --bold --background $color_base) # punctuation
  set -g c1 (set_color $color_text --bold --background $color_base) # labels
  set -g c2 (set_color $color_text --bold --background $color_base) # outer pwd
  set -g c3 (set_color $color_high --bold --background $color_base) # branch, cur dir
  set -g c4 (set_color cyan                            ) # > prompt
  set -g ce (set_color $color_high --background $color_base) # numbers
  set -g cr (set_color normal)

  printf "$c0 "

  # # Git branch and dirty files
  # git_branch
  if set -q git_branch
    section git $git_branch
  end
  #   set out $git_branch
  #   if test $git_dirty_count -gt 0
  #     set out "$out$cr$c0:$cr$ce$git_dirty_count"
  #   end
  #   section git $out
  # end

  # Current Directory
  if test $my_pwd[1] != "~"
    printf "$c0/$cr"
  end

  for dir in $my_pwd
    if test $dir = $hl_dir
      printf $c3
      printf $dir
      if test $dir = $my_pwd[-1]
        printf $cr
      else
        printf "$c0/$cr"
      end
    else
      printf $c2
      printf $dir
      printf "$c0/$cr"
    end
  end


  set -ge git_branch
  set -ge git_root
end

function fish_prompt
  # $status gets nuked as soon as something else is run, e.g. set_color
  # so it has to be saved asap.
  set -g last_status $status

  # Set and print text of prompt statusline
  set prompt_str (fish_prompt_line)
  printf $prompt_str

  # Fill rest of line with background color
  printf (set_color --background $color_base)
  set cols (tput cols)
  set prompt_width (echo $prompt_str | ansifilter | wc -c)
  set remaining (math $cols - $prompt_width)
  printf (yes " " | head -n $remaining | tr -d "\n")

  # Next line
  printf " \n"
  printf (set_color normal)

  # Put last status at start of line
  if [ $last_status -ne 0 ]
    printf (set_color red)
    printf "$last_status"
    set -ge status
  end

  # And the prompt character
  printf "$c4> "
end

