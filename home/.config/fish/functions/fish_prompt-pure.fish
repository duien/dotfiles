function _pwd_with_tilde
  echo $PWD | sed 's|^'$HOME'\(.*\)$|~\1|' 
end

# pwd
# if in git dir, surround repo with bold


function _in_git_directory
  git rev-parse --git-dir > /dev/null 2>&1
end

function _git_branch_name_or_revision
  set -l branch (git symbolic-ref HEAD 2> /dev/null | sed -e 's|^refs/heads/||')
  set -l revision (git rev-parse HEAD 2> /dev/null | cut -b 1-7)

  if test (count $branch) -gt 0
    echo "¬ $branch"
  else
    echo $revision
  end
end

function _git_upstream_configured
  git rev-parse --abbrev-ref @"{u}" > /dev/null 2>&1
end

function _git_behind_upstream
  test (git rev-list --right-only --count HEAD...@"{u}" 2> /dev/null) -gt 0
end

function _git_ahead_of_upstream
  test (git rev-list --left-only --count HEAD...@"{u}" 2> /dev/null) -gt 0
end

function _git_dirty
  set -l is_git_dirty (command git diff --name-only --ignore-submodules 2>/dev/null)
  test -n "$is_git_dirty"
end

function _git_staged_changes
  set -l has_staged_changes (command git diff --name-only --cached 2>/dev/null)
  test -n "$has_staged_changes"
end

function _git_upstream_status
  set -l arrows

  if _git_upstream_configured
    if _git_behind_upstream
      set arrows "$arrows↓"
    end

    if _git_ahead_of_upstream
      set arrows "$arrows↑"
    end
  end

  echo $arrows
end

function _git_status
  set -l asterisk

  if _git_dirty
    set asterisk "$asterisk◇"
  end

  echo $asterisk
end

function _git_staged
  set -l indicator
  if _git_staged_changes
    set indicator "◆"
  end

  echo $indicator
end
# function _git_stash_status
#   set -l stash
#
#   set -l stash_count (git stash list | wc -l | string trim)
#   if test $stash_count -gt 0
#     set stash "($stash_count)"
#   end
#
#   for i in (seq 0 (echo "$stash_count - 1" | bc))
#     if test (git rev-parse stash@\{$i\}^) = (git rev-parse @)
#       set stash "from current"
#     end
#   end
#
#   echo $stash
# end

function _git_stash_count
  git stash list | wc -l | string trim
end

function _git_stash_current
  set -l stash_count (_git_stash_count)
  set -l stash_current

  if test $stash_count -gt 0
    set stash_current "△"
    for i in (seq 0 (echo "$stash_count - 1" | bc))
      if test (git rev-parse stash@\{$i\}^) = (git rev-parse @)
        set stash_current (_print_in_color "▲" red)
        break
      else
        set -l branch_commits (git log master.. --pretty=%H)
        if contains (git rev-parse stash@\{$i\}^) $branch_commits
          set stash_current (_print_in_color "△" red)
          break
        end
      end
    end
  end

  echo $stash_current
end

function _print_in_color
  set -l string $argv[1]
  set -l color  $argv[2..-1]

  if test -n "$string"
    set_color $color
    printf " "$string
    set_color normal
  end
end

function _status_prompt
  set -l last_status $argv[1]
  set -l out
  if test $last_status -ne 0
    set out $last_status
  end
  echo $out
end

function _prompt_color_for_status
  if test $argv[1] -eq 0
    echo green
  else
    echo red
  end
end

function _print_colored_stash
  set -l _stash (_git_stash_current)
  if test $_stash = "▲"
    _print_in_color $_stash red # c1651a
  else
    _print_in_color $_stash brblack
  end
end

function fish_prompt
  set -l last_status $status

  _print_in_color "\n"(_pwd_with_tilde) blue --bold

  if _in_git_directory
    # "\e[3m#{str}\e[0m"
    # _print_in_color " \e[3m"(_git_branch_name_or_revision)"\e[0m" magenta # 242
    _print_in_color (_git_branch_name_or_revision) magenta --italics
    _print_in_color (_git_upstream_status) cyan
    _print_in_color (_git_staged) green
    _print_in_color (_git_status) yellow # FCBC47
    _print_colored_stash
    # _print_in_color (_git_stash_count) black
    printf "\n"
  else
    printf " "
  end

  # printf "\n"
  # if test $last_status -ne 0
    # _print_in_color (_status_prompt $last_status) red
    printf (set_color (_prompt_color_for_status $last_status))"> "(set_color normal)
  # else
    # _print_in_color "▷ " (_prompt_color_for_status $last_status)
    # printf (set_color (_prompt_color_for_status $last_status))"$prompt_char "(set_color normal)
  # end

  # _print_in_color (_status_prompt $last_status) (_prompt_color_for_status $last_status)
end
