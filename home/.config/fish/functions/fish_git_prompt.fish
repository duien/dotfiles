# function fish_git_prompt
#   # fish_prompt_segment black $git_status_color (__fish_git_prompt "%s")
#   if not is_git_repo
#     return
#   end
#   if git_dirty
#   # c94c22
#   # bb3613
#   # AF5F00
#   #
#     fish_prompt_segment black yellow (__git_ps1 '%s')
#   else
#     fish_prompt_segment black green (__git_ps1 '%s')
#   end

# end

# function __git_ps1
#   set -l g (git rev-parse --git-dir ^/dev/null)
#   if [ -n "$g" ]
#     set -l r ""
#     set -l b ""

#     if [ -d "$g/../.dotest" ]
#       if [ -f "$g/../.dotest/rebasing" ]
#         set r "|REBASE"
#       elseif [ -f "$g/../.dotest/applying" ]
#         set r "|AM"
#       else
#         set r "|AM/REBASE"
#       end

#       set b (git symbolic-ref HEAD ^/dev/null)
#     elseif [ -f "$g/.dotest-merge/interactive" ]
#       set r "|REBASE-i"
#       set b (cat "$g/.dotest-merge/head-name")
#     elseif [ -d "$g/.dotest-merge" ]
#       set r "|REBASE-m"
#       set b (cat "$g/.dotest-merge/head-name")
#     elseif [ -f "$g/MERGE_HEAD" ]
#       set r "|MERGING"
#       set b (git symbolic-ref HEAD ^/dev/null)
#     else
#       if [ -f "$g/BISECT_LOG" ]
#         set r "|BISECTING"
#       end

#       set b (git symbolic-ref HEAD ^/dev/null)
#       if [ -z $b ]
#         set b (git describe --exact-match HEAD ^/dev/null)
#         if [ -z $b ]
#           set b (cut -c1-7 "$g/HEAD")
#           set b "$b..."
#         end
#       end
#     end

#     set b (echo $b | sed -e 's|^refs/heads/||')

#     # set_git_color
#     printf "%s" "$r $b" ^/dev/null
#     # set_color normal
#   end
# end

# function git_dirty
#   if not is_git_repo
#     return 1
#   end
#   not git diff HEAD --quiet ^/dev/null
# end

# function is_git_repo
#   git status >/dev/null ^/dev/null
#   not test $status -eq 128
# end

function fish_git_prompt -d "Display the actual git state"
  set -l ref
  set -l dirty
  if command git rev-parse --is-inside-work-tree >/dev/null 2>&1
    set dirty (parse_git_dirty)
    set ref (command git symbolic-ref HEAD 2> /dev/null)
    set ref (command git symbolic-ref HEAD 2> /dev/null)
    if [ $status -gt 0 ]
      set -l branch (command git show-ref --head -s --abbrev |head -n1 2> /dev/null)
      set ref "➦ $branch"
    end
    set branch_symbol \uE0A0
    set -l branch (echo $ref | sed  "s-refs/heads/-$branch_symbol -")
    if [ "$dirty" != "" ]
      fish_prompt_segment black yellow "$branch"
    else
      fish_prompt_segment black green "$branch"
    end
  end
end

set -g __fish_git_prompt_showdirtystate 'yes'
set -g __fish_git_prompt_char_dirtystate '±'
set -g __fish_git_prompt_char_cleanstate ''

function parse_git_dirty
  set -l submodule_syntax
  set submodule_syntax "--ignore-submodules=dirty"
  set git_dirty (command git status -s $submodule_syntax  2> /dev/null)
  if [ -n "$git_dirty" ]
    if [ $__fish_git_prompt_showdirtystate = "yes" ]
      echo -n "$__fish_git_prompt_char_dirtystate"
    end
  else
    if [ $__fish_git_prompt_showdirtystate = "yes" ]
      echo -n "$__fish_git_prompt_char_cleanstate"
    end
  end
end
