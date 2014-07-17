# maybe fool with this and try setting the symbol in color 875F00

function fish_git_prompt -d "Display the actual git state"
  set -l ref
  set -l dirty
  set -l status_symbol \uE0A0

  set -l repo_info (command git rev-parse --git-dir --is-inside-git-dir --is-bare-repository --is-inside-work-tree --short HEAD ^/dev/null)
  test -n "$repo_info"; or return

  set -l git_dir $repo_info[1]
  set -l inside_worktree $repo_info[4]

  set -l rbc (fish_git_prompt_operation_branch $repo_info)
  set status_symbol $rbc[1]
  set branch        $rbc[2]
  
  set dirty (parse_git_dirty)
  set -l branch (echo $branch | sed  "s-refs/heads/--")
  if [ "$dirty" != "" ]
    fish_prompt_segment black yellow "$status_symbol$branch"
  else
    fish_prompt_segment black green "$status_symbol$branch"
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

# set -g fish_git_prompt_char_branch        ""
set -e fish_git_prompt_char_branch
set -g fish_git_prompt_char_rebase        "⤻ "
set -g fish_git_prompt_char_rebase_i      "$fish_git_prompt_char_rebase i"
set -g fish_git_prompt_char_rebase_m      "$fish_git_prompt_char_rebase m"

set -g fish_git_prompt_char_am            "AM "
set -g fish_git_prompt_char_am_rebase     "AM/REBASE "
set -g fish_git_prompt_char_merging       "⑃ "
set -g fish_git_prompt_char_cherrypicking "∷ "
set -g fish_git_prompt_char_reverting     "↩ "
set -g fish_git_prompt_char_bisecting     "⁒ "

function fish_git_prompt_operation_branch --description "__fish_git_prompt helper, returns the current Git operation and branch"
  # This function is passed the full repo_info array
  set -l git_dir         $argv[1]
  set -l inside_gitdir   $argv[2]
  set -l bare_repo       $argv[3]
  set -l short_sha
  if test (count $argv) = 5
    set short_sha $argv[5]
  end

  set -l branch
  set -l operation $fish_git_prompt_char_branch
  set -l detached no
  set -l bare
  set -l step
  set -l total
  set -l os

  if test -d $git_dir/rebase-merge
    set branch (cat $git_dir/rebase-merge/head-name ^/dev/null)
    set step (cat $git_dir/rebase-merge/msgnum ^/dev/null)
    set total (cat $git_dir/rebase-merge/end ^/dev/null)
    if test -f $git_dir/rebase-merge/interactive
      set operation $fish_git_prompt_char_rebase_i
    else
      set operation $fish_git_prompt_char_rebase_m
    end
  else
    if test -d $git_dir/rebase-apply
      set step (cat $git_dir/rebase-apply/next ^/dev/null)
      set total (cat $git_dir/rebase-apply/last ^/dev/null)
      if test -f $git_dir/rebase-apply/rebasing
        set branch (cat $git_dir/rebase-apply/head-name ^/dev/null)
        set operation $fish_git_prompt_char_rebase
      else if test -f $git_dir/rebase-apply/applying
        set operation $fish_git_prompt_char_am
      else
        set operation $fish_git_prompt_char_am_rebase
      end
    else if test -f $git_dir/MERGE_HEAD
      set operation $fish_git_prompt_char_merging
    else if test -f $git_dir/CHERRY_PICK_HEAD
      set operation $fish_git_prompt_char_cherrypicking
    else if test -f $git_dir/REVERT_HEAD
      set operation $fish_git_prompt_char_reverting
    else if test -f $git_dir/BISECT_LOG
      set operation $fish_git_prompt_char_bisecting
    end
  end

  if test -n "$step" -a -n "$total"
    set operation "$operation $step/$total"
  end

  if test -z "$branch"
    set branch (command git symbolic-ref HEAD ^/dev/null; set os $status)
    if test $os -ne 0
      set detached yes
      set branch (switch "$__fish_git_prompt_describe_style"
            case contains
              command git describe --contains HEAD
            case branch
              command git describe --contains --all HEAD
            case describe
              command git describe HEAD
            case default '*'
              command git describe --tags --exact-match HEAD
            end ^/dev/null; set os $status)
      if test $os -ne 0
        if test -n "$short_sha"
          set branch $short_sha...
        else
          set branch unknown
        end
      end
      set branch "($branch)"
    end
  end

  if test "true" = $inside_gitdir
    if test "true" = $bare_repo
      set bare "BARE:"
    else
      # Let user know they're inside the git dir of a non-bare repo
      set branch "GIT_DIR!"
    end
  end

  echo $operation
  echo $branch
  echo $detached
  echo $bare
end
