set powerline_right \uE0B0
set powerline_right_soft \uE0B1
set powerline_left \uE0B2
set powerline_left_soft \uE0B3
set powerline_branch \uE0A0

#         red    green  yellow blue   purple cyan   orange
# bright  f73028 b8bb26 fabd2f 83a598 d3869b 7db669 fe8019
# neutral cc241d 98971a d79921 458588 b16286 578e57 d65d0e
# faded   890009 66620d a56311 0e5365 7b2b5e 356a46 9d2807
#
# grayscale 1d2021 282828 32302f 3c3836 504945 665c54 7c6f64 928374 a89984 bdae93 d5c4a1 ebdbb2 f2e5bc fbf1c7 f9f5d7

# Use $COLUMNS to determine window width

set red    f73028 cc241d 890009
set orange fe8019 d65d0e 9d2807
set yellow fabd2f d79921 a56311
set green  b8bb26 98971a 66620d
set cyan   7db669 54a367 00875f
set blue   87afaf 458588 005f87 # trying term color codes instead
set purple d3869b b16286 7b2b5e
set grayscale 1d2021 282828 32302f 3c3836 504945 665c54 7c6f64 928374 a89984 bdae93 d5c4a1 ebdbb2 f2e5bc fbf1c7 f9f5d7

set bg_normal $grayscale[2]

# If `git_root` is the root of a git repo, and the # of segments to show is 5:
#
# QUESTION: Should the branch count as a segment? Probably -- it does take
#           up a segment worth of space
#
# ~/git_root
# ~ < br > git_root
#
# ~/foo/git_root/bar
# ~ > foo < br > git_root > bar
#
# ~/foo/bar/git_root/baz
# ... > bar < br > git_root > bar
#
# ~/foo/git_root/bar/baz
# br > git_root > bar > baz
#
# ~/foo/bar/git_root/bar/baz
# br > git_root > bar > baz
#
# ~/foo/git_root/bar/baz/quux
# br > git_root > bar > baz > quux
#
# ~/git_root/foo/bar/baz/quux
# br > git_root ... bar > baz > quux
#
# And a special 2-or-3 segment example:
#
# ~/git_root/foo/bar/baz/quux
# br > git_root ... quux

# Ok, summarize that into rules:
#
# - always show git root and branch (2 segments)
# - in outside section, `...` counts as a segment
# - in inside section, use `...` as special separator
# - if `...` out be only segment in outside, skip it
#   (eg, if configured length is 1 and path length > 1)

function __shish_debug
  [ "$SHISH_DEBUG" ] ; and echo "[$argv]"
end

set SHISH_DEBUG
set __shish_bg_current

function __shish_print_in -a color
  # __shish_debug in $color
  # __shish_debug p $argv[2..-1]
  set_color normal
  set_color $color
  [ 'unstarted' != $__shish_bg_current ] ; and set_color -b $__shish_bg_current
  echo -n $argv[2..-1]
  set_color normal
  [ 'unstarted' != $__shish_bg_current ] ; and set_color -b $__shish_bg_current
end

function __shish_end
  # TODO Something werid is happening, but eh
  __shish_segment hard right normal # $bg_normal
  # echo -n ' '
end

function __shish_switch_bg -a color
  set_color normal
  set_color -b $color
  set __shish_bg_current $color
end

function __shish_print_list_in -a color separator -d "Print pretty list <color> <separator> <list>"
  set -l list $argv[3..-1]
  set -l length (count $list)

  if set -q list[2]
    for item in $list[1..-2]
      __shish_print_in $color $item
      __shish_print_in $color $separator
    end
  end
  __shish_print_in $color $list[-1]
end

function _shish_limit_to -a length -d "Limit list to n items (including `...`)"
  set -l list $argv[2..-1]
  if [ (count $list) -gt $length ]
    set -e list[1..(math -1\*$length-1)]
    set list[1] '⋯ '
  end
  for item in $list ; echo $item ; end
end

function __shish_segment -a kind direction color -d "Generate segment <kind> <direction> <color>"
  # Usage:
  #   __shish_segment <kind> <direction> <color>
  #
  # Options:
  #   <kind>        Type of separator: `hard` (filled arrow) or `soft` (open arrow)
  #   <direction>   Direction of separator: pointing `left` or `right`
  #   <color>       For hard: The color of the new segment to transition to
  #                 For soft: The color of the separator itself
  switch $kind
    case soft
      switch $direction ; case right ; __shish_print_in $color " $powerline_right_soft "
        case left  ; __shish_print_in $color " $powerline_left_soft "
      end
    case hard
      set -l unstarted
      [ "unstarted" = $__shish_bg_current ] ; and set unstarted true
      if [ -z "$unstarted" ]
        switch $direction
          case right
            echo -n " "
            set_color normal
            set_color $__shish_bg_current -b $color
            echo -n "$powerline_right "
          case left
            __shish_print_in $color " $powerline_left"
        end
      end

      __shish_switch_bg $color
      [ "$unstarted" -o "$kind $direction" = "hard left" ] ; and echo -n ' '
  end
end

function _shish_pretty_pwd
  echo (_shish_pretty_dir $PWD)
end

function _shish_pretty_dir -a directory -d "Current directory, with home turned into `~` if applicable"
  echo $directory | sed -e "s|^$HOME|~|" -e 's|^/private||' -e 's|^/||'
end

function _shish_ruby
  set -l actual_ruby_version (ruby -v | cut -f 2 -d ' ' | cut -f 1 -d 'p')
  if which rbenv | grep 'rbenv' > /dev/null
    # TODO For some stupid reason, `rbenv shell` works differently than
    # all the other commands and puts output somewhere that still shows
    # up even with STDOUT and STDERR redirected

    rbenv local ^ /dev/null > /dev/null; and echo -n 'l'
    rbenv global ^ /dev/null > /dev/null; and echo -n 'g'


    # # Check for local ruby version
    # set -l ruby_version (rbenv local ^ /dev/null)
    # if [ -n "$ruby_version" ]
    #   echo -n "⟡ $ruby_version"
    #   return
    # end

    # Check for global ruby version
    # set -l ruby_version (rbenv global ^ /dev/null)
    # if [ -n "$ruby_version" ]
    #   echo -n "∗ $ruby_version"
    #   return
    # end

    # Huh, that's weird
    # set -l ruby_version (rbenv version ^ /dev/null)
    # echo -n \? ($ruby_version | cut -f 1 -d ' ')

  else
    echo -n (ruby -v | cut -f 2 -d ' ' | cut -f 1 -d 'p') # ruby version
  end
end

# Prompt color setup
set shish_bg_pwd $grayscale[4]
set shish_fg_pwd $grayscale[7]
set shish_sp_pwd $grayscale[5]

set shish_git_clean     $cyan
set shish_git_untracked $blue
set shish_git_staged    $orange
set shish_git_dirty     $red

set shish_bg_branch $grayscale[1]
set shish_fg_branch $purple[3]
set shish_hl_branch $purple[1]

set shish_bg_detached $grayscale[-1]
set shish_fg_detached $purple[3]

# TODO Maybe set up all the syntax highlighting colors? o.O
# The following variables are available to change the highlighting colors in fish:
#
# fish_color_normal, the default color
# fish_color_command, the color for commands
# fish_color_quote, the color for quoted blocks of text
# fish_color_redirection, the color for IO redirections
# fish_color_end, the color for process separators like ';' and '&'
# fish_color_error, the color used to highlight potential errors
# fish_color_param, the color for regular command parameters
# fish_color_comment, the color used for code comments
# fish_color_match, the color used to highlight matching parenthesis
# fish_color_search_match, the color used to highlight history search matches
# fish_color_operator, the color for parameter expansion operators like '*' and '~'
# fish_color_escape, the color used to highlight character escapes like '\n' and '\x70'
# fish_color_cwd, the color used for the current working directory in the default prompt
#
# Additionally, the following variables are available to change the highlighting in the completion pager:
#
# fish_pager_color_prefix, the color of the prefix string, i.e. the string that is to be completed
# fish_pager_color_completion, the color of the completion itself
# fish_pager_color_description, the color of the completion description
# fish_pager_color_progress, the color of the progress bar at the bottom left corner
# fish_pager_color_secondary, the background color of the every second completion

# This would be the spot for a hook to run after a long-running command
# although doing so before figuring out how to ignore interactive commands
# would be annoying
# function _test_postexec --on-event fish_postexec
#   set -g __shish_last_command $argv
#   echo "post[$argv] $status $_"
# end

# This is the best start I've been able to make on saving the previous
# foreground job before current. This would let us check for a known
# list of interactive things, but that's far from perfect
# function __save_previous_command -v _
#   [ "$_" != 'fish' ] ; and set -g __ $_
# end

set shish_error_symbol "⚡︎" # "╰→"
set shish_duration_symbol "Δ" # "⟳ "

function fish_prompt
  set -l last_status $status
  set -l segments (math $COLUMNS/20)
  set __shish_bg_current 'unstarted'

  # Show return status of last command if non-zero
  [ $last_status -ne 0 ] ; and __shish_print_in $orange[2] "$shish_error_symbol $last_status "

  # Show the duration of the last command
  # NOTE There's some variation in the contents of the CMD_DURATION variable. In
  # slightly older versions of fish, it's preformatted number of seconds, and in
  # newer versions it's a raw millisecond value
  # TODO Find a way to skip duration for interactive commands
  if [ -n "$CMD_DURATION" ]
    # For numeric duration
    if [ "$CMD_DURATION" -eq "$CMD_DURATION" ]
      # Print if over 5 seconds
      [ "$CMD_DURATION" -gt 5000 ] ; and __shish_print_in $blue[2] "$shish_duration_symbol $CMD_DURATION ms"
    else
      __shish_print_in $blue[2] "$shish_duration_symbol $CMD_DURATION"
    end
  end
  echo


  # Ruby version (hacky)
  # __shish_segment hard right $red[3]
  # __shish_print_in $red[1] (_shish_ruby)

  # Are we in a git repository?
  set -l git_root (_shish_git root)
  if [ "$git_root" ]
    # Display prompt with git info
    # components : outside / branch / root / inside
    set -l remaining_length (math $segments-2) # segments after branch & root
    set -l inside (echo "$PWD" | sed -e "s#$git_root##g" -e 's#^/##' | tr / \n)
    [ -z "$inside" ] ; and set -e inside
    set -l outside_length (math $remaining_length-(count $inside))

    set -l outside (echo (_shish_pretty_dir $git_root) | tr / \n)
    set -l root $outside[-1]
    set -e outside[-1]

    # If we would show only '...' outside, skip it entirely
    if [ $outside_length -eq 1 -a (count $outside) -gt 1 ]
      set outside_length 0
    end

    set -l inside_count (count $inside)
    [ -z "$inside" ] ; and set inside_count 0
    set -l inside_length
    if [ $outside_length -gt 0 ]
      set inside_length (math $remaining_length-$outside_length)
    else
      set inside_length $remaining_length
    end

    # Print outside segments if we ended up with any
    if [ $outside_length -gt 0 ]
      __shish_segment hard right $shish_bg_pwd
      __shish_print_list_in $shish_fg_pwd (__shish_segment soft right $shish_sp_pwd) \
          (_shish_limit_to $outside_length $outside)
    end

    # Print branch info
    set -l branch (_shish_git branch)
    if [ $branch ]
      __shish_segment hard left $shish_bg_branch
      if [ $branch = 'master' ]
        __shish_print_in $shish_fg_branch $branch
      else
        set -l branch_bits (echo $branch | tr / \n)
        if set -q branch_bits[2]
          __shish_print_list_in $shish_fg_branch '/' $branch_bits[1..-2]
          __shish_print_in $shish_fg_branch '/'
        end
        __shish_print_in $shish_hl_branch $branch_bits[-1]
      end
    else
      __shish_segment hard left $shish_bg_detached
      __shish_print_in $shish_fg_detached (_shish_git ref)
    end


    # Determine git status color
    set -l shish_status_color $shish_git_clean
    if      [ (_shish_git dirty) ]     ; set shish_status_color $shish_git_dirty
    else if [ (_shish_git staged) ]    ; set shish_status_color $shish_git_staged
    else if [ (_shish_git untracked) ] ; set shish_status_color $shish_git_untracked
    end

    # Print git root
    __shish_segment hard right $shish_status_color[3]
    __shish_print_in $grayscale[-1] $root

    # Print segments inside repo, if any
    if [ $inside_count -gt 0 ]

      # We have room for all the segments
      if [ $inside_length -ge $inside_count ]
        # __shish_segment soft right $shish_status_color[2]
        # __shish_print_list_in $shish_status_color[1] (__shish_segment soft right $shish_status_color[2]) $inside

        __shish_segment hard right $shish_status_color[2]
        __shish_print_list_in $grayscale[-3] (__shish_segment soft right $shish_status_color[3]) $inside



      # Only show some segments
      else
        set inside_length (math $inside_length+1)
        [ $inside_length -eq 1 ] ; and set inside_length 2
        set -l visible_inside (_shish_limit_to $inside_length $inside)


        __shish_segment hard right $shish_status_color[2]
        __shish_print_in $shish_status_color[3] '⋯  '
        __shish_print_list_in $grayscale[-3] (__shish_segment soft right $shish_status_color[3]) $visible_inside[2..-1]
        
        # __shish_print_in $shish_status_color[2] ' ⋯  '
        # __shish_print_list_in $shish_status_color[1] (__shish_segment soft right $shish_status_color[2]) $visible_inside[2..-1]
      end
    end

  else
    # Display prompt without git info
    __shish_segment hard right $shish_bg_pwd
    __shish_print_list_in $shish_fg_pwd (__shish_segment soft right $shish_sp_pwd) \
      (_shish_limit_to $segments (_shish_pretty_pwd | tr / \n))
  end
  __shish_end
end

# TODO Return status of last command
function messy_fish_prompt
  set -l segments (math $COLUMNS/20)
  echo "[seg|$segments]"

  set -l git_root (command git rev-parse --show-toplevel ^/dev/null)
  if [ "$git_root" ]
    set -l path_bits (_shish_segments $git_root)
    set -l dir_inside_git (echo "$PWD" | sed -e "s#$git_root##g" -e 's#^/##')
    set -l path_bits_inside (_shish_segments $dir_inside_git)
    set -l inside_length
    if [ $dir_inside_git ]
      set inside_length (count $path_bits_inside)
      if [ $inside_length -gt (math $segments-1) ]
        set inside_length (math $segments-1)
      end
    else
      set inside_length 0
    end
    set -l outside_length (math $segments-1-$inside_length)

    set -l dirty (_shish_git dirty)
    set -l staged (_shish_git staged)
    set -l untracked (_shish_git untracked)

    set -l status_colors
    if [ $dirty ] ; set status_colors $orange
    else if [ $staged ] ; set status_colors $yellow
    else if [ $untracked ] ; set status_colors $blue
    # else if [ $stashed ] ; set status_colors $yellow
    # else if [ $ahead ] ; set status_colors $green
    else ; set status_colors $cyan
    end

    set -l ref (command git symbolic-ref HEAD ^/dev/null)
    set -l fg_branch $purple[1]
    set -l bg_branch $grayscale[1]
    if [ $status -gt 0 ]
      set -l branch (command git show-ref --head -s --abbrev | head -n1 ^/dev/null)
      set ref $branch
      set bg_branch $grayscale[-1]
      set fg_branch $purple[3]
    end
    set ref (echo $ref | sed  "s#refs/heads/##")
    if [ "master" = $ref ]
      set fg_branch $purple[3]
    end

    # everything leading up to git root
    if test $outside_length -gt 0
      _shish_cprintf $grayscale[4] $grayscale[6] ' '
      _shish_list $outside_length " $powerline_right_soft " $grayscale[4] $grayscale[7] $grayscale[5] $path_bits[1..-2]
      _shish_transition $grayscale[4] $bg_branch $powerline_left
    else
      _shish_cprintf $bg_branch $status_colors[1] ' '
    end

    # git branch / status
    set -l branch_segments (_shish_segments $ref)
    if [ (count $branch_segments) -gt 1 ]
      _shish_list (count $branch_segments) '/' $bg_branch $purple[3] $purple[3] $branch_segments[1..-2]
      _shish_cprintf $bg_branch $purple[3] '/'
    end
    _shish_cprintf $bg_branch $fg_branch $branch_segments[-1]
    _shish_transition $bg_branch $status_colors[3] $powerline_right true

    # the git root
    _shish_cbprintf $status_colors[3] $grayscale[-1] $path_bits[-1]

    # and the dir inside git
    if test $inside_length -gt 0
      _shish_cprintf $status_colors[3] $status_colors[2] " $powerline_right_soft "
      _shish_list $inside_length " $powerline_right_soft " $status_colors[3] $status_colors[1] $status_colors[2] $path_bits_inside
    end
    _shish_transition $status_colors[3] $bg_normal $powerline_right true
  else
    # not inside of git

    _shish_cprintf $grayscale[4] $grayscale[7] ' '
    _shish_pwd $segments " $powerline_right_soft " $grayscale[4] $grayscale[7] $grayscale[5] $grayscale[-1] (_shish_segments $PWD)
    _shish_transition $grayscale[4] $bg_normal $powerline_right true
  end
end
