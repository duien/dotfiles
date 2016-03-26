function _shish_list -a length separator background foreground fg_separator
  # [1] length       : max number of segments to show
  # [2] separator    : character(s) to separate segments
  # [3] background   : background color
  # [4] foreground   : main forground color
  # [5] fg_separator : foreground for separators
  # [6..-1]          : the segments themselves

  set -l segments $argv[6..-1]
  set -l start (math -1\*$length)
  if set -q segments[(math $start-1)]
    # We have more segments than we can display. Start with "⋯" and display one
    # fewer full segment
    _shish_cprintf $background $foreground '⋯ '
    _shish_cprintf $background $fg_separator $separator
    set start (math $start+1)
  end

  if test $start -le -2
    for i in (seq $start -2)
      # Print segments until second-to-last
      if set -q segments[$i]
        _shish_cprintf $background $foreground $segments[$i]
        _shish_cprintf $background $fg_separator $separator
      end
    end
  end

  # Print the last segment
  _shish_cprintf $background $foreground $segments[-1]
end
