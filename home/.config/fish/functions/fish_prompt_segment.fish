function fish_prompt_segment --description 'Print out a segment of the prompt'
  set foreground $argv[1]
  set background $argv[2]
  if not set -q argv[3]
    return
  end
  if set -q argv[4]
    set contents $argv[3..-1]
  else
    set contents $argv[3]
  end

  if test $previous_background != 'start'
    echo -n " "
  #   set_color --background $background $previous_background
  #   echo -n ""
  end

  set_color --background $background $foreground
  echo -n " $contents"
  set previous_background $background
end


# Code point  Glyph Description
# U+E0A0   Version control branch
# U+E0A1   LN (line) symbol
# U+E0A2   Closed padlock
# U+E0B0   Rightwards black arrowhead
# U+E0B1   Rightwards arrowhead
# U+E0B2   Leftwards black arrowhead
# U+E0B3   Leftwards arrowhead
