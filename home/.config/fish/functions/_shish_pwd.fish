function _shish_pwd -a segments separator background foreground fg_separator fg_current
  # [1] segments     : number of segments
  # [2] separator    : character(s) to separate segments
  # [3] background   : background color
  # [4] foreground   : main forground color
  # [5] fg_separator : foreground for separators
  # [6] fg_current   : foreground for current directory

  if [ (count $argv) -gt 7 ]
    set -l directory_parts $argv[7..-2]
    _shish_list (math $segments-1) $separator $background $foreground $fg_separator $directory_parts
    _shish_cprintf $background $fg_separator $separator
  end

  _shish_cbprintf $background $fg_current $argv[-1]
end
