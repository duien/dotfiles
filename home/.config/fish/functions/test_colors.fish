function test_colors
  for color in (set_color --print-colors)
    set_color $color
    printf $color
  end
  printf "\n"
  for color in (set_color --print-colors)
    set_color $color --bold
    printf $color
  end
  printf "\n"
  for color in (set_color --print-colors)
    set_color $color --bold --background $color
    printf $color
  end
  printf "\n"

  # black blue brblack brblue brcyan brgreen brmagenta brred brwhite bryellow cyan green magenta red white yellow normal
  set_color black --bold --background brblack
  printf black
  set_color blue --bold --background brblue
  printf blue
  set_color brblack --bold --background black
  printf  brblack
  set_color brblue --bold --background blue
  printf brblue
  set_color brcyan --bold --background cyan
  printf brcyan
  set_color brgreen --bold --background green
  printf brgreen
  set_color brmagenta --bold --background magenta
  printf brmagenta
  set_color brred --bold --background red
  printf brred
  set_color brwhite --bold --background white
  printf brwhite
  set_color bryellow --bold --background yellow
  printf bryellow
  set_color cyan --bold --background brcyan
  printf cyan
  set_color green --bold --background brgreen
  printf green
  set_color magenta --bold --background brmagenta
  printf magenta
  set_color red --bold --background brred
  printf red
  set_color white --bold --background brwhite
  printf white
  set_color yellow --bold --background bryellow
  printf yellow
  set_color normal --bold --background normal
  printf normal
end
