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
end
