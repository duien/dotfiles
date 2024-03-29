function colors -d "What do my terminal colors look like?"
  set -l colors black red green yellow blue purple cyan white

  for color in $colors
    set_color --background $color
    set_color --bold
    # echo -n " "
    echo -n $(string pad -c" " -w7 $color)
    set_color normal
    set_color br$color --background $color
    echo -n " on "
    set_color --bold br$color
    echo -n "self "
    set_color normal
    set_color br$color --background $color -r
    echo -n " and "
    set_color --bold
    echo -n "reversed "
    set_color normal
    set_color $color
    echo -n " regular"
    set_color br$color
    echo -n " bright"
    set_color normal
    echo " "
  end
end

function colors_grid -d "More complete view of the colors"
  set -l colors normal black red green yellow blue purple cyan white

  for background in $colors
    set_color --background $background
    
    for foreground in $colors
      set_color $foreground
      echo -n " $foreground "
      set_color --bold
      echo -n " bold "
      set_color normal
      set_color --background $background
    end
    
    set_color normal
    echo " on $background "
  end
end
