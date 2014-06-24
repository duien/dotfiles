function colors -d "What do my terminal colors look like?"
  set -l colors normal black red green yellow blue purple cyan white

  echo "=== THE COLORS ==="

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
