function prompt_pwd --description "Print the current working directory, shortened to fit the prompt"
  set -l directory_parts (pwd_home)
  # echo $directory_parts[-3 -2 -1]

  # set -l background_color 005F87
  set -l background_color blue
  set -l segment_color white
  # set -l separator_color blue
  set -l separator_color cyan

  set_color --background $background_color $segment_color

  set -l start -4

  if set -q directory_parts[-5]
    echo -n '⋯ '
    set_color $separator_color
    echo -n '  '
    set_color $segment_color
    set start -3
  end

  for i in (seq $start -2)
    if set -q directory_parts[$i]
      set -l part $directory_parts[$i]
      echo -n "$part"
      set_color $separator_color
      echo -n '  '
      set_color $segment_color
    end
  end

  set_color --bold $segment_color
  echo -n $directory_parts[-1]
  set_color normal
  set_color --background $background_color
end
