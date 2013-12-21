function prompt_pwd --description "Print the current working directory, shortened to fit the prompt"
  set -l directory_parts (echo $PWD | sed -e "s|^$HOME|~|" -e 's|^/private||' | tr / \n)
  # echo $directory_parts[-3 -2 -1]

  set_color --background 005F87 white

  set -l start -4

  if set -q directory_parts[-5]
    echo -n '⋯ '
    set_color blue
    echo -n '  '
    set_color white
    set start -3
  end

  for i in (seq $start -2)
    if set -q directory_parts[$i]
      set -l part $directory_parts[$i]
      echo -n "$part"
      set_color blue
      echo -n '  '
      set_color white
    end
  end

  set_color --bold white
  echo -n $directory_parts[-1]
  set_color normal
  set_color --background 005F87
end
