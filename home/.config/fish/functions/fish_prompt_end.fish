function fish_prompt_end
  if test $previous_background != 'start'
    echo -n " "
    # set_color --background normal $previous_background
    # echo -n " "
    set_color normal
    echo -n " "
    set -e previous_background
  end
end
