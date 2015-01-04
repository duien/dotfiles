function fish_right_prompt --description 'Write out the right prompt'

  # set_color $fish_color_virtualenv
  # prompt_ruby

  set -l last_status $status
  
  if not test $last_status -eq 0
    # fish_prompt_segment d70000 black "➥ $last_status"
    # fish_prompt_segment red black "➥ $last_status"
    set_color red
    echo -n "➥ $last_status"
    set_color normal
  end

end
