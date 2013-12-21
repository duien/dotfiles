function fish_right_prompt --description 'Write out the right prompt'

  set_color $fish_color_virtualenv

  # ruby -v | awk '{split($0,a," "); print a[2]}'
  ruby -v | cut -f 2 -d ' '

end
