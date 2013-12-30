function prompt_ruby
  # set_color d70000
  echo -n (ruby -v | cut -f 2 -d ' ' | cut -f 1 -d 'p') # ruby version
  # set_color 870000
  # echo -n '  '
  # echo -n p
  # set_color d70000
  # echo -n (ruby -v | cut -f 2 -d ' ' | cut -f 2 -d 'p') # ruby patch
  # echo -n (ruby -v | cut -f 2 -d ' ') # full ruby version and patch
end
