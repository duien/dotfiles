function _reset_color
  set_color normal
  set_color $argv
end

function test_prompt
# 
  set_color --background blue
  printf " ~/Code "
  _reset_color magenta --background blue
  printf ""
  _reset_color brwhite --bold --background magenta
  printf " layouts "
  
  # _reset_color magenta --background brmagenta
  # printf ""
  _reset_color brmagenta --background magenta
  printf ""

  _reset_color --background brmagenta
  printf " master "
  _reset_color brmagenta --background blue
  printf ""
  _reset_color --background blue
  printf " views "
  _reset_color blue
  echo ""
  set_color normal

end
