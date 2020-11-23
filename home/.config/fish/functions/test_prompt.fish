function _reset_color
  set_color normal
  set_color $argv
end

function test_prompt
# 
  set_color blue --background black
  printf " ~/Code/"
  set_color brblue --bold
  printf "layouts"
  _reset_color blue --backgroun black
  printf "/views "
  _reset_color black
  echo ""
  set_color normal

end
