function _shish_cbprintf -a bg fg
  # echo $argv[3..-1]
  set_color -b $bg $fg -o
  # printf $argv[3..-1]
  echo -n $argv[3..-1]
  set_color normal
end
