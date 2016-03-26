function _shish_cprintf -a bg fg
  # echo $argv[3..-1]
  set_color -b $bg $fg
  # printf $argv[3..-1]
  echo -n $argv[3..-1]
  set_color normal
end
