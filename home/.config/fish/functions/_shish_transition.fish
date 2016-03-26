function _shish_transition -a from to separator
  if set -q argv[4] # optional 'reverse' arg
    _shish_cprintf $from $to " "
    _shish_cprintf $to $from "$separator "
  else
    _shish_cprintf $from $to " $separator"
    _shish_cprintf $to $from " "
  end
end
