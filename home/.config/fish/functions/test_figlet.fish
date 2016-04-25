function test_figlet
  if test -n "$argv[1]"
    set message $argv[1]
  else
    set message (date '+%Y-%m-%d')
  end
  echo "message: $message"
  for font in (ls (figlet -I2))
    set_color --bold magenta
    echo $font
    set_color normal
    figlet -f $font $message
    echo ""
  end
end
