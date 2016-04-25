function _do_fig
  figlet -c sweet sweetness
  # figlet -c may the pink
  # figlet -c be with you
end

function fish_greeting -d "what's up, fish?"
  # figlet -c (date '+%Y-%m-%d')
  set_color --background magenta
  printf (yes " " | head -n (tput cols) | tr -d "\n")
  set_color normal
  printf "\n"
  _do_fig | lolcat
  printf "\n"
end
