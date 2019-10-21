function experiment
  # ...
  # tput sc
  tput smcup
  tput civis
  # tput home
  set_color yellow --reverse
  echo -n " HELLO WORLD "
  sleep 1

  for i in (seq 1 6)
    tput cub1
    # tput dch1
    echo -n " "
    tput cub1
    sleep 0.07
  end
  sleep 1
  for c in E M I L Y
    echo -n $c
    sleep 0.1
  end
  echo -n " "

  set_color normal
  # tput rc
  sleep 2
  # tput flash
  tput rmcup
end
