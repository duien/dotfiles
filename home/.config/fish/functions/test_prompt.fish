function test_prompt
  set_color black --background green
  printf "hello ╱ world "
  set_color normal
  set_color green --reverse
  printf "║│┃┊┋┆┇ stuff╱things ╳ ▊▋▌▍▎▏▕▐"
  set_color normal
  set_color black --background green
  printf " wahtever 𐩕 ꩜ ᪤ "
  set_color red
  printf "⦙ ∴ ∵ ∷ ☼ "
  set_color normal
end
