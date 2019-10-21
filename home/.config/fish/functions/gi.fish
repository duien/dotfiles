function gi
  # set command (string replace --regex '^t' '' $argv[1])
  # echo $command
  if set args (string replace --regex '^t' '' $argv)
    git $args
  else
    echo "That doesn't look like git"
  end
end
