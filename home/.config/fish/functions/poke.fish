function poke
  mkdir -p (dirname "$argv[1]")
  touch "$argv[1]"
end