function recompile_all_the_rubies -d "Uninstall all the rbenv rubies and recompile them"
  if test (count $argv) -eq 1
    echo "Usage:"
    echo "    recompile_all_the_rubies [--because library]"
    return
  end
  
  set_color green
  echo "Update homebrew"
  set_color normal
  brew update
  
  if test (count $argv) -gt 1
    if test '--because' = $argv[1]
      set_color green
      echo "Upgrade $argv[2]"
      set_color normal
      brew upgrade $argv[2]
    end
  end
  
  set_color green
  echo "Upgrade ruby-build"
  set_color normal
  brew upgrade ruby-build
  
  
  for ruby in (rbenv versions --bare)
    set_color red
    echo "Uninstall and re-install $ruby"
    set_color normal
    rbenv uninstall $ruby
    rbenv install $ruby
    gem pristine --all
  end
end
