function fish_title
  if [ $_ != 'fish' ]
    echo -n "($_) "
  end
  for part in (pwd_home)
    echo -n "$part/"
  end
end
