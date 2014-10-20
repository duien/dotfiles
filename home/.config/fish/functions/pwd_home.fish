function pwd_home
  echo $PWD | sed -e "s|^$HOME|~|" -e 's|^/private||' -e 's|^/||' | tr / \n
end
