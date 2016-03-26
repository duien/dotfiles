function _shish_segments -a directory
  echo $directory | sed -e "s|^$HOME|~|" -e 's|^/private||' -e 's|^/||' | tr / \n
end
