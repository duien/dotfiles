function gh --description 'open in Github app'
  set -l directory '.'
  if test -n "$argv"
    set directory $argv
  end

	open -a Github $directory
end
