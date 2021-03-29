function prepend_if_exists
  # prepend__if_exists VAR DIR [DIR ...]

  # if `dir` exists and is not already included in `var` then prepend it
  # this is useful for adding things to your path from a file that could be
  # reloaded. it could replace something like the following:
  #
  # if test -d "$HOME/.bin"
  #   set PATH $HOME/.bin $PATH
  # end
  set -l var $argv[1]
  set -l dirs $argv[-1..2]

  for dir in $dirs
   if test -d "$dir"
      if not contains $dir $$var
        set -g $var $dir $$var
      end
    end
  end
end
