function prompt_ruby
  if which rbenv | grep 'rbenv' > /dev/null
    for env in local global
      set -l ruby_version (rbenv $env ^ /dev/null)
      if [ -n "$ruby_version" ]
        echo -n $ruby_version \($env\)
        break
      end
      # if rbenv $env | grep 'version configured' > /dev/null ^ /dev/null
      #   # no version configured
      #   echo -n "no $env"
      # else
      #   echo -n "yes $env"
      # end
    end
  else
    echo -n (ruby -v | cut -f 2 -d ' ' | cut -f 1 -d 'p') # ruby version
  end
end
