function prompt_ruby
  # Maybe use ∗ for global and ⟡ for local
  if which rbenv | grep 'rbenv' > /dev/null
    # Check for local ruby version
    set -l ruby_version (rbenv local ^ /dev/null)
    if [ -n "$ruby_version" ]
      echo -n "⟡ $ruby_version"
      return
    end

    # Check for global ruby version
    set -l ruby_version (rbenv global ^ /dev/null)
    if [ -n "$ruby_version" ]
      echo -n "∗ $ruby_version"
      return
    end

    # Huh, that's weird
    set -l ruby_version (rbenv version ^ /dev/null)
    echo -n \? ($ruby_version | cut -f 1 -d ' ')
  else
    echo -n (ruby -v | cut -f 2 -d ' ' | cut -f 1 -d 'p') # ruby version
  end
end
