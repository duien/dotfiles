function fish_prompt --description 'Write out the prompt'

	set -l last_status $status

	if not set -q __fish_prompt_normal
		set -g __fish_prompt_normal (set_color normal)
	end

  set -xg previous_background "start"
	# fish_prompt_segment white 005F87 (prompt_pwd)
	fish_prompt_segment blue black (prompt_pwd)
  # fish_prompt_segment d70000 5f0000 ⟡ (prompt_ruby)
	fish_prompt_segment red black (prompt_ruby)
  fish_git_prompt
  # if not test $last_status -eq 0
  #   # fish_prompt_segment d70000 black "➥ $last_status"
	# 	fish_prompt_segment red black "➥ $last_status"
  # end
  fish_prompt_end

end
