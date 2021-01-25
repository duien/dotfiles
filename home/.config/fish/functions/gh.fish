function gh --description 'gh pr status'
	if test -z "$argv"
    command gh pr status
  else if test "draft" = "$argv"
    # IDEA get pivotal story from branch name
    command gh pr create --draft
  else
    command gh $argv
  end
end
