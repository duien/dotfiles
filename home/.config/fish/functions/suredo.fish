# Replaces the current commandline with the last command you ran, prefixed
# with `sudo`
# Because this replaces the commandline rather than running it, you have a
# chance to interactively edit the command before rerunning. Yes, there are a
# million ways to do this already, but I liked the name, and it was easier than
# the history editing I was doing.
#
function suredo -d "Redo the last command with sudo. Sure, do it!"
  commandline --replace "sudo $history[1]"
end
