
function _get_recommended_exercise
  # argv should be track name


  set -l token (cat ~/.config/exercism/user.json | jq -r '.token')
  # call with hard-coded URL instead of the one from config because that one is
  # v1 and we need v2
  curl -s \
    --header "Authorization: Bearer $token" \
    "https://api.exercism.io/v2/tracks/$argv/exercises" \
    | jq -r '.exercises[] | select(.is_recommended) | .slug'
end

function xm
  set -l all_args $argv
  # exercism download -telixir -espiral-matrix
  argparse 't/track=' 'e/exercise=' -- $argv

  if test -n "$argv" # there are left-over args
    exercism $all_args
    return $status
  end

  set -l workspace (cat ~/.config/exercism/user.json | jq -r '.workspace')
  set -l track
  if test -n "$_flag_track"
    set track $_flag_track
  else
    set track (string match -rg (string join '/' $workspace '([^/]+)') (pwd))
  end
  if test -z "$track"
    echo "You must either be in an Exercism directory or specify a track"
    return 1
  end

  echo "Found track: $track"

  set -l exercise
  if test -n "$_flag_exercise"
    set exercise $_flag_exercise
  else
    set exercise (_get_recommended_exercise $track) # TODO manual specification
  end

  if test -z "$exercise"
    echo "Can't figure out what exercise to open"
    return 1
  end
  echo "Starting '$exercise' in '$track'..."

  exercism download -t$track -e$exercise 2> /dev/null > /dev/null

  cd "$workspace/$track/$exercise"
  set -l solution (cat .exercism/config.json | jq -r '.files.solution[]')
  # $EDITOR (pwd) $solution
  em $solution
end
