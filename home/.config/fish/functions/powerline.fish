function powerline
  set -l separator_left \UE0B0 \UE0B4 \UE0B8 \UE0BC \UE0C0 \UE0C4 \UE0C6 \UE0C8
  set -l separator_right \UE0B2 \UE0B6 \UE0BA \UE0BE \UE0C2 \UE0C5 \UE0C7 \UE0CA
  set -l divider_glyphs \UE0B1 \UE0B3 \UE0B5 \UE0B7 \UE0B9 \UE0BB \UE0BD \UE0BF \UE0A0

  for glyph in $separator_left
    set_color black --background blue
    printf " "
    set_color --background purple blue
    printf "$glyph "
    set_color normal
    printf "\n\n"
  end

  for glyph in $separator_right
    set_color --background purple blue
    printf " $glyph"
    set_color black --background blue
    printf " "
    set_color normal
    printf "\n\n"
  end

  for glyph in $divider_glyphs
    set_color black --background blue
    printf " $glyph "
    set_color normal
    printf "\n\n"
  end
end
