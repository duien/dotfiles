Pry.prompt = [
  proc { |target_self, nest_level, pry|
    "\001\e[0;40m\002 #{pry.input_array.size}" + # line number on black
    " \001\e[0;30;41m\002⮀ " + # black to red triange
    "#{Pry.config.prompt_name + '  ' if Pry.config.prompt_name}" + # optional prompt name
    "#{Pry.view_clip(target_self)}#{":#{nest_level}" unless nest_level.zero?} " +
    "\001\e[0;31m\002⮀ \001\e[0m\002" # close red and reset color
  },
  proc { |target_self, nest_level, pry|
    " #{pry.input_array.size}  #{Pry.config.prompt_name + '  ' if Pry.config.prompt_name }#{Pry.view_clip(target_self)}#{":#{nest_level}" unless nest_level.zero?} ".gsub(/./, ' ') +
    "\001\e[0;31m\002 " + # a red triange
    "\001\e[0m\002" # reset color
  }
]
