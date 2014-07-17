module Colorer
  COLORED_REGEXP = /\e\[(?:(?:[349]|10)[0-7]|[0-9]|[34]8;5;\d{1,3})?m/
  def uncolor(str)
    str.gsub(COLORED_REGEXP, '')
  end
  
  def black(str)
    "\e[30m#{str}\e[0m"
  end
  
  def on_red(str)
    "\e[41m#{str}\e[0m"
  end
  
  def on_black(str)
    "\e[40m#{str}\e[0m"
  end
end

include Colorer

if `git rev-parse --is-inside-work-tree 2>&1`.strip == "true"
  Pry.config.prompt_name = `basename \`git rev-parse --show-toplevel\``.strip
elsif Pry.config.prompt_name == 'pry'
  Pry.config.prompt_name = nil
end

def rails_sym
  defined?(Rails) ? '⌗ ' : ''
end

Pry.prompt = [
  proc { |target_self, nest_level, pry|
    on_black(" #{pry.input_array.size} ") + # line number on black
    black(on_red(" #{rails_sym + Pry.config.prompt_name + ' 〉' if Pry.config.prompt_name}")) + # optional prompt name
    black(on_red("#{Pry.view_clip(target_self)}#{":#{nest_level}" unless nest_level.zero?} ")) +
    " "
  },
  proc { |target_self, nest_level, pry|
    regular_prompt = " #{pry.input_array.size} " + # line number on black
                     " #{rails_sym + Pry.config.prompt_name + ' 〉' if Pry.config.prompt_name}" + # optional prompt name
                     "#{Pry.view_clip(target_self)}#{":#{nest_level}" unless nest_level.zero?} "
    prompt_length = uncolor(regular_prompt).length + regular_prompt.scan('〉').length - 1
    " " * (prompt_length) +
    on_red(" ") +
    " "
  }
]
