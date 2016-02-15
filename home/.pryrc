# coding: utf-8
module Colorer
  COLORED_REGEXP = /\e\[(?:(?:[349]|10)[0-7]|[0-9]|[34]8;5;\d{1,3})?m/
  def uncolor(str)
    str.gsub(COLORED_REGEXP, '')
  end

  def dim(str)
    "\e[2m#{str}\e[22m"
  end

  def black(str)
    "\e[30m#{str}\e[0m"
  end

  def white(str)
    "\e[37m#{str}\e[0m"
  end

  def red(str)
    "\e[31m#{str}\e[0m"
  end

  def green(str)
    "\e[32m#{str}\e[0m"
  end

  def cyan(str)
    "\e[36m#{str}\e[0m"
  end

  def num(num, str)
    "\e[38;5;#{num}m#{str}\e[0m"
  end

  def on_red(str)
    "\e[41m#{str}\e[0m"
  end

  def on_black(str)
    "\e[40m#{str}\e[0m"
  end

  def on_white(str)
    "\e[47m#{str}\e[0m"
  end

  def on_cyan(str)
    "\e[46m#{str}\e[0m"
  end

  def on_num(num, str)
    "\e[48;5;#{num}m#{str}\e[0m"
  end

  def italic(str)
    "\e[3m#{str}\e[0m"
  end
end

include Colorer

# Require a gem, even if we're inside Bundler and the gem is not. This is handy for colorizers and other
# little bits that make your life easier, but you don't want to require your team to use.
#
# Since there's often setup you do only if you load a gem in that situation, you can also pass a block to
# the require and it'll execute it only if the gem was succcessfully loaded.
#
def require_maybe_outside_bundler(gem)
  begin
    require gem
  rescue LoadError => e
    print italic("Looking for #{gem}... ")

    if defined? Bundler
      Bundler.with_clean_env do
        to_load = [gem]
        to_load += `gem dependency #{gem} --pipe`.lines.map{ |l| l.match(/([\w_-]+) --/){ |m| m[1]  } }.compact
        to_load.each do |g|
          which = `gem which #{g} 2>/dev/null`
          if which.empty?
            return puts red("Could not locate dependency #{g}.")
          end
          $LOAD_PATH << File.dirname(which)
        end
        begin
          require gem
          puts green("Success!")
        rescue LoadError
          return puts red("Failed to require after updating LOAD_PATH.")
        end
      end
    else
      return puts red('Nowhere else to look.')
    end
  end
  yield if block_given?
end

require_maybe_outside_bundler 'pbcopy'
require_maybe_outside_bundler 'pry-doc'

if `git rev-parse --is-inside-work-tree 2>&1`.strip == "true"
  Pry.config.prompt_name = `basename \`git rev-parse --show-toplevel\``.strip
elsif Pry.config.prompt_name == 'pry'
  Pry.config.prompt_name = nil
end

def rails_sym
  defined?(Rails) ? '# ' : ''
end

use_powerline = true
powerline_symbols = {
  right_arrow: "\uE0B0",
  right_separator: "\uE0B1",
  left_arrow: "\uE0B2",
  left_separator: "\uE0B3"
}

prompt_name_color = 88 # 124
prompt = proc { |target_self, nest_level, pry|
  tree = pry.binding_stack.map { |b| Pry.view_clip(b.eval("self")) }
  current = tree.pop
  tree.push('')
  tree = ["⋯ "] + tree.last(3) if tree.length > 4

  [
    on_white(black(" #{pry.input_array.size} ")),
    Pry.config.prompt_name ? [
      white(on_num(prompt_name_color, powerline_symbols[:right_arrow])),
      on_num(prompt_name_color, " "),
      on_num(prompt_name_color, white("#{rails_sym}#{Pry.config.prompt_name} ")),
      num(prompt_name_color, on_red(powerline_symbols[:right_arrow])),
    ] : [
      white(on_red(powerline_symbols[:right_arrow]))
    ],
    on_red(" "),
    on_red(num(88, tree.join(" #{powerline_symbols[:right_separator]} "))),
    on_red(white("#{Pry.view_clip(target_self)} ")),
    red(powerline_symbols[:right_arrow]),
    " "
  ].flatten.join
}

Pry.prompt = [
  prompt,
  proc { |target_self, nest_level, pry|
    prompt_str = uncolor(prompt.call(target_self, nest_level, pry))[0...-2]
    prompt_str.gsub!(powerline_symbols[:right_arrow], powerline_symbols[:right_separator])
    "#{dim(prompt_str)}#{red(powerline_symbols[:right_separator])} "
  }
]
