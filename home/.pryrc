module Colorer
  COLORED_REGEXP = /\e\[(?:(?:[349]|10)[0-7]|[0-9]|[34]8;5;\d{1,3})?m/
  def uncolor(str)
    str.gsub(COLORED_REGEXP, '')
  end
  
  def black(str)
    "\e[30m#{str}\e[0m"
  end
  
  def red(str)
    "\e[31m#{str}\e[0m"
  end
  
  def green(str)
    "\e[32m#{str}\e[0m"
  end
  
  def on_red(str)
    "\e[41m#{str}\e[0m"
  end
  
  def on_black(str)
    "\e[40m#{str}\e[0m"
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
    print "Looking for #{gem}... "

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

if `git rev-parse --is-inside-work-tree 2>&1`.strip == "true"
  Pry.config.prompt_name = `basename \`git rev-parse --show-toplevel\``.strip
elsif Pry.config.prompt_name == 'pry'
  Pry.config.prompt_name = nil
end

def rails_sym
  # defined?(Rails) ? '⌗ ' : ''
  defined?(Rails) ? '# ' : ''
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
