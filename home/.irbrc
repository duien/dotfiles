require 'rubygems'
require 'pp'
begin
  require 'wirble'
  Wirble.init
  Wirble.colorize
rescue LoadError
end


IRB.conf[:AUTO_INDENT] = true

class Object
  def my_methods
    (methods - Object.instance_methods).sort
  end
end

def gen_password(num_chars, strength=[:lower, :upper, :digit])
  chars = []
  chars << ('a'..'z').to_a if strength.member? :lower
  chars << ('A'..'Z').to_a if strength.member? :upper
  chars << ('0'..'9').to_a if strength.member? :digit
  chars << %w{! @ # $ ^ & *} if strength.member? :symbol
  chars.flatten!
  passwd = ''
  num_chars.times { passwd << chars[rand(chars.length)] }
  passwd
end

class Symbol
  def <=> other
    self.to_s <=> other.to_s
  end
end
