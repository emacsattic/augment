def at_exit; end # keep miniunit's at_exit block from running
gem 'miniunit'
require 'test/unit'

# ideally we could do this with flet, but alias_method is uncooperative
# flet(:at_exit => {}) do
#   # keep miniunit's at_exit block from running
#   gem 'miniunit'
#   require 'test/unit'
# end

module Test
  def Unit.puke(*args)
    TestUnitBackend.record_failure(*args)
  end

  def Unit.puts(*args); end
end

class TestUnitBackend < Backend
  class << self
    def run(file)
      @file = file
      @layers = {}
      
      load(file)
      Test::Unit.autotest # this will call record_failure
      write_layers
    end

    def record_failure(*args)
      # FIXME: errors here could actually occur in impl rather than test file
      (@layers[@file] ||= []) << Layer.from_failure(*args)
    end
  end
end

def Layer.from_failure(klass, method, exception)
  color = Test::Assertion === exception ? 'red' : 'yellow'
  range = color == 'red' ? (228 ... 338) : (61 ... 103) # FIXME
  Layer.new(range, color, exception.message)
end
