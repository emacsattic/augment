Object.flet(:at_exit => lambda {}) do
  # keep miniunit's at_exit block from running
  gem 'miniunit'
  require 'test/unit'
end

module Test
  def Unit.puke(*args) # pass on failure info
    TestUnitBackend.record_failure(*args)
  end

  def Unit.puts(*args); end # muffle test output
end

class TestUnitBackend < Backend
  class << self
    def run(file)
      @file = file
      @layers = {}
      
      load(file)
      Test::Unit.autotest # this will call record_failure as needed
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

Augment::BACKENDS['test'] = TestUnitBackend
