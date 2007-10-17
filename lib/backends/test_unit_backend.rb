Object.flet(:at_exit => lambda {}) do
  # keep miniunit's at_exit block from running
  gem 'miniunit', ">= 1.0.1"
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

    def record_failure(klass, method, exception)
      # FIXME: errors here could actually occur in impl rather than test file
      (@layers[@file] ||= []) << Layer.from_failure(@file, klass, method, exception)
    end
  end
end

def Layer.from_failure(file, klass, method, exception)
  color = Test::Assertion === exception ? 'red' : 'yellow'

  trace = exception.backtrace.detect { |e| e =~ /test_drink/ }
  line = trace.match(/:(\d*):/)[1]

  range = Layer.line_to_char_range(file, line.to_i)
  Layer.new(range, color, exception.message)
end

Augment::BACKENDS['test'] = TestUnitBackend
