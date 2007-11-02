Object.flet(:at_exit => lambda {}) do
  # keep miniunit's at_exit block from running
  gem 'miniunit', ">= 1.0.1"
  require 'test/unit'
end

module Test
  def Unit.puke(*args) # puke in a bucket
    TestUnitBackend.failure_bucket(*args)
  end
end

class TestUnitBackend < Backend
  class << self
    def run(file)
      @file = file
      @layers = {}

      load(find_test_for(file))
      # this will call record_failure as needed
      with_no_output { Test::Unit.autotest }
      write_layers
    end

    def failure_bucket(klass, method, exception)
      # FIXME: errors here could actually occur in impl rather than test file
      (@layers[@file] ||= []) << Layer.from_failure(@file, klass, method, exception)
    end

    def find_test_for(file)
      # TODO: return test for implementation if possible
      file
    end
  end
  
  Augment::BACKENDS['test'] = self
end

# TODO: let Layer.new handle this by using a Fixnum instead of range
def Layer.from_failure(file, klass, method, exception)
  color = Test::Assertion === exception ? 'red' : 'yellow'

  trace = exception.backtrace.detect { |e| e =~ /test_drink/ }
  line = trace.match(/:(\d*):/)[1]

  range = Layer.line_to_char_range(file, line.to_i)
  Layer.new(range, color, exception.message, TestUnitBackend)
end
