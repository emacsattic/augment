Object.flet(:at_exit => lambda {}) do
  # keep miniunit's at_exit block from running
  gem 'miniunit', ">= 1.0.1"
  require 'test/unit'
end

module Test
  # Puke failures/errors in a bucket
  def Unit.puke(*args) 
    TestUnitBackend.failure_bucket(*args)
  end
end

##
# Backend for gathering test results. Instead of ntalbott's test/unit
# which ships with Ruby as of 1.8.6, this uses miniunit, a vastly
# simpler mostly-compatible replacement.
#
class TestUnitBackend < Backend
  class << self
    # Kicks off a miniunit run and captures the failures
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
      color = Test::Assertion === exception ? 'red' : 'yellow'
      line = exception.backtrace.grep(Regexp.new(@file))[0].match(/:(\d*):/)[1].to_i

      (@layers[@file] ||= []) << Layer.new(line, color, exception.message, self, @file)
    end

    # This should allow us to augment a test by running its associated
    # implementation
    def find_test_for(file)
      # TODO: return test for implementation if possible
      file
    end
  end
  
  Augment::BACKENDS['test'] = self
end
