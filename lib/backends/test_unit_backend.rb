require 'miniunit'

module Test
  class Unit
    def puke(klass, meth, e)
      TestUnitBackend.record_failure(klass, meth, e)
    end
  end
end

class TestUnitBackend < Backend
  class << self
    def run(file)
      run_tests(file)
      write_layers
    end

    def run_tests(file)
      Test::Unit.autotest
      # FIXME: write this
      @layers = { 'test/test_drink.rb' => [Layer.new((61 ... 103), 'yellow', 'You had an error, dork!'),
                                           Layer.new((106 ... 225), 'green', 'good job, it passed'),
                                           Layer.new((228 ... 338), 'red', 'fail.')]}
    end

    def record_failure(klass, meth, e)
    end
  end
end
