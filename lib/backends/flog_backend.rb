require 'flog'

##
# Gather complexity metrics about a piece of code via flog
#
class FlogBackend < Backend
  class << self
    def run(file)
      @file = file
      @layers = {}

      flogger = Flog.new
      flogger.flog_files @file
      flogger.totals.each { |method, score| record method, score }
      write_layers
    end

    def record(method, score)
      return if method =~ /\#none$/ # tossing this stuff
      color = 'red' # TODO: determine color smartly
      message = "#{method} flogs at #{score}"
      (@layers[@file] ||= []) << Layer.new(method, color, message, self, @file)
    end
  end

  Augment::BACKENDS['flog'] = self
end
