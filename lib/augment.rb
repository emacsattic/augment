$LOAD_PATH << File.dirname(__FILE__)
$LOAD_PATH << File.dirname(__FILE__) + '/backends'
$LOAD_PATH << File.dirname(__FILE__) + '/frontends'

require 'rubygems'

require 'layer'
require 'flet'
require 'backend'
require 'frontend'

class Augment
  VERSION = '1.0.0'
  BACKENDS = {}
  FRONTENDS = {}
  
  def initialize(backend_name, file)
    BACKENDS[backend_name].run(file)
  end

  class << self
    def output(frontend_name, file)
      FRONTENDS[frontend_name].show(file)
    end
    
    def augment_path(original)
      "#{File.dirname(File.expand_path(original))}/.augment/#{File.basename(original)}"
    end
  end
end

Dir.glob(File.dirname(__FILE__) + '/*ends/*rb').each { |b| require b[0 .. -4] }
