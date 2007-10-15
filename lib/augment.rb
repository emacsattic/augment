$LOAD_PATH << File.dirname(__FILE__)
$LOAD_PATH << File.dirname(__FILE__) + '/backends'
$LOAD_PATH << File.dirname(__FILE__) + '/frontends'

require 'rubygems'
require 'json'
require 'layer'
require 'flet'

class Augment
  VERSION = '1.0.0'
  BACKENDS = {}
  
  def initialize(backend_name, file)
    BACKENDS[backend_name].run(file)
  end

  class << self
    def augment_path(original)
      "#{File.dirname(File.expand_path(original))}/.augment/#{File.basename(original)}"
    end
  end
end

require 'backend'
require 'frontend'

Dir.glob(File.dirname(__FILE__) + '/*ends/*rb').each { |b| require b[0 .. -4] }
