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
  
  def initialize(action, file)
    raise "No backend or frontend with that name." if action.nil?
    action.run(file)
  end

  class << self
    def interactive(backend_names = 'test')
      loop do
        begin
          filename = STDIN.gets.chomp
          backend_names.each { |backend| BACKENDS[backend].run(filename) }
          puts "{\"#{filename}\" : #{File.read(augment_path(filename))} }"
        rescue
          puts "Error augmenting #{filename}."
        end
      end
    end
    
    def augment_path(original)
      "#{File.dirname(File.expand_path(original))}/.augment/#{File.basename(original)}"
    end
  end
end

Dir.glob(File.dirname(__FILE__) + '/*ends/*rb').each { |b| require b[0 .. -4] }
