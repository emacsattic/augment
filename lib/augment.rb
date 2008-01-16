$LOAD_PATH << File.dirname(__FILE__)
$LOAD_PATH << File.dirname(__FILE__) + '/backends'
$LOAD_PATH << File.dirname(__FILE__) + '/frontends'

require 'rubygems'

require 'layer'
require 'flet'
require 'backend'
require 'frontend'

class Augment
  VERSION = '1.0.1'
  BACKENDS = {}
  FRONTENDS = {}
  
  def initialize(action, file)
    raise "No backend or frontend with that name." if action.nil?
    action.run(file)
  end

  class << self
    ##
    # Interactive mode allows you to repeatedly give augment a filename and have
    # it spit back layer JSON immediately. By default only uses the test backend.
    #
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

    # Where should the JSON layer files be given a file?
    def augment_path(original)
      "#{File.dirname(File.expand_path(original))}/.augment/#{File.basename(original)}"
    end
  end
end
