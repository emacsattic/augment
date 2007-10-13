$LOAD_PATH << File.dirname(__FILE__)
$LOAD_PATH << File.dirname(__FILE__) + '/backends'
$LOAD_PATH << File.dirname(__FILE__) + '/frontends'

require 'rubygems'
require 'json'

#require 'flet'
require 'backend'
require 'frontend'
require 'layer'

Dir.glob(File.dirname(__FILE__) + '/backends/*rb').each { |b| require b.gsub('.rb', '') }
Dir.glob(File.dirname(__FILE__) + '/frontends/*rb').each { |b| require b.gsub('.rb', '') }

class Augment
  VERSION = '1.0.0'

  def initialize(backend)
    @backend = { 'color' => ColoringBackend,
      'test' => TestUnitBackend
    }[backend]
  end

  def run(file)
    @backend.run(file)
  end

  class << self
    def augment_path(original)
      "#{File.dirname(File.expand_path(original))}/.augment/#{File.basename(original)}"
    end
  end
end
