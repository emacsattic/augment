$LOAD_PATH << File.dirname(__FILE__)
$LOAD_PATH << File.dirname(__FILE__) + '/backends'
$LOAD_PATH << File.dirname(__FILE__) + '/frontends'

require 'rubygems'
require 'json'

require 'backend'
require 'layer'

Dir.glob(File.dirname(__FILE__) + '/backends/*rb').each { |b| require b.gsub('.rb', '') }

class Augment
  VERSION = '1.0.0'
  
end
