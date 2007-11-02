$LOAD_PATH << File.dirname(__FILE__) + '/../lib'
require 'augment'
require 'fileutils'

PROJECT_ROOT = File.expand_path(File.dirname(__FILE__) + '/fixtures/drinks/') unless defined? PROJECT_ROOT
