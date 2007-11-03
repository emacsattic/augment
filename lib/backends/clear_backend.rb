require 'find'
require 'fileutils'

class ClearBackend < Backend
  class << self
    def run(file)
      Find.find(file) { |f| FileUtils.rm_rf(f) if f =~ /\.augment$/ }
    end
  end
  
  Augment::BACKENDS['clear'] = self
end
