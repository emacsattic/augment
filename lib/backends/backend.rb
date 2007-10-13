require 'fileutils'

class Backend
  class << self
    def write_layers
      @layers.each do |file, layers|
        FileUtils.mkpath(File.dirname(file) + '/.augment')
        File.open(Augment.augment_path(file), 'w') do |f|
          f.puts "[#{layers.map{ |l| l.to_json }.join(", \n")}]"
        end
      end
    end
  end
end
