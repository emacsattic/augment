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

    def with_no_output
      old_stdout = $stdout.clone
      $stdout.reopen(File.new('/dev/null','w'))
      yield
      $stdout.reopen(old_stdout)
    end
  end
end
