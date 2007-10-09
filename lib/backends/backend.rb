class Backend
  class << self
    def write_layers
      FileUtils.mkpath('.augment')
      @layers.each do |file, layers|
        File.open(augment_file(file), 'w') do |f|
          f.puts "[#{layers.map{ |l| l.to_json }.join(", \n")}]"
        end
      end
    end

    def augment_file(file)
      '.augment/' + file.gsub(/[^a-zA-Z]/, '_')
    end
  end
end
