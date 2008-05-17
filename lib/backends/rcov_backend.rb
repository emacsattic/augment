
class RcovBackend < Backend
  def self.run(file)
    rcov = Marshal.load(File.read('coverage.info')).last[file]

    position = 0
    rcov[:lines].zip(rcov[:coverage]).each do |line, coverage|
      if coverage == false then
        
        self.layers[file] << Layer.new((position ... position + line.length),
                                       "red", "not covered", self)
        position += line.length
      end
    end

    write_layers
  end
  
  Augment::BACKENDS['rcov'] = self
end
