class ColoringBackend < Backend
  COLORS = ['white', 'red', 'green', 'blue', 'brown']
  
  class << self
    attr_reader :layers
    
    def run(file)
      @layers = {}
      text = File.read(file)

      COLORS.each do |color|
        offset = 0
        while occurance = text.index(color, offset) do
          @layers[file] ||= []
          @layers[file] << Layer.new((occurance .. occurance + color.length),
                                     color, "Found a #{color}")
          offset += (occurance + 1)
        end
      end
      
      write_layers
    end
  end
end