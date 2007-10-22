class Frontend
  class << self
    def show(file)
      text = File.read(file)
      layers = Layer.read(file)

      # TODO: deal with overlapping color ranges
      puts layers.inject(text) { |layer_text, layer| process_layer(layer_text, layer) }
    end
  end
end
