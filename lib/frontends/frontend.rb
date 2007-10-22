class Frontend
  class << self
    def show(file)
      # TODO: deal with overlapping color ranges
      Layer.read(file).inject(File.read(file)) { |text, layer| process_layer(text, layer) }
    end
  end
end
