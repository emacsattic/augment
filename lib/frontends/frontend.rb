class Frontend
  class << self
    def run(file)
      # TODO: deal with overlapping color ranges
      Layer.read(file).inject(File.read(file)) { |text, layer| process_layer(text, layer) }
    end
  end
end
