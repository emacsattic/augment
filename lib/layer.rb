class Layer
  def initialize(range, color, message)
    @attrs = { :range => range, :color => color, :message => message}
  end

  def self.read(original_file)
    JSON.parse(File.read(Augment.augment_file(original_file)))
  end

  def to_json
    @attrs.to_json
  end
end
