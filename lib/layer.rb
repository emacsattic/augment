class Layer
  def initialize(range, color, message)
    range = (range.split('...').first.to_i ... range.split('...').last.to_i) if range.is_a? String
    @attrs = { 'range' => range, 'color' => color, 'message' => message}
  end

  def self.read(original_file)
    JSON.parse(File.read(Augment.augment_path(original_file))).map{ |l| Layer.new(l['range'], l['color'], l['message']) }.sort_by{ |l| l['range'].begin }.reverse
  end

  def to_json
    @attrs.to_json
  end

  def [](attr)
    @attrs[attr]
  end
end
