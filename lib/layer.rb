require 'json'

class Layer
  def initialize(range, color, message, backend)
    range = (range.split('...').first.to_i ... range.split('...').last.to_i) if range.is_a? String
    @attrs = { 'range' => range, 'color' => color, 'message' => message,
      'backend' => backend.to_s.downcase.gsub(/backend/, '')}
  end

  def self.read(original_file)
    JSON.parse(File.read(Augment.augment_path(original_file))).map{ |l| Layer.new(l['range'], l['color'], l['message'], l['backend']) }.sort_by{ |l| l['range'].begin }.reverse
  end

  def self.line_to_char_range(file, line)
    file = File.read(file).split("\n")
    start = file[0 ... line - 1].join("\n").size + 2
    (start ... start + file[line - 1].size)
  end

  def to_json
    @attrs.to_json
  end

  def [](attr)
    @attrs[attr]
  end
end
