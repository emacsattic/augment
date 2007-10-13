class String
  COLOR_LOOKUP = {'black' => 30,
      'red' => 31,
      'green' => 32,
      'yellow' => 33,
      'blue' => 34,
      'magenta' => 35,
      'cyan' => 36,
      'white' => 37,
    'default' => 38 }
  
  def colorize(color_code)
    "\e[#{String.lookup_color_code(color_code)}m#{self}\e[0m"
  end

  def colorize_range(range, color)
    "#{self[0 ... range.begin]}#{self[range].colorize(color)}#{self[range.end .. -1]}"
  end
  
  def self.lookup_color_code(color, foreground = true)
    return color if color.is_a? Fixnum
    COLOR_LOOKUP[color.downcase]
  end
end

class AnsiColorFrontend < Frontend
  class << self
    def show(file)
      text = File.read(file)
      layers = Layer.read(file)

      # TODO: deal with overlapping color ranges
      layers.each do |layer|
        text = text.colorize_range(layer['range'], layer['color'])
      end

      puts text
    end
  end
end
