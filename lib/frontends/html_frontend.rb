class String
  def html_colorize(color)
    "<span style='color: #{color};'>#{self}</span>"
  end

  def html_colorize_range(range, color)
    "#{self[0 ... range.begin]}#{self[range].html_colorize(color)}#{self[range.end .. -1]}"
  end
end

class HtmlFrontend < Frontend
  class << self
    def run(file)
      puts "<html>
  <head><title>#{file} - Augment</title></head>
  <body>
#{super(file).gsub("\n", "<br />").gsub('  ', '&nbsp; ')}
  </body>
</html>"
    end
    
    def process_layer(text, layer)
      text.html_colorize_range(layer.range, layer.color)
    end
  end
  Augment::FRONTENDS['html'] = self
end
