class String
  def html_colorize(color)
    "<span style='color: #{color};'>#{self}</span>"
  end

  def html_colorize_range(range, color)
    "#{self[0 ... range.begin]}#{self[range].html_colorize(color)}#{self[range.end .. -1]}"
  end
end

class HtmlFrontend < Frontend
end

Augment::FRONTENDS['html'] = HtmlFrontend
