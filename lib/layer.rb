class Layer
  def initialize(range, color, message)
    @attrs = { :range => range, :color => color, :message => message}
  end

  def to_json
    @attrs.to_json
  end
end
