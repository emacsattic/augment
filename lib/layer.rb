require 'json'

##
# Stores metadata about a file. Fields stored are:
#
# * range - the character range the layer applies to
# * color - the color with which to highlight the piece of code
# * message - what to display to the user
# * backend - which backend generated this layer
#
# Layer data is stored and emitted in JSON format.
#
# Note that ranges are always exclusive, so always use three dots!
#
class Layer
  attr_accessor :range, :color, :message, :backend

  def initialize(where, color, message, backend, file = nil)
    @range, @color, @message, @backend = [Layer.interpret_where(where, file),
                                          color, message,
                                          backend.to_s.downcase.gsub(/backend/, '')]
  end

  ##
  # Determines the correct range as defined by a few rules:
  #
  # * Numbers get interpreted as line numbers
  # * Strings get converted into Ranges if they include '...'
  # * Otherwise Strings are interpreted as Class#method names
  # * Ranges pass through untouched
  #
  def Layer.interpret_where where, file = nil
    case where
    when Fixnum
      Layer.line_to_char_range file, where
    when String
      if where =~ /\.\.\./ # grabbing this from JSON strings
        Layer.range_string_to_char_range where
      else
        Layer.method_to_char_range file, where
      end
    else
      where
    end
  end

  # Convert a line number to a character range given a file.
  def self.line_to_char_range(file, line)
    file = File.read(file).split("\n")
    start = file[0 ... line - 1].join("\n").size + 2
    (start ... start + file[line - 1].size)
  end

  # Basically implements the inverse of Range#to_s
  def self.range_string_to_char_range(range)
    (range.split('...').first.to_i ... range.split('...').last.to_i)
  end

  # Convert a method name to a character range given a file.
  def self.method_to_char_range(file, method)
    # TODO: get smart about what class the method is defined on... ugh
    start = File.read(file) =~ Regexp.new("def .*#{method.split(/#/).last}")
    finish = start + 5 # TODO: fix
    (start ... finish) # we're just layering the word "def" for now
  end

  # Slurp up layer data from stored JSON given the original file.
  def self.read(original_file)
    return [] if !File.exist?(Augment.augment_path(original_file))
    layers = JSON.parse(File.read(Augment.augment_path(original_file)))
    layers.map!{ |l| Layer.new(l['range'], l['color'], l['message'], l['backend']) }
    layers.sort_by{ |l| l.range.begin }.reverse
  end

  def to_json
    { 'range' => @range, 'color' => @color, 'message' => @message,
      'backend' => @backend}.to_json
  end
end
