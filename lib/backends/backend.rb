require 'fileutils'

##
# Base class from which to subclass other backends. Subclasses must
# implement the +run+ method that gets passed a file name and must (0)
# set the +@layers+ instance var to a hash of layers containing the
# metadata gathered and call the +write_layers+ method when finished
# to save that data.
#
class Backend
  def self.layers
    @layers ||= Hash.new { |h,k| h[k] = [] }
  end
  
  def self.run(file)
    raise "Base Backend class shouldn't be used for real augmentation."
  end

  # Output the +@layers+ hash as JSON where augment expects it.
  def self.write_layers
    self.layers.each do |file, layers|
      FileUtils.mkpath(File.dirname(file) + '/.augment')
      File.open(Augment.augment_path(file), 'w') do |f|
        f.puts "[#{layers.map{ |l| l.to_json }.join(", \n")}]"
      end
    end
  end

  # Suppress STDOUT while a block runs.
  def self.with_no_output
    old_stdout = $stdout.clone
    $stdout.reopen(File.new('/dev/null','w'))
    yield
    $stdout.reopen(old_stdout)
  end
end
