##
# Horribly disregard any OOP best practices by temporarily redefining
# methods for the duration of a block. Stolen from Common Lisp.
#
# DON'T USE THIS! (unless your *really* mean it and there's no other way)
#
def Object.flet(bindings, &block)
  old_methods = {}

  bindings.each do |the_method, body|
    old_methods[the_method] = method(the_method)
    define_method(the_method, body)
  end
  
  begin
    block.call
  ensure
    bindings.each do |the_method, body|
      define_method(the_method) { |*args| old_methods[the_method].call(*args) }
    end
  end
end

# Demo:
if $0 == __FILE__
  puts "foo" # should output "foo"
  
  Object.flet(:puts => lambda { |str| print "#{str.reverse}\n" }) do
    puts "foo" # should output "oof"
  end

  puts "foo" # should output "foo"
end
