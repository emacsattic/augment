def Object.flet(bindings, &block)
  old_methods = {}

  bindings.each do |the_method, body|
    old_methods[the_method] = method(the_method)
    define_method(the_method, body)
  end
  
  block.call
  
  bindings.each do |the_method, body|
    define_method(the_method) { |*args| old_methods[the_method].call(*args) }
  end
end

if $0 == __FILE__
  def foo
    puts "foo"
  end
  
  foo # should output "foo"
  
  Object.flet(:foo => lambda { puts "bar" }) do
    foo # should output "bar"
  end

  foo # should output "foo"
end
