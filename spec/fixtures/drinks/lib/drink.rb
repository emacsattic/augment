class Drink
  attr_accessor :name, :proof, :color

  def initialize args
    @name = args[:name]
    @proof = args[:proof]
    @color = args[:color]
  end
end

Vodka = Drink.new :name => 'Vodka', :proof => 80, :color => 'white'
Kahlua = Drink.new :name => 'Kahlua', :proof => 40, :color => 'brown'
TomatoJuice = Drink.new :name => 'Tomato Juice', :proof => 0, :color => 'red'
MikesHardLime = Drink.new :name => 'Mike\'s Hard Lime', :proof => 8, :color => 'green'
Jager = Drink.new :name => 'Jagermeister', :proof => 40, :color => 'black'
