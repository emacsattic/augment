class Drink
  attr_accessor :name, :proof, :color

  def initialize args
    @name = args[:name]
    @proof = args[:proof]
    @color = args[:color]
  end

  def self.load_drinks
    @vodka = Drink.new :name => 'Vodka', :proof => 80, :color => 'white'
    @kahlua = Drink.new :name => 'Kahlua', :proof => 40, :color => 'brown'
    @tomatoJuice = Drink.new :name => 'Tomato Juice', :proof => 0, :color => 'red'
    @mikesHardLime = Drink.new :name => 'Mike\'s Hard Lime', :proof => 8, :color => 'green'
    @jager = Drink.new :name => 'Jagermeister', :proof => 40, :color => 'black'
  end
end
