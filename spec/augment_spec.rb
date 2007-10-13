$LOAD_PATH << File.dirname(__FILE__) + '/../lib'
require 'augment'

PROJECT_ROOT = File.expand_path(File.dirname(__FILE__) + '/fixtures/drinks/')

describe Backend, " when augmenting by color" do
  before do
    FileUtils.cd(PROJECT_ROOT)
    FileUtils.rm_r('lib/.augment') rescue nil
    
    ColoringBackend.run('lib/drink.rb')
  end

  it "should create .augment directory and files" do
    File.should exist('lib/.augment')
    File.should exist('lib/.augment/drink.rb')
  end
  
  it "should color the colors and ranges" do
    # REFACTOR : should use Layer.read instead of JSON.parse
    layers = JSON.parse(File.read('lib/.augment/drink.rb'))
    layers.size.should == 4
    layers[0]['color'].should == 'white'
    layers[1]['color'].should == 'red'
    layers[2]['color'].should == 'green'
    layers[3]['color'].should == 'black'

    layers[0]['range'].should == (221 ... 226).to_s
    layers[1]['range'].should == (371 ... 374).to_s
    layers[2]['range'].should == (456 ... 461).to_s
    layers[3]['range'].should == (531 ... 536).to_s
  end
end

describe Backend, " when augmenting test results" do
  before do
    FileUtils.cd(PROJECT_ROOT)
    FileUtils.rm_r('test/.augment') rescue nil
    
    TestUnitBackend.run('test/test_drink.rb')
  end
  
  it "should color failing/erroring tests" do
    File.should exist(Augment.augment_path('test/test_drink.rb'))
    layers = Layer.read('test/test_drink.rb')
    layers.first['color'].should == 'red'
    layers.last['color'].should == 'yellow'

    layers.first['range'].should == (228 ... 338)
  end
  
  it "should include failure message"
end

describe Backend, " when augmenting flog results" do

  it "should color a complex method"
  it "should color a simple method"
end

describe Frontend, " when outputting ANSI color" do
  before do
    FileUtils.cd(PROJECT_ROOT)
    FileUtils.rm_r('lib/.augment') rescue nil
    FileUtils.rm_r('test/.augment') rescue nil

    ColoringBackend.run('lib/drink.rb')
    TestUnitBackend.run('test/test_drink.rb')
  end
  
  it "should color red as red" do
    output = `../../../bin/augment_color #{PROJECT_ROOT}/lib/drink.rb`
    output.to_s.should include("#{'white'.colorize('white')}")
    output.to_s.should include("#{'red'.colorize('red')}")
  end

  it "should color the test_drink" do
    output = `../../../bin/augment_color #{PROJECT_ROOT}/test/test_drink.rb`
    output.to_s.should match(/\e\[#{String::COLOR_LOOKUP['red']}m *def/)
    output.to_s.should match(/\e\[#{String::COLOR_LOOKUP['yellow']}m *def/)
  end
end
