$LOAD_PATH << File.dirname(__FILE__)
require 'spec_helper'

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
    layers = Layer.read('lib/drink.rb')
    layers.size.should == 4

    layers[0]['color'].should == 'black'
    layers[1]['color'].should == 'green'
    layers[2]['color'].should == 'red'
    layers[3]['color'].should == 'white'

    layers[0]['range'].should == (531 ... 536)
    layers[1]['range'].should == (456 ... 461)
    layers[2]['range'].should == (371 ... 374)
    layers[3]['range'].should == (221 ... 226)
    layers.map{ |l| l['backend'] }.uniq.should == ['coloring']
  end
end
