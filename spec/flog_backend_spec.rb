$LOAD_PATH << File.dirname(__FILE__)

require 'spec_helper'

describe FlogBackend, " when augmenting flog results" do
  before do
    FileUtils.cd(PROJECT_ROOT)
    FileUtils.rm_r('lib/.augment') rescue nil
    
    FlogBackend.run('lib/drink.rb')
    @layers = Layer.read('lib/drink.rb')
  end

  it "should create layers for all methods" do
    @layers.size.should == 2
  end
  
  it "should include flog levels in the messages" do
    @layers.first.message.should =~ /9.5/
    @layers.last.message.should =~ /5.0/
  end
  
  it "should color different levels of ugliness different colors"
end
