$LOAD_PATH << File.dirname(__FILE__)

require 'spec_helper'

describe TestUnitBackend, " when augmenting test results" do
  before do
    FileUtils.cd(PROJECT_ROOT)
    FileUtils.rm_r('test/.augment') rescue nil
    
    TestUnitBackend.run('test/test_drink.rb')
    @layers = Layer.read('test/test_drink.rb')
  end
  
  it "should color failing/erroring tests" do
    @layers.first.color.should == 'red'
    @layers.last.color.should == 'yellow'
  end

  it "should set the range to the line of the error/failure" do
    @layers.first.range.should == (289 ... 332)
    @layers.last.range.should == (85 ... 97)
  end
  
  it "should include failure message" do
    @layers = Layer.read('test/test_drink.rb')
    @layers.first.message.should =~ /bad length/
    @layers.last.message.should =~ /undefined local variable or method/
  end

  it "should set the backend field" do
    @layers.map{ |l| l.backend }.uniq.should == ['testunit']
  end
end
