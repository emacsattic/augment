$LOAD_PATH << File.dirname(__FILE__)
require 'spec_helper'

describe Layer, " when creating ranges" do
  before do
    FileUtils.cd(PROJECT_ROOT)
    @filename = "test/test_drink.rb"
  end

  it "should get range from a line number" do
    Layer.interpret_where(5, @filename).should == (85 ... 97)
  end
  
  it "should get range from a range string" do
    Layer.interpret_where("22 ... 33").should == (22 ... 33)
  end
  
  it "should get range from a method name" do
    Layer.interpret_where("#test_might_error", @filename).should == (63 ... 68)
    Layer.interpret_where("#test_will_fail", @filename).should == (230 ... 235)
  end
end

describe Layer, " when working with JSON" do
  it "should read from a file"
  it "should output valid JSON"
end
