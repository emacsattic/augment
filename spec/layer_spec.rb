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

  it "should get range from an ambiguous method name"
  it "should get range from a class method name"
end

describe Layer, " when working with JSON" do
  before do
    FileUtils.cd(PROJECT_ROOT + '/..')
    # have to create this on the fly since "augment clear"ing the spec dir will erase it. =(
    Dir.mkdir('.augment') rescue nil
    FileUtils.cp('layers.json', '.augment/layers.json')
    @layers = Layer.read('layers.json').reverse
  end
  
  it "should read from a file" do
    @layers.map{ |l| l.message }.should == ['cons', 'car', 'cdr']
    @layers.map{ |l| l.color }.should == ['red', 'green', 'blue']
    @layers.map{ |l| l.range }.should == [(1...12), (13...24), (25...36)]
    @layers.map{ |l| l.backend }.uniq.should == ['coloring']
  end
  
  it "should output valid JSON" do
    @layers.to_json.should == File.read('layers.json').gsub(/[ \n]/, '')
  end

  after do
    FileUtils.rm_rf('.augment')
  end
end

describe Layer, " when converting line range to char range" do
  it "should convert properly" do
    FileUtils.cd(PROJECT_ROOT)
    Layer.line_to_char_range('test/test_drink.rb', 10).should == (153 ... 176)
    Layer.line_to_char_range('test/test_drink.rb', 3).should == (22 ... 61)
    Layer.line_to_char_range('test/test_drink.rb', 17).should == (289 ... 332)
  end
end
