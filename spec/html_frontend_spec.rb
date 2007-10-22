$LOAD_PATH << File.dirname(__FILE__)

require 'spec_helper'

describe Frontend, " when outputting HTML" do
  before do
    FileUtils.cd(PROJECT_ROOT)
    FileUtils.rm_r('lib/.augment') rescue nil
    ColoringBackend.run('lib/drink.rb')
  end

  it "should create styled spans around colors" do
    output = `../../../bin/augment-out html #{PROJECT_ROOT}/lib/drink.rb`
    output.to_s.should include("<span style='color: white;'>white</span>")
  end
end

describe Layer, " when converting line range to char range" do
  it "should convert properly" do
    Layer.line_to_char_range('test/test_drink.rb', 10).should == (153 ... 176)
    Layer.line_to_char_range('test/test_drink.rb', 3).should == (22 ... 61)
    Layer.line_to_char_range('test/test_drink.rb', 17).should == (289 ... 332)
  end
end
