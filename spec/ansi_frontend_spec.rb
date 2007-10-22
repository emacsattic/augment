$LOAD_PATH << File.dirname(__FILE__)

require 'spec_helper'

describe Frontend, " when outputting ANSI color" do
  before do
    FileUtils.cd(PROJECT_ROOT)
    FileUtils.rm_r('lib/.augment') rescue nil
    FileUtils.rm_r('test/.augment') rescue nil

    ColoringBackend.run('lib/drink.rb')
    TestUnitBackend.run('test/test_drink.rb')
  end
  
  it "should color red as red" do
    output = `../../../bin/augment-out ansi #{PROJECT_ROOT}/lib/drink.rb`
    output.to_s.should include("#{'white'.colorize('white')}")
    output.to_s.should include("#{'red'.colorize('red')}")
  end

  it "should color the test_drink" do
    output = `../../../bin/augment-out ansi #{PROJECT_ROOT}/test/test_drink.rb`
    output.to_s.should match(/\e\[#{String::COLOR_LOOKUP['red']}m *assert/)
    output.to_s.should match(/\e\[#{String::COLOR_LOOKUP['yellow']}m *junk/)
  end
end
