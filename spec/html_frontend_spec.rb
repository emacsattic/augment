$LOAD_PATH << File.dirname(__FILE__)

require 'spec_helper'

describe Frontend, " when outputting HTML" do
  before do
    FileUtils.cd(PROJECT_ROOT)
    FileUtils.rm_r('lib/.augment') rescue nil
    ColoringBackend.run('lib/drink.rb')
  end

  it "should create styled spans around colors" do
    output = `../../../bin/augment html #{PROJECT_ROOT}/lib/drink.rb`
    output.to_s.should include("<span style='color: white;'>white</span>")
  end
end
