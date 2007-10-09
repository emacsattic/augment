$LOAD_PATH << File.dirname(__FILE__) + '/../lib'
require 'augment'

PROJECT_ROOT = File.expand_path(File.dirname(__FILE__) + '/fixtures/drinks/')

describe Backend, " when augmenting by color" do
  before do
    FileUtils.cd(PROJECT_ROOT)
    FileUtils.rm_r('.augment') rescue nil
    
    ColoringBackend.run('lib/drink.rb')
  end

  it "should create .augment directory and files" do
    File.should exist('lib/.augment')
    File.should exist('lib/.augment/drink.rb')
  end
  
  it "should color the colors and ranges" do
    output = JSON.parse(File.read('lib/.augment/drink.rb'))
    output.size.should == 4
    output[0]['color'].should == 'red'
    output[1]['color'].should == 'green'
    output[2]['color'].should == 'brown'
    output[3]['color'].should == 'brown'

    output[0]['range'].should == (371 .. 374).to_s
    output[1]['range'].should == (456 .. 461).to_s
    output[2]['range'].should == (291 .. 296).to_s
    output[3]['range'].should == (528 .. 533).to_s
  end
end

describe Backend, " when augmenting test results" do

  it "should color passing tests green"
  it "should color failing tests red"
  it "should color errors orange"
  it "should include failure message"
  it "should highlight specific line"
end

describe Backend, " when augmenting flog results" do

  it "should color a complex method"
  it "should color a simple method"
end
