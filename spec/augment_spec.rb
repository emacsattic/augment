$LOAD_PATH << File.dirname(__FILE__) + '/../lib'
require 'augment'
require 'coloring_augmentor'
require 'spec_helper'

describe AugmentBackend, " when augmenting by color" do
  before do
    FileUtils.cd(File.dirname(__FILE__) + '/fixtures/drinks')
    FileUtils.rm_r('.augment') rescue nil
    
    ColoringAugmentor.run('lib/drink.rb')
    ColoringAugmentor.run('lib/white_russian.rb')
  end

  it "should create .augment directory" do
    File.should exist('.augment')
  end
  
  it "should create .augment files for each code file"
  it "should color red things red"
end

describe AugmentBackend, " when augmenting test results" do

  it "should color passing tests green"
  it "should color failing tests red"
  it "should color errors orange"
  it "should include failure message"
  it "should highlight specific line"
end

describe AugmentBackend, " when augmenting flog results" do

  it "should color a complex method"
  it "should color a simple method"
end
