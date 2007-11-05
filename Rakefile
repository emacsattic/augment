# -*- ruby -*-

require 'rubygems'
require 'hoe'
require 'erb'
require 'spec/rake/spectask'
require './lib/augment.rb'

Hoe.new('augment', Augment::VERSION) do |p|
  p.rubyforge_name = 'augment'
  p.author = 'Phil Hagelberg'
  p.email = 'technomancy@gmail.com'
  p.summary = 'Augment is a system for collecting and displaying code metadata.'
  p.description = p.paragraphs_of('README.txt', 2..5).join("\n\n")
  p.url = p.paragraphs_of('README.txt', 0).first.split(/\n/)[1..-1]
  p.changes = p.paragraphs_of('History.txt', 0..1).join("\n\n")
  p.extra_deps << ['miniunit', '>= 1.0.1']
  p.extra_deps << ['json', '>= 1.1.1']
end

task :publish_html do
  system("scp html/* technomancy@rubyforge.org:/var/www/gforge-projects/augment/")
end

task :render_html do
  FileUtils.cd(File.dirname(__FILE__) + '/html/src/')
  TEMPLATE = File.read('html.erb')
  Dir.glob('*html').each do |filename|
    @title = ("Augment - " + filename.match(/(.*)\.html/)[1].gsub(/_/, ' ').capitalize).gsub(/ - Index/, '')
    @body = File.read(filename)
    
    html = ERB.new(TEMPLATE).result( binding )
    File.open("../#{filename}", 'w') { |f| f.puts html }
  end
end

Spec::Rake::SpecTask.new

# vim: syntax=Ruby
