# -*- ruby -*-

require 'rubygems'
require 'hoe'
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
  system("scp -r html/* technomancy@rubyforge.org:/var/www/gforge-projects/augment/")
end

# vim: syntax=Ruby
