# frozen_string_literal: true
require "bundler/gem_tasks"
require "rake/testtask"

task :rubocop do
  sh "bundle exec rubocop"
end

task :test do
  sh "./bin/t"
end

task :default => [:rubocop, :test]
