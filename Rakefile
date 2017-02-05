# frozen_string_literal: true
require "bundler/gem_tasks"
require "rake/testtask"

task :rubocop do
  sh "bundle exec rubocop"
end

if RUBY_PLATFORM == "java"
  Rake::TestTask.new(:test) do |t|
    t.libs << "test"
    t.libs << "lib"
    t.test_files = FileList["test/**/*_test.rb"]
  end
else
  task :test do
    sh "./bin/t"
  end
end

task :default => [:rubocop, :test]
