# frozen_string_literal: true
require "bundler/gem_tasks"

task :rubocop do
  require "rubocop"
  status = RuboCop::CLI.new.run([])
  raise "failed with status #{status}" unless status.zero?
end

task :test do
  if RUBY_PLATFORM == "java"
    $LOAD_PATH.unshift(File.expand_path("../test", __FILE__))

    FileList["test/**/*.rb"].each do |test_file|
      require "./#{test_file}"
    end

    raise unless Minitest.run
  else
    sh "TEST_QUEUE_SPLIT_GROUPS=1 bundle exec ruby -r minitest/autorun -I test -S minitest-queue $(find test -name *_test.rb)"
  end
end

task :default => [:rubocop, :test]
