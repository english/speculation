# frozen_string_literal: true

require "bundler/gem_tasks"
require "yard"

task :rubocop do
  # rubocop:disable Lint/HandleExceptions
  begin
    require "rubocop"
    status = RuboCop::CLI.new.run(["--display-cop-names"])
    raise "failed with status #{status}" unless status.zero?
  rescue LoadError
  end
  # rubocop:enable Lint/HandleExceptions
end

task :test do
  $LOAD_PATH.unshift(File.expand_path("../test", __FILE__))

  FileList["test/**/*.rb"].each do |test_file|
    require "./#{test_file}"
  end

  raise unless Minitest.run
end

task :doc do
  YARD::CLI::Yardoc.run("--no-private", "lib/**/*.rb", "-", "README.md", "LICENSE.txt")
end

task :default => [:rubocop, :test]
