# frozen_string_literal: true
$LOAD_PATH.unshift File.expand_path("../../lib", __FILE__)
require "minitest/autorun"
require "pry"
require "hamster"
require "set"
require "speculation"
require "speculation/test"
require "speculation/gen"
require "speculation/utils_specs"

Minitest.after_run do
  # this should be handled automatically, but isn't...
  Concurrent.global_fast_executor.shutdown
end
