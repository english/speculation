# frozen_string_literal: true

require_relative "experimental/object_spec"
require_relative "experimental/rspec_matcher_spec"

module Speculation
  module Experimental
    def self.object_spec(specs)
      ObjectSpec.new(specs)
    end
  end
end
