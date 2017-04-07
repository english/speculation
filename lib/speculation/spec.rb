# frozen_string_literal: true

module Speculation
  # @private
  class Spec
    attr_accessor :name
    attr_reader :id

    def conform(_x)
      raise NotImplementedError
    end

    def explain(_path, _via, _inn, _value)
      raise NotImplementedError
    end

    def gen(_overrides, _path, _rmap)
      raise NotImplementedError
    end

    def inspect
      "#{self.class}(#{name})"
    end
  end
end

require_relative "spec/hash_spec"
require_relative "spec/predicate_spec"
require_relative "spec/tuple_spec"
require_relative "spec/or_spec"
require_relative "spec/and_spec"
require_relative "spec/merge_spec"
require_relative "spec/every_spec"
require_relative "spec/regex_spec"
require_relative "spec/f_spec"
require_relative "spec/nilable_spec"
require_relative "spec/nonconforming_spec"
