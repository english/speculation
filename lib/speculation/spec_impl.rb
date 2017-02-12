# frozen_string_literal: true

module Speculation
  # @private
  class SpecImpl
    attr_accessor :name, :gen
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

require_relative "spec_impl/hash_spec"
require_relative "spec_impl/spec"
require_relative "spec_impl/tuple_spec"
require_relative "spec_impl/or_spec"
require_relative "spec_impl/and_spec"
require_relative "spec_impl/merge_spec"
require_relative "spec_impl/every_spec"
require_relative "spec_impl/regex_spec"
require_relative "spec_impl/f_spec"
require_relative "spec_impl/nilable_spec"
