# frozen_string_literal: true
require "test_helper"

class SpeculationUtilsTest < Minitest::Test
  using Speculation::NamespacedSymbols.refine(self)

  STest = Speculation::Test
  U = Speculation::Utils

  methods = U.methods(false).map { |m| U.method(m) }

  methods.each do |meth|
    define_method(:"test_check_#{meth.name}") do
      result = STest.check(meth, :num_tests => 100)

      if result.first
        assert result.dig(0, :ret.ns(STest), :result), result
      end
    end
  end
end
