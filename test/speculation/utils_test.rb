# frozen_string_literal: true
require "test_helper"

class SpeculationUtilsTest < Minitest::Test
  using Speculation::NamespacedSymbols.refine(self)

  STest = Speculation::Test
  U = Speculation::Utils

  methods = STest.checkable_methods.select { |m| m.namespace == U }

  methods.each do |meth|
    define_method(:"test_check_#{meth.name}") do
      results = STest.check(meth, :num_tests => 100)
      result = STest.abbrev_result(results.first)

      assert_nil result[:failure], PP.pp(result, String.new)
    end
  end
end
