# frozen_string_literal: true
require "test_helper"

module Speculation
  class SpeculationUtilsTest < Minitest::Test
    STest = Speculation::Test

    def test_check_utils
      results = STest.check(STest.enumerate_methods(Speculation::Utils), :num_tests => 50)

      abbreved_results = results.map(&STest.method(:abbrev_result))
      failed_results = abbreved_results.select { |result| result[:failure] }

      assert failed_results.empty?, PP.pp(failed_results, String.new)
    end
  end
end
