require 'test_helper'
require 'speculation/core'

class SpeculationTest < Minitest::Test
  S = Speculation::Core

  def test_that_it_has_a_version_number
    refute_nil ::Speculation::VERSION
  end

  def test_conform_with_simple_predicate
    S.def(:integer) { |x| x.is_a?(Integer) }

    assert_equal 2, S.conform(:integer, 2)
    assert_equal :"Speculation::Core/invalid", S.conform(:integer, "two")
  end
end
