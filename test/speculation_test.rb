require 'test_helper'
require 'speculation/core'

class SpeculationTest < Minitest::Test
  S = Speculation::Core

  def test_that_it_has_a_version_number
    refute_nil ::Speculation::VERSION
  end

  def test_conform_with_existing_spec
    S.def(:integer) { |x| x.is_a?(Integer) }

    assert_equal 2, S.conform(:integer, 2)
    assert_equal :"Speculation::Core/invalid", S.conform(:integer, "two")

    assert S.valid?(:integer, 2)
    refute S.valid?(:integer, "two")
  end

  def test_conform_with_predicate
    predicate = -> (x) { x.is_a?(Integer) }
    assert_equal 2, S.conform(predicate, 2)
    assert_equal :"Speculation::Core/invalid", S.conform(predicate, "two")

    assert S.valid?(predicate, 2)
    refute S.valid?(predicate, "two")
  end
end
