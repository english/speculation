require 'test_helper'
require 'speculation/core'
require 'hamster/hash'

class SpeculationTest < Minitest::Test
  S = Speculation::Core
  H = Hamster::Hash
  V = Hamster::Vector

  def setup
    Speculation::Core.reset_registry!
  end

  def test_that_it_has_a_version_number
    refute_nil ::Speculation::VERSION
  end

  def test_conform_with_existing_spec
    S.def(:int?, -> (x) { x.is_a?(Integer) })

    assert_equal 2, S.conform(:int?, 2)
    assert_equal :"Speculation::Core/invalid", S.conform(:int?, "two")

    assert S.valid?(:int?, 2)
    refute S.valid?(:int?, "two")
  end

  def test_conform_with_predicate
    predicate = -> (x) { x.is_a?(Integer) }
    assert_equal 2, S.conform(predicate, 2)
    assert_equal :"Speculation::Core/invalid", S.conform(predicate, "two")

    assert S.valid?(predicate, 2)
    refute S.valid?(predicate, "two")
  end

  def test_and_composition
    S.def(:int?, -> (x) { x.is_a?(Integer) })
    S.def(:even?, -> (x) { x.even? })

    S.def(:big_even, S.and(:int?, :even?, -> (x) { x > 1000 }))

    assert_equal :"Speculation::Core/invalid", S.conform(:big_even, :foo)
    assert_equal :"Speculation::Core/invalid", S.conform(:big_even, 100)
    assert_equal 1_000_000, S.conform(:big_even, 1_000_000)

    refute S.valid?(:big_even, :foo)
    refute S.valid?(:big_even, 10)
    assert S.valid?(:big_even, 1_000_000)
  end

  def test_or_composition
    S.def(:int?, -> (x) { x.is_a?(Integer) })
    S.def(:string?, -> (x) { x.is_a?(String) })

    S.def(:name_or_id, S.or(name: :string?, id: :int?))

    assert_equal :"Speculation::Core/invalid", S.conform(:name_or_id, :foo)
    assert_equal [:name, "abc"], S.conform(:name_or_id, "abc")
    assert_equal [:id, 100], S.conform(:name_or_id, 100)
  end

  def test_cat_sequence
    S.def(:number?, -> (x) { x.is_a?(Numeric) })
    S.def(:symbol?, -> (x) { x.is_a?(Symbol) })
    S.def(:string?, -> (x) { x.is_a?(String) })
    S.def(:boolean?, -> (x) { [true, false].include?(x) })

    S.def(:ingredient, S.cat(quantity: :number?, unit: :symbol?))

    expected = H[quantity: 2, unit: :teaspoon]
    assert_equal(expected, S.conform(:ingredient, [2, :teaspoon]))

    S.def(:config, S.cat(prop: :string?, val: S.alt(s: :string?, b: :boolean?)))

    assert_equal(H[prop: "-server", val: [:s, "foo"]],
                 S.conform(:config, ["-server", "foo"]))
  end

  def test_nested_cat_sequence
    S.def(:number?, -> (x) { x.is_a?(Numeric) })
    S.def(:string?, -> (x) { x.is_a?(String) })
    S.def(:nested, S.cat(names_sym: -> (x) { x == :names },
                         names: S.spec(S.cat(name1: :string?, name2: :string?)),
                         nums_sym: -> (x) { x == :nums },
                         nums: S.spec(S.cat(num1: :number?, num2: :number?))))

    conformed = S.conform(:nested, [:names, ["a", "b"], :nums, [1, 2]])

    expected = H[names_sym: :names,
                 nums_sym: :nums,
                 nums: H[num1: 1, num2: 2],
                 names: H[name1: "a", name2: "b"]]

    assert_equal expected, conformed
  end

  def test_zero_or_more
    S.def(:symbol?, -> (x) { x.is_a?(Symbol) })
    S.def(:seq_of_symbols, S.zero_or_more(:symbol?))

    assert_equal [:a, :b, :c], S.conform(:seq_of_symbols, [:a, :b, :c])
    assert_equal [], S.conform(:seq_of_symbols, [])
    assert_equal :"Speculation::Core/invalid", S.conform(:seq_of_symbols, [1, 2, 3])
  end

  def test_nested_seq
    S.def(:number?, -> (x) { x.is_a?(Numeric) })
    S.def(:string?, -> (x) { x.is_a?(String) })
    S.def(:nested, S.cat(names_sym: -> (x) { x == :names },
                         names: S.spec(S.zero_or_more(:string?)),
                         nums_sym: -> (x) { x == :nums },
                         nums: S.spec(S.zero_or_more(:number?))))

    conformed = S.conform(:nested, [:names, ["a", "b"], :nums, [1, 2]])

    expected = H[names_sym: :names, names: V["a", "b"],
                 nums_sym: :nums, nums: V[1, 2]]

    assert_equal expected, conformed
  end
end
