# frozen_string_literal: true
require "test_helper"

module Speculation
  class AndSpecTest < Minitest::Test
    S = Speculation
    include S::NamespacedSymbols

    def test_and_composition
      S.def(ns(:even?), ->(x) { x.even? })

      S.def(ns(:big_even), S.and(Integer, ns(:even?), ->(x) { x > 1000 }))

      assert_equal :"Speculation/invalid", S.conform(ns(:big_even), :foo)
      assert_equal :"Speculation/invalid", S.conform(ns(:big_even), 100)
      assert_equal 1_000_000, S.conform(ns(:big_even), 1_000_000)

      refute S.valid?(ns(:big_even), :foo)
      refute S.valid?(ns(:big_even), 10)
      assert S.valid?(ns(:big_even), 1_000_000)
    end
  end
end
