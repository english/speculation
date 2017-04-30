# frozen_string_literal: true

require "test_helper"

module Speculation
  class PredicateSpecTest < Minitest::Test
    S = Speculation
    Gen = S::Gen
    include S::NamespacedSymbols

    def test_class_predicate
      S.def(ns(:seq_of_symbols), S.zero_or_more(Symbol))
      assert_equal [:foo, :bar], S.conform(ns(:seq_of_symbols), [:foo, :bar])

      S.def(ns(:seq_of_symbols), S.zero_or_more(Symbol))
      assert_equal [:foo, :bar], S.conform(ns(:seq_of_symbols), [:foo, :bar])
    end

    def test_set_predicate
      S.def(ns(:suit), Set[:club, :diamond, :heart, :spade])

      assert S.valid?(ns(:suit), :club)
      assert S.valid?(ns(:suit), :heart)
      refute S.valid?(ns(:suit), :lung)

      ed = S.explain_data(ns(:suit), 1)
      expected = [{ :path => [],
                    :val  => 1,
                    :via  => [:"Speculation::PredicateSpecTest/suit"],
                    :in   => [],
                    :pred => [Set[:club, :diamond, :heart, :spade], [1]] }]

      assert_equal expected, ed.fetch(:problems)

      val = Gen.generate(S.gen(ns(:suit)))
      assert [:club, :diamond, :heart, :spade].include?(val)
    end
  end
end
