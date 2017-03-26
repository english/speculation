# frozen_string_literal: true

require "test_helper"

module Speculation
  class EverySpecTest < Minitest::Test
    S = Speculation
    Gen = S::Gen
    include S::NamespacedSymbols

    def test_coll_of
      S.def(ns(:symbol_collection), S.coll_of(Symbol))

      assert_equal [:a, :b, :c], S.conform(ns(:symbol_collection), [:a, :b, :c])
      assert_equal Set[5, 10, 2], S.conform(S.coll_of(Numeric), Set[5, 10, 2])

      expected = { :a => :x, :b => :y, :c => :z }
      assert_equal expected, S.conform(S.coll_of(ns(:symbol_collection)), :a => :x, :b => :y, :c => :z)

      assert S.valid?(S.coll_of(Integer), [1, 2, 3])
      assert S.valid?(S.coll_of(Integer, :kind => ->(coll) { coll.is_a?(Array) }), [1, 2, 3])
      refute S.valid?(S.coll_of(Integer), ["a", "b", "c"])

      assert S.valid?(S.coll_of(Integer, :count => 3), [1, 2, 3])
      refute S.valid?(S.coll_of(Integer, :count => 2), [1, 2, 3])

      refute S.valid?(S.coll_of(Integer, :min_count => 3, :max_count => 4), [1, 2])
      assert S.valid?(S.coll_of(Integer, :min_count => 3, :max_count => 4), [1, 2, 3])
      assert S.valid?(S.coll_of(Integer, :min_count => 3, :max_count => 4), [1, 2, 3, 4])
      refute S.valid?(S.coll_of(Integer, :min_count => 3, :max_count => 4), [1, 2, 3, 4, 5])

      assert_kind_of Set, S.conform(S.coll_of(Integer, :into => Set[]), [1, 2, 3, 4, 5])
      assert_kind_of Hash, S.conform(S.coll_of(S.coll_of(Integer), :into => {}), [[1, 2], [3, 4]])

      Gen.generate(S.gen(ns(:symbol_collection))).each do |x|
        assert_kind_of Symbol, x
      end

      coll = Gen.generate(S.gen(S.coll_of(Integer, :min_count => 3, :max_count => 4, :distinct => true, :into => Set[])))
      assert coll.count.between?(3, 4)
      assert Utils.distinct?(coll)
      coll.each do |x|
        assert_kind_of Integer, x
      end

      assert S.valid?(S.coll_of(Integer, :min_count => 3, :max_count => 4, :distinct => true, :kind => Set), Set[1, 2, 3])
      refute S.valid?(S.coll_of(Integer, :min_count => 3, :max_count => 4, :distinct => true, :kind => Array), Set[1, 2, 3])
      assert S.valid?(S.coll_of(Integer, :min_count => 3, :max_count => 4, :distinct => true, :kind => Enumerable), Set[1, 2, 3])

      assert_kind_of Array, Gen.generate(S.gen(S.coll_of(Integer, :min_count => 3, :max_count => 4, :distinct => true, :kind => Array)))
    end

    def test_tuple
      S.def(ns(:point), S.tuple(Integer, Integer, Integer))

      assert S.valid?(ns(:point), [1, 2, 3])
      refute S.valid?(ns(:point), [1, 2, "3"])

      expected = {
        :"Speculation/problems" => [
          { :path => [2], :val => 3.0, :via => [ns(:point)], :in => [2], :pred => [Integer, [3.0]] }
        ]
      }

      assert_equal expected, S.explain_data(ns(:point), [1, 2, 3.0])

      assert(Gen.generate(S.gen(ns(:point))).all? { |x| x.is_a?(Integer) })
    end

    def test_hash_of
      S.def(ns(:scores), S.hash_of(String, Integer))

      expected = { "Sally" => 1000, "Joe" => 500 }
      assert_equal expected, S.conform(ns(:scores), "Sally" => 1000, "Joe" => 500)

      refute S.valid?(ns(:scores), "Sally" => true, "Joe" => 500)

      hash = Gen.generate(S.gen(ns(:scores)))

      hash.keys.each do |key|
        assert_kind_of String, key
      end
      hash.values.each { |value| assert_kind_of Integer, value }
    end

    def test_explain_hash_of
      S.def(ns(:scores), S.hash_of(String, Integer))

      expected = { :"Speculation/problems" => [{ :path => [1],
                                                 :val  => "300",
                                                 :via  => [ns(:scores)],
                                                 :in   => ["Joe", 1],
                                                 :pred => [Integer, ["300"]] }] }

      assert_equal expected, S.explain_data(ns(:scores), "Sally" => 1000, "Joe" => "300")
    end
  end
end
