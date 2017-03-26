# frozen_string_literal: true

require "test_helper"

module Speculation
  class SpeculationTest < Minitest::Test
    S = Speculation
    include S::NamespacedSymbols

    def test_conform_with_existing_spec
      S.def(ns(:int?), ->(x) { x.is_a?(Integer) })

      assert_equal 2, S.conform(ns(:int?), 2)
      assert_equal :"Speculation/invalid", S.conform(ns(:int?), "two")

      assert S.valid?(ns(:int?), 2)
      refute S.valid?(ns(:int?), "two")
    end

    def test_def_requires_namespaced_symbol
      assert_raises(ArgumentError) do
        S.def("foo/integer", Integer)
      end

      assert_raises(ArgumentError) do
        S.def(:integer, Integer)
      end
    end

    def test_conform_with_predicate
      predicate = ->(x) { x.is_a?(Integer) }
      assert_equal 2, S.conform(predicate, 2)
      assert_equal :"Speculation/invalid", S.conform(predicate, "two")

      assert S.valid?(predicate, 2)
      refute S.valid?(predicate, "two")
    end

    def test_conformer
      S.def(ns(:wont_conform_keys), S.hash_of(S.and(Symbol, S.conformer(&:to_s)),
                                              S.and(Float, S.conformer(&:to_i))))

      assert_equal({ :foo => 1, :bar => 2 },
                   S.conform(ns(:wont_conform_keys), :foo => 1.0, :bar => 2.0))

      S.def(ns(:will_conform_keys), S.hash_of(S.and(Symbol, S.conformer(&:to_s)),
                                              S.and(Float, S.conformer(&:to_i)),
                                              :conform_keys => true))

      assert_equal({ "foo" => 1, "bar" => 2 },
                   S.conform(ns(:will_conform_keys), :foo => 1.0, :bar => 2.0))
    end

    def test_explain_data
      S.def(ns(:even), ->(x) { x.even? })

      ed = S.explain_data(ns(:even), 1)
      problems = ed.fetch(ns(S, :problems))

      assert_equal 1, problems.count

      problem = problems.first
      assert_equal [], problem[:path]
      assert_equal 1, problem[:val]
      assert_equal [ns(:even)], problem[:via]
      assert_equal [], problem[:in]
      assert_kind_of Proc, problem[:pred].first
      assert_equal [1], problem[:pred].last

      S.def(ns(:integer), Integer)
      S.def(ns(:even), ->(x) { x.even? })
      S.def(ns(:even_integer), S.and(ns(:integer), ns(:even)))

      ed = S.explain_data(ns(:even_integer), "s")
      problems = ed.fetch(ns(S, :problems))

      assert_equal 1, problems.count

      problem = problems.first
      assert_equal [], problem[:path]
      assert_equal "s", problem[:val]
      assert_equal [ns(:even_integer), ns(:integer)], problem[:via]
      assert_equal [], problem[:in]
      assert_equal [Integer, ["s"]], problem[:pred]
    end
  end
end
