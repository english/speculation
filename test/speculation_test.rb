# frozen_string_literal: true

require "test_helper"

module Speculation
  class SpeculationTest < Minitest::Test
    S = Speculation
    include S::NamespacedSymbols

    def test_def_requires_namespaced_symbol
      assert_raises(ArgumentError) do
        S.def("foo/integer", Integer)
      end

      assert_raises(ArgumentError) do
        S.def(:integer, Integer)
      end
    end

    def test_def_with_nil_removes_spec
      S.def(:"foo/bar", String)
      assert S.registry.key?(:"foo/bar")

      S.def(:"foo/bar", nil)
      refute S.registry.key?(:"foo/bar")
    end

    def test_conform_with_existing_spec
      S.def(ns(:int?), ->(x) { x.is_a?(Integer) })

      assert_equal 2, S.conform(ns(:int?), 2)
      assert_equal :"Speculation/invalid", S.conform(ns(:int?), "two")

      assert S.valid?(ns(:int?), 2)
      refute S.valid?(ns(:int?), "two")
    end

    def test_conform_with_predicate
      predicate = ->(x) { x.is_a?(Integer) }
      assert_equal 2, S.conform(predicate, 2)
      assert_equal :"Speculation/invalid", S.conform(predicate, "two")

      assert S.valid?(predicate, 2)
      refute S.valid?(predicate, "two")
    end

    def test_conformer
      S.def(ns(:wont_conform_keys), S.hash_of(S.and(Symbol, S.conformer(method(:String))),
                                              S.and(Float, S.conformer(method(:Integer)))))

      assert_equal Hash[:foo => 1, :bar => 2],
                   S.conform(ns(:wont_conform_keys), :foo => 1.0, :bar => 2.0)

      S.def(ns(:will_conform_keys), S.hash_of(S.and(Symbol, S.conformer(method(:String))),
                                              S.and(Float, S.conformer(method(:Integer))),
                                              :conform_keys => true))

      assert_equal Hash["foo" => 1, "bar" => 2],
                   S.conform(ns(:will_conform_keys), :foo => 1.0, :bar => 2.0)
    end

    def test_nonconforming
      name_or_id = S.or(:name => String, :id => Integer)
      assert_equal [:name, "foo"], S.conform(name_or_id, "foo")
      assert_equal "foo", S.conform(S.nonconforming(name_or_id), "foo")
    end

    def test_explain_data
      S.def(ns(:integer), Integer)
      S.def(ns(:even), ->(x) { x.even? })
      S.def(ns(:even_integer), S.and(ns(:integer), ns(:even)))

      ed = S.explain_data(ns(:even_integer), "s")
      problems = ed.fetch(:problems)

      assert_equal 1, problems.count

      assert_equal Hash[:path => [],
                        :val  => "s",
                        :via  => [ns(:even_integer), ns(:integer)],
                        :in   => [],
                        :pred => [Integer, ["s"]]], problems.first
    end

    def test_conform_unform
      spec = S.and(String,
                   S.conformer(method(:Integer), method(:String)),
                   Integer,
                   :even?.to_proc)

      assert_equal 1_000_000, S.conform(spec, "1_000_000")
      assert_equal "1000000", S.unform(spec, S.conform(spec, "1_000_000"))
    end
  end
end
