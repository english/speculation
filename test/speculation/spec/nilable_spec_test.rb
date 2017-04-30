# frozen_string_literal: true

require "test_helper"

module Speculation
  class NilableSpecTest < Minitest::Test
    S = Speculation
    include S::NamespacedSymbols

    def test_nilable
      S.def(ns(:maybe_string), S.nilable(String))

      assert S.valid?(ns(:maybe_string), "foo")
      assert S.valid?(ns(:maybe_string), nil)
      refute S.valid?(ns(:maybe_string), 1)

      ed = S.explain_data(ns(:maybe_string), 1)
      expected = [{ :path => [:pred],
                    :val  => 1,
                    :in   => [],
                    :via  => [ns(:maybe_string)],
                    :pred => [String, [1]] },
                  { :path => [:nil],
                    :val  => 1,
                    :in   => [],
                    :via  => [ns(:maybe_string)],
                    :pred => [NilClass, [1]] }]

      assert_equal expected, ed.fetch(:problems)

      val = Gen.generate(S.gen(ns(:maybe_string)))
      assert val.is_a?(String) || val.nil?
    end

    def test_conform_unform
      spec = S.nilable(Integer)

      assert_equal 5, S.conform(spec, 5)
      assert_equal 5, S.unform(spec, 5)

      assert_nil S.conform(spec, nil)
      assert_nil S.unform(spec, nil)

      spec = S.nilable(S.or(:i => Integer, :s => String))

      assert_equal [:i, 5], S.conform(spec, 5)
      assert_equal 5, S.unform(spec, [:i, 5])

      assert_equal [:s, "x"], S.conform(spec, "x")
      assert_equal "x", S.unform(spec, [:s, "x"])

      assert_nil S.conform(spec, nil)
      assert_nil S.unform(spec, nil)
    end
  end
end
