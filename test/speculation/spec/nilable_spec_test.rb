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
      expected = [{ :path => [:"Speculation/pred"],
                    :val  => 1,
                    :in   => [],
                    :via  => [ns(:maybe_string)],
                    :pred => [String, [1]] },
                  { :path => [:"Speculation/nil"],
                    :val  => 1,
                    :in   => [],
                    :via  => [ns(:maybe_string)],
                    :pred => [NilClass, [1]] }]

      assert_equal expected, ed.fetch(ns(Speculation, :problems))

      val = Gen.generate(S.gen(ns(:maybe_string)))
      assert val.is_a?(String) || val.nil?
    end
  end
end
