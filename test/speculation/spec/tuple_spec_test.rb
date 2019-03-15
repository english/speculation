# frozen_string_literal: true

require "test_helper"

module Speculation
  class TupleSpecTest < Minitest::Test
    S = Speculation
    include S::NamespacedSymbols

    def test_explain
      refute S.valid?(S.tuple(:"Speculation/any"), [])

      explain_data = S.explain_data(S.tuple(:"Speculation/any"), [])
      assert_equal [S::Predicates.method(:count_eq?), [[:"Speculation/any"], 0]], explain_data.fetch(:problems).first.fetch(:pred)
    end
  end
end
