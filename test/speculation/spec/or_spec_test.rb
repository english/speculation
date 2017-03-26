# frozen_string_literal: true

require "test_helper"

module Speculation
  class OrSpecTest < Minitest::Test
    S = Speculation
    include S::NamespacedSymbols

    def test_or_composition
      S.def(ns(:name_or_id), S.or(:name => String, :id => Integer))

      assert_equal :"Speculation/invalid", S.conform(ns(:name_or_id), :foo)
      assert_equal [:name, "abc"], S.conform(ns(:name_or_id), "abc")
      assert_equal [:id, 100], S.conform(ns(:name_or_id), 100)
    end

    def test_explain_or
      S.def(ns(:name_or_id), S.or(:name => String, :id => Integer))

      expected = {
        :"Speculation/problems" => [
          { :path => [:name], :val => :foo, :in => [], :via => [ns(:name_or_id)], :pred => [String, [:foo]] },
          { :path => [:id], :val => :foo, :in => [], :via => [ns(:name_or_id)], :pred => [Integer, [:foo]] }
        ]
      }

      assert_equal expected, S.explain_data(ns(:name_or_id), :foo)
      assert_nil S.explain_data(ns(:name_or_id), 1)
    end
  end
end
