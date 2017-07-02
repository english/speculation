# frozen_string_literal: true

require "test_helper"

module Speculation
  class RegexSpecTest < Minitest::Test
    S = Speculation
    include S::NamespacedSymbols

    def test_cat_sequence
      S.def(ns(:boolean), ->(x) { [true, false].include?(x) })
      S.def(ns(:ingredient), S.cat(:quantity => Numeric, :unit => Symbol))

      expected = { :quantity => 2, :unit => :teaspoon }
      assert_equal expected, S.conform(ns(:ingredient), [2, :teaspoon])

      S.def(ns(:config), S.cat(:prop => String, :val => S.alt(:s => String, :b => ns(:boolean))))

      assert_equal({ :prop => "-server", :val => [:s, "foo"] }, S.conform(ns(:config), ["-server", "foo"]))
    end

    def test_nested_cat_sequence
      S.def(ns(:nested), S.cat(:names_sym => ->(x) { x == :names },
                               :names     => S.spec(S.cat(:name1 => String, :name2 => String)),
                               :nums_sym  => ->(x) { x == :nums },
                               :nums      => S.spec(S.cat(:num1 => Numeric, :num2 => Numeric))))

      expected = { :names_sym => :names,
                   :nums_sym  => :nums,
                   :nums      => { :num1 => 1, :num2 => 2 },
                   :names     => { :name1 => "a", :name2 => "b" } }

      assert_equal expected, S.conform(ns(:nested), [:names, ["a", "b"], :nums, [1, 2]])
    end

    def test_zero_or_more
      S.def(ns(:seq_of_symbols), S.zero_or_more(Symbol))

      assert_equal [:a, :b, :c], S.conform(ns(:seq_of_symbols), [:a, :b, :c])
      assert_equal [], S.conform(ns(:seq_of_symbols), [])
      assert_equal :"Speculation/invalid", S.conform(ns(:seq_of_symbols), [1, 2, 3])
    end

    def test_nested_seq
      S.def(ns(:nested), S.cat(:names_sym => Set[:names],
                               :names     => S.spec(S.zero_or_more(String)),
                               :nums_sym  => Set[:nums],
                               :nums      => S.spec(S.zero_or_more(Numeric))))

      conformed = S.conform(ns(:nested), [:names, ["a", "b"], :nums, [1, 2]])

      expected = { :names_sym => :names, :names => ["a", "b"],
                   :nums_sym => :nums, :nums => [1, 2] }

      assert_equal expected, conformed
    end

    def test_non_nested
      S.def(ns(:unnested), S.cat(:names_sym => ->(x) { x == :names },
                                 :names     => S.zero_or_more(String),
                                 :nums_sym  => ->(x) { x == :nums },
                                 :nums      => S.zero_or_more(Numeric)))

      expected = { :names_sym => :names, :names => ["a", "b"],
                   :nums_sym => :nums, :nums => [1, 2, 3] }

      assert_equal expected, S.conform(ns(:unnested), [:names, "a", "b", :nums, 1, 2, 3])
    end

    def test_one_or_more
      spec = S.one_or_more(Symbol)

      input = [:a, :b, :c]
      expected_conformed = [:a, :b, :c]

      assert_equal expected_conformed, S.conform(spec, [:a, :b, :c])
      assert_equal input, S.unform(spec, expected_conformed)

      assert_equal :"Speculation/invalid", S.conform(spec, [])
    end

    def test_zero_or_one
      S.def(ns(:odd), ->(x) { x.odd? })
      S.def(ns(:even), ->(x) { x.even? })

      S.def(ns(:maybe_odd), S.zero_or_one(ns(:odd)))

      assert_equal 1, S.conform(ns(:maybe_odd), [1])
      assert_nil S.conform(ns(:maybe_odd), [])
      assert_equal :"Speculation/invalid", S.conform(ns(:maybe_odd), [2])

      S.def(ns(:odds_then_maybe_even), S.cat(:odds => S.one_or_more(ns(:odd)),
                                             :even => S.zero_or_one(ns(:even))))

      input = [1, 3, 5, 100]
      expected_conformed = { :odds => [1, 3, 5], :even => 100 }

      assert_equal expected_conformed, S.conform(ns(:odds_then_maybe_even), input)
      assert_equal input, S.unform(ns(:odds_then_maybe_even), expected_conformed)
    end

    def test_alt_zero_or_more
      S.def(ns(:config),
            S.zero_or_more(S.cat(:prop => String,
                                 :val  => S.alt(:s => String,
                                                :b => ->(x) { [true, false].include?(x) }))))

      conformed = S.conform(ns(:config), ["-server", "foo", "-verbose", true, "-user", "joe"])
      expected = [{ :prop => "-server",  :val => [:s, "foo"] },
                  { :prop => "-verbose", :val => [:b, true] },
                  { :prop => "-user",    :val => [:s, "joe"] }]

      assert_equal expected, conformed
    end

    def test_constrained
      S.def(ns(:even_strings),
            S.constrained(S.zero_or_more(String), ->(x) { x.count.even? }))

      refute S.valid?(ns(:even_strings), ["a"])
      assert S.valid?(ns(:even_strings), ["a", "b"])
      refute S.valid?(ns(:even_strings), ["a", "b", "c"])
      assert S.valid?(ns(:even_strings), ["a", "b", "c", "d"])
    end

    def test_explain_regex
      S.def(ns(:ingredient), S.cat(:quantity => Numeric, :unit => Symbol))

      ed = S.explain_data(ns(:ingredient), [11, "peaches"])
      problems = ed.fetch(:problems)

      assert_equal 1, problems.count

      problem = problems.first
      assert_equal [:unit], problem[:path]
      assert_equal "peaches", problem[:val]
      assert_equal [ns(:ingredient)], problem[:via]
      assert_equal [1], problem[:in]
      assert_equal [Symbol, ["peaches"]], problem[:pred]

      even_count = ->(nums) { nums.count.even? }

      S.def(ns(:nested), S.cat(:names_sym => ->(x) { x == :names },
                               :names     => S.spec(S.zero_or_more(String)),
                               :nums_sym  => ->(x) { x == :nums },
                               :nums      => S.spec(S.constrained(S.one_or_more(Numeric), even_count))))

      ed = S.explain_data(ns(:nested), [:names, ["a", "b"], :nums, [1, 2, 3, 4, 5]])
      problems = ed.fetch(:problems)

      assert_equal 1, problems.count

      problem = problems.first
      assert_equal [:nums], problem[:path]
      assert_equal [1, 2, 3, 4, 5], problem[:val]
      assert_equal [3], problem[:in]
      assert_equal [ns(:nested)], problem[:via]
      assert_equal [even_count, [[1, 2, 3, 4, 5]]], problem[:pred]
    end

    def test_explain_alt
      S.def(ns(:nested), S.cat(:names_sym => ->(x) { x == :names },
                               :names     => S.spec(S.zero_or_more(String)),
                               :nums_sym  => ->(x) { x == :nums },
                               :nums      => S.spec(S.alt(:ints   => S.one_or_more(Integer),
                                                          :floats => S.one_or_more(Float)))))

      expected = {
        :problems => [
          {
            :path => [:nums, :ints],
            :val  => "1",
            :via  => [ns(:nested)],
            :in   => [3, 0],
            :pred => [Integer, ["1"]]
          },
          {
            :path => [:nums, :floats],
            :val  => "1",
            :via  => [ns(:nested)],
            :in   => [3, 0],
            :pred => [Float, ["1"]]
          }
        ],
        :spec     => ns(:nested),
        :value    => [:names, ["a", "b"], :nums, ["1"]]
      }

      assert_equal expected, S.explain_data(ns(:nested), [:names, ["a", "b"], :nums, ["1"]])
    end

    def test_unform
      S.def(ns(:config),
            S.zero_or_more(S.cat(:prop => String,
                                 :val  => S.alt(:s => String,
                                                :b => ->(x) { [true, false].include?(x) }))))

      input = ["-server", "foo", "-verbose", true, "-user", "joe"]
      conformed = S.conform(ns(:config), input)
      unformed = S.unform(ns(:config), conformed)

      expected = [{ :prop => "-server",  :val => [:s, "foo"] },
                  { :prop => "-verbose", :val => [:b, true] },
                  { :prop => "-user",    :val => [:s, "joe"] }]

      assert_equal expected, conformed
      assert_equal input, unformed

      assert_equal expected, conformed
    end
  end
end
