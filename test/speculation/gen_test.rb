# frozen_string_literal: true

require "test_helper"

module Speculation
  class SpeculationGenTest < Minitest::Test
    S = Speculation
    Gen = S::Gen

    include Speculation::NamespacedSymbols

    def test_generate
      assert_kind_of Integer, Gen.generate(S.gen(Integer))

      S.def(ns(:string), String)
      assert_kind_of String, Gen.generate(S.gen(ns(:string)))

      S.def(ns(:even), S.and(Integer, ->(x) { x.even? }))
      val = Gen.generate(S.gen(ns(:even)))
      assert_kind_of Integer, val
      assert val.even?

      S.def(ns(:foo), S.or(:neg_int => S.and(Integer, ->(x) { x < 0 }), :str => String))
      val = Gen.generate(S.gen(ns(:foo)))
      assert((val.is_a?(Integer) && val < 0) || val.is_a?(String))

      S.def(ns(:x), String)
      S.def(ns(:y), Integer)
      S.def(ns(:z), Integer)
      S.def(ns(:hash), S.keys(:req => [ns(:x), ns(:y)], :opt => [ns(:z)]))

      val = Gen.generate(S.gen(ns(:hash)))

      assert val.keys.include?(ns(:x))
      assert val.keys.include?(ns(:y))
      assert val[ns(:x)].is_a?(String)
      assert val[ns(:y)].is_a?(Integer)

      S.def(ns(:hash), S.keys(:req_un => [ns(:x), ns(:y)], :opt_un => [ns(:z)]))

      val = Gen.generate(S.gen(ns(:hash)))

      assert val.keys.include?(:x)
      assert val.keys.include?(:y)
      assert val[:x].is_a?(String)
      assert val[:y].is_a?(Integer)
      assert val[:z].is_a?(Integer) if val.key?(:z)
    end

    def test_with_gen
      email_regex = /^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,63}$/
      spec = S.and(String, email_regex)
      gen = ->(rantly) do
        local_part = rantly.sized(rantly.range(1, 64)) { string(:alnum) }
        subdomain = rantly.sized(rantly.range(1, 10)) { string(:alnum) }
        tld = rantly.sized(3) { string(:alpha).downcase }

        "#{local_part}@#{subdomain}.#{tld}"
      end

      S.def(ns(:email_type), S.with_gen(spec, gen))

      assert Gen.generate(S.gen(ns(:email_type))).is_a?(String)
    end

    def test_regex_gen
      S.def(ns(:ingredient), S.cat(:quantity => Numeric, :unit => Symbol))

      quantity, unit = Gen.generate(S.gen(ns(:ingredient)))

      assert_kind_of Numeric, quantity
      assert_kind_of Symbol, unit

      S.def(ns(:nested), S.cat(:names_sym => Set[:names],
                               :names     => S.spec(S.zero_or_more(String)),
                               :nums_sym  => Set[:nums],
                               :nums      => S.spec(S.zero_or_more(Numeric))))

      names_sym, names, nums_sym, nums = Gen.generate(S.gen(ns(:nested)))

      assert_kind_of Symbol, names_sym
      assert_kind_of Array, names
      assert_kind_of Symbol, nums_sym
      assert_kind_of Array, nums

      assert(names.all? { |n| n.is_a?(String) })
      assert(nums.all? { |n| n.is_a?(Numeric) })

      S.def(ns(:non_nested), S.cat(:names_sym => Set[:names],
                                   :names     => S.zero_or_more(String),
                                   :nums_sym  => Set[:nums],
                                   :nums      => S.zero_or_more(Numeric)))

      non_nested = Gen.generate(S.gen(ns(:non_nested)))
      syms, names_and_nums = non_nested.partition { |x| x.is_a?(Symbol) }
      assert_equal [:names, :nums], syms.sort
      assert(names_and_nums.all? { |x| x.is_a?(String) || x.is_a?(Numeric) })

      S.def(ns(:config), S.cat(:prop => String, :val => S.alt(:s => String, :b => Set[true, false])))

      prop, val = Gen.generate(S.gen(ns(:config)))

      assert_kind_of String, prop
      assert [String, TrueClass, FalseClass].include?(val.class)
    end

    def test_fdef_gen
      S.def(ns(:ranged_rand),
            S.fspec(:args => S.and(S.cat(:start => Integer, :end => Integer),
                                   ->(args) { args[:start] < args[:end] }),
                    :ret  => Integer))

      genned = Gen.generate(S.gen(ns(:ranged_rand)))

      assert_kind_of Proc, genned
      assert_kind_of Integer, genned.call(1, 2)
      # fails :args spec
      assert_raises(RuntimeError) { genned.call(2, 1) }
    end

    def test_gen_overrides
      S.def(ns(:x), String)
      S.def(ns(:hash), S.keys(:req => [ns(:x)]))

      gen = S.gen(ns(:hash), ns(:x) => S.gen(Set.new("a".."z")))
      val = Gen.generate(gen)

      assert Set.new("a".."z").include?(val[ns(:x)])
    end
  end
end
