# frozen_string_literal: true
require "test_helper"

class SpeculationGenTest < Minitest::Test
  S = Speculation
  Gen = S::Gen

  using S::NamespacedSymbols.refine(self)

  def test_generate
    assert_kind_of Integer, Gen.generate(S.gen(Integer))

    S.def(:string.ns, String)
    assert_kind_of String, Gen.generate(S.gen(:string.ns))

    S.def(:even.ns, S.and(Integer, ->(x) { x.even? }))
    val = Gen.generate(S.gen(:even.ns))
    assert_kind_of Integer, val
    assert val.even?

    S.def(:foo.ns, S.or(:neg_int => S.and(Integer, ->(x) { x.negative? }), :str => String))
    val = Gen.generate(S.gen(:foo.ns))
    assert((val.is_a?(Integer) && val.negative?) || val.is_a?(String))

    S.def(:x.ns, String)
    S.def(:y.ns, Integer)
    S.def(:z.ns, Integer)
    S.def(:hash.ns, S.keys(:req => [:x.ns, :y.ns], :opt => [:z.ns]))

    val = Gen.generate(S.gen(:hash.ns))

    assert val.keys.include?(:x.ns)
    assert val.keys.include?(:y.ns)
    assert val[:x.ns].is_a?(String)
    assert val[:y.ns].is_a?(Integer)

    S.def(:hash.ns, S.keys(:req_un => [:x.ns, :y.ns], :opt_un => [:z.ns]))

    val = Gen.generate(S.gen(:hash.ns))

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

    S.def(:email_type.ns, S.with_gen(spec, &gen))

    assert Gen.generate(S.gen(:email_type.ns)).is_a?(String)
  end

  def test_regex_gen
    S.def(:ingredient.ns, S.cat(:quantity => Numeric, :unit => Symbol))

    quantity, unit = Gen.generate(S.gen(:ingredient.ns))

    assert_kind_of Numeric, quantity
    assert_kind_of Symbol, unit

    S.def(:nested.ns, S.cat(:names_sym => Set[:names],
                            :names     => S.spec(S.zero_or_more(String)),
                            :nums_sym  => Set[:nums],
                            :nums      => S.spec(S.zero_or_more(Numeric))))

    names_sym, names, nums_sym, nums = Gen.generate(S.gen(:nested.ns))

    assert_kind_of Symbol, names_sym
    assert_kind_of Array, names
    assert_kind_of Symbol, nums_sym
    assert_kind_of Array, nums

    assert names.all? { |n| n.is_a?(String) }
    assert nums.all? { |n| n.is_a?(Numeric) }

    S.def(:non_nested.ns, S.cat(:names_sym => Set[:names],
                                :names     => S.zero_or_more(String),
                                :nums_sym  => Set[:nums],
                                :nums      => S.zero_or_more(Numeric)))

    non_nested = Gen.generate(S.gen(:non_nested.ns))
    syms, names_and_nums = non_nested.partition { |x| x.is_a?(Symbol) }
    assert_equal [:names, :nums], syms.sort
    assert names_and_nums.all? { |x| x.is_a?(String) || x.is_a?(Numeric) }

    S.def(:config.ns, S.cat(:prop => String, :val => S.alt(:s => String, :b => Set[true, false])))

    prop, val = Gen.generate(S.gen(:config.ns))

    assert_kind_of String, prop
    assert [String, TrueClass, FalseClass].include?(val.class)
  end

  def test_fdef_gen
    mod = Module.new do
      def self.ranged_rand(start, eend)
        start + rand(eend - start)
      end
    end

    S.def(:ranged_rand.ns,
          S.fdef(mod.method(:ranged_rand),
                 :args => S.and(S.cat(:start => Integer, :end => Integer),
                                ->(args) { args[:start] < args[:end] }),
                 :ret  => Integer))

    genned = Gen.generate(S.gen(:ranged_rand.ns))

    assert_kind_of Proc, genned
    assert_kind_of Integer, genned.call(1, 2)
    # fails :args spec
    assert_raises(RuntimeError) { genned.call(2, 1) }
  end
end
