# frozen_string_literal: true
require "test_helper"

class SpeculationTestTest < Minitest::Test
  S = Speculation
  STest = Speculation::Test
  Utils = Speculation::Utils
  H = Hamster::Hash
  V = Hamster::Vector

  using S::NamespacedSymbols.refine(self)

  def test_fdef_instrument
    mod = Module.new do
      def self.ranged_rand(start, eend)
        start + rand(eend - start)
      end
    end

    S.fdef(mod.method(:ranged_rand),
           :args => S.and(S.cat(:start => Integer, :end => Integer),
                          ->(args) { args[:start] < args[:end] }))

    STest.instrument(mod.method(:ranged_rand))

    e = assert_raises(S::Error) { mod.ranged_rand(8, 5) }

    assert_match(/^Call to '.*ranged_rand' did not conform to spec/, e.data[:cause])

    assert_equal :instrument, e.data.fetch(:"Speculation/failure")
    assert_match %r{speculation/test/speculation/test_test\.rb:\d+:in `block in test_fdef_instrument'}, e.data.fetch(:"Speculation::Test/caller")

    problems = e.data.fetch(:"Speculation/problems")
    assert_equal 1, problems.count

    problem = problems.first
    assert_equal [:args], problem[:path]
    assert_equal Hash[:start => 8, :end => 5], problem[:val]
    assert_equal [], problem[:in]
    assert_equal [], problem[:via]
    assert_kind_of Proc, problem[:pred]

    assert_equal [8, 5], e.data.fetch(:"Speculation/args")

    mod.ranged_rand(5, 8) # doesn't blow up
  end

  def test_instrument_instance_method
    klass = Class.new do
      def bar(_str)
        "baz"
      end
    end

    S.fdef(klass.instance_method(:bar), :args => S.cat(:str => String))

    STest.instrument(klass.instance_method(:bar))

    subject = klass.new
    assert_raises(S::Error) do
      subject.bar(8)
    end
    subject.bar("asd")
  end

  def test_check
    mod = Module.new do
      def self.bad_ranged_rand(start, eend)
        start + rand(start..eend)
      end
    end

    S.fdef(mod.method(:bad_ranged_rand),
           :args => S.and(S.cat(:start => Integer, :end => Integer),
                          ->(args) { args[:start] < args[:end] }),
           :ret  => Integer,
           :fn   => S.and(->(x) { x[:ret] >= x[:args][:start] },
                          ->(x) { x[:ret] < x[:args][:end] }))

    results = STest.check(mod.method(:bad_ranged_rand))
    assert_equal 1, results.count

    result = results.first
    assert_equal [:"Speculation::Test/ret", :failure, :method, :spec], result.keys.sort
    assert_equal mod.method(:bad_ranged_rand), result[:method]
  end

  def test_instrument_spec_override
    mod = Module.new do
      def self.concat(a, b)
        a + b
      end
    end

    S.fdef(mod.method(:concat), :args => S.cat(:a => Integer, :b => Integer))

    alt_concat = S.fspec(:args => S.cat(:a => String, :b => String))

    STest.instrument(mod.method(:concat),
                     :spec => { mod.method(:concat) => alt_concat })

    assert_raises(S::Error) do mod.concat(1, 2) end
    mod.concat("a", "b") # doesn't blow up
  end

  def test_instrument_stub
    mod = Module.new do
      def self.concat(_a, _b)
        raise "I shouldn't have been called"
      end
    end

    S.fdef(mod.method(:concat),
           :args => S.cat(:a => Integer, :b => Integer),
           :ret  => Integer)

    STest.instrument(mod.method(:concat), :stub => [mod.method(:concat)])

    assert_kind_of Integer, mod.concat(1, 2)
  end

  def test_instrument_stub_gen_override
    mod = Module.new do
      def self.concat(_a, _b)
        raise "I shouldn't have been called"
      end
    end

    S.fdef(mod.method(:concat),
           :args => S.cat(:a => Integer, :b => Integer),
           :ret  => Integer)

    STest.instrument(mod.method(:concat),
                     :stub => [mod.method(:concat)],
                     :gen  => { mod.method(:concat) => ->(_r) { ->(_a, _b) { 1 } } })

    assert_equal 1, mod.concat(1, 2)
  end

  def test_instrument_replace
    mod = Module.new do
      def self.concat(_a, _b)
        raise "I shouldn't have been called"
      end
    end

    S.fdef(mod.method(:concat),
           :args => S.cat(:a => Integer, :b => Integer),
           :ret  => Integer)

    STest.instrument(mod.method(:concat),
                     :replace => { mod.method(:concat) => ->(_a, _b) { 1 } })

    assert_equal 1, mod.concat(1, 2)
  end
end
