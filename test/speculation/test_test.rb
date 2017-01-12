require 'test_helper'

class SpeculationTestTest < Minitest::Test
  S = Speculation
  STest = Speculation::Test
  Utils = Speculation::Utils
  H = Hamster::Hash
  V = Hamster::Vector

  def test_fdef_instrument
    mod = Module.new do
      def self.ranged_rand(start, eend)
        start + rand(eend - start)
      end
    end

    S.fdef(mod.method(:ranged_rand),
           args: S.and(S.cat(start: Integer, end: Integer),
                       -> (args) { args[:start] < args[:end] }))

    STest.instrument(mod.method(:ranged_rand))

    e = assert_raises(STest::DidNotConformError) { mod.ranged_rand(8, 5) }

    assert_match /^Call to '.*ranged_rand' did not conform to spec/, e.message

    assert_equal :instrument, e.explain_data.fetch(:"Speculation/failure")
    assert_match %r{speculation/test/speculation/test_test\.rb:\d+:in `block in test_fdef_instrument'}, e.explain_data.fetch(:"Speculation/caller")
    assert_equal V[H[:path => V[:args],
                     :val => H[:start => 8, :end => 5],
                     :in => V[],
                     :via => V[],
                     :pred => "<proc>"]], e.explain_data.fetch(:"Speculation/problems")

    assert_equal [8, 5], e.explain_data.fetch(:"Speculation/args")

    mod.ranged_rand(5, 8)
  end

  def test_instrument_instance_method
    klass = Class.new do
      def bar(str)
        "baz"
      end
    end

    S.fdef(klass.instance_method(:bar), args: S.cat(str: String))

    STest.instrument(klass.instance_method(:bar))

    subject = klass.new
    assert_raises(STest::DidNotConformError) { subject.bar(8) }
    subject.bar('asd')
  end

  def test_check
    mod = Module.new do
      def self.bad_ranged_rand(start, eend)
        start + rand(start..eend)
      end
    end

    S.fdef(mod.method(:bad_ranged_rand),
           args: S.and(S.cat(start: Integer, end: Integer),
                       -> (args) { args[:start] < args[:end] }),
           ret: Integer,
           fn: S.and(-> (x) { x[:ret] >= x[:args][:start] },
                     -> (x) { x[:ret] < x[:args][:end] }))

    results = STest.check(mod.method(:bad_ranged_rand))
    assert_equal 1, results.count

    result = results.first
    assert_equal [:"Speculation::Test::Check/ret", :failure, :method, :spec], result.keys.sort
    assert_equal mod.method(:bad_ranged_rand), result[:method]
  end
end
