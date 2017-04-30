# frozen_string_literal: true

require "test_helper"

module Speculation
  class SpeculationTestTest < Minitest::Test
    S = Speculation
    STest = Speculation::Test
    Utils = Speculation::Utils

    include Speculation::NamespacedSymbols

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

      assert_match(/^Call to '.*ranged_rand' did not conform to spec/, e.message)

      assert_equal :instrument, e.data.fetch(:failure)
      assert_match %r{test/speculation/test_test\.rb:\d+:in `block in test_fdef_instrument'}, e.data.fetch(:caller)

      problems = e.data.fetch(:problems)
      assert_equal 1, problems.count

      problem = problems.first
      assert_equal [:args], problem[:path]
      assert_equal Hash[:start => 8, :end => 5], problem[:val]
      assert_equal [], problem[:in]
      assert_equal [], problem[:via]
      assert_kind_of Array, problem[:pred]
      assert_kind_of Proc, problem[:pred].first

      assert_equal [8, 5], e.data.fetch(:args)

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

      results = STest.check(mod.method(:bad_ranged_rand), :num_tests => 50)
      assert_equal 1, results.count

      result = results.first
      assert_equal [:failure, :method, :ret, :spec], result.keys.sort
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

    def test_instrument_stub_and_check
      mod = Module.new do
        def self.invoke_service(_service, _request)
          raise "shouldn't get called"
        end

        def self.run_query(service, query)
          response = invoke_service(service, :"foo/query" => query)
          result, error = response.values_at(:"foo/result", :"foo/error")
          result || error
        end
      end

      S.def(:"foo/query", String)
      S.def(:"foo/request", S.keys(:req => [:"foo/query"]))
      S.def(:"foo/result", S.coll_of(String, :gen_max => 3))
      S.def(:"foo/error", Integer)
      S.def(:"foo/response", S.or(:ok  => S.keys(:req => [:"foo/result"]),
                                  :err => S.keys(:req => [:"foo/error"])))

      S.fdef(mod.method(:invoke_service),
             :args => S.cat(:service => :"Speculation/any", :request => :"foo/request"),
             :ret  => :"foo/response")

      S.fdef(mod.method(:run_query),
             :args => S.cat(:service => :"Speculation/any", :query => String),
             :ret  => S.or(:ok => :"foo/result", :err => :"foo/error"))

      # verify they satisfy spec now instrumented
      STest.instrument(mod.method(:invoke_service), :stub => [mod.method(:invoke_service)])
      mod.invoke_service(nil, :"foo/query" => "test")

      results = STest.check(mod.method(:run_query), :num_tests => 50)
      assert_nil results.first[:failure]
    end

    def test_fdef_block_instrument
      mod = Module.new do
        def self.ranged_rand(start, eend, &block)
          start + block.call(eend - start)
        end
      end

      S.fdef(mod.method(:ranged_rand),
             :args  => S.and(S.cat(:start => Integer, :end => Integer),
                             ->(args) { args[:start] < args[:end] }),
             :block => S.fspec(:args => S.cat(:max => Integer),
                               :ret  => ns(S, :positive_integer),
                               :fn   => ->(x) { x[:ret] < x[:args][:max].abs }))

      STest.instrument(mod.method(:ranged_rand))

      e = assert_raises(S::Error) { mod.ranged_rand(8, 5) }

      assert_match(/^Call to '.*ranged_rand' did not conform to spec/, e.message)

      assert_equal :instrument, e.data.fetch(:failure)
      assert_match %r{test/speculation/test_test\.rb:\d+:in `block in test_fdef_block_instrument'}, e.data.fetch(:caller)

      problems = e.data.fetch(:problems)
      assert_equal 1, problems.count

      problem = problems.first
      assert_equal [:args], problem[:path]
      assert_equal Hash[:start => 8, :end => 5], problem[:val]
      assert_equal [], problem[:in]
      assert_equal [], problem[:via]
      assert_kind_of Proc, problem[:pred].first
      assert_equal Hash[:start => 8, :end => 5], problem[:val], problem[:pred].last

      assert_equal [8, 5], e.data.fetch(:args)

      mod.ranged_rand(5, 8, &method(:rand)) # doesn't blow up
    end
  end
end
