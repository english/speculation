# frozen_string_literal: true

require "test_helper"

module Speculation
  class FSpecTest < Minitest::Test
    S = Speculation
    Gen = S::Gen
    include NamespacedSymbols

    def test_fspec
      mod = Module.new do
        def self.foo(x)
          x + 1
        end
      end

      S.fdef(mod.method(:foo),
             :args => S.cat(:x => Integer),
             :ret  => Integer)

      assert S.valid?(mod.method(:foo), :next.to_proc)
      refute S.valid?(mod.method(:foo), :to_s.to_proc)
      assert S.valid?(mod.method(:foo), mod.method(:foo))
      refute S.valid?(mod.method(:foo), "not-a-method")

      Gen.generate(S.gen(mod.method(:foo)))

      identifier = S.send(:MethodIdentifier, mod.method(:foo))
      expected = { :path => [:ret], :via => [identifier], :in => [], :pred => [Integer, ["0"]] }

      ed = S.explain_data(mod.method(:foo), :to_s.to_proc)
      assert_equal(expected, ed.fetch(ns(S, :problems)).first.reject { |k, _v| k == :val })
      assert_kind_of String, ed.fetch(ns(S, :problems)).first[:val]

      S.def(ns(:foo), S.fspec(:args => S.cat(:x => String), :ret => Integer))

      trigger_no_method_error = :trigger_no_method_error.to_proc
      ed = S.explain_data(ns(:foo), trigger_no_method_error).fetch(ns(S, :problems)).first
      assert_equal [], ed[:path]
      assert_equal [trigger_no_method_error, [""]], ed[:pred]
      assert_equal [ns(:foo)], ed[:via]
      assert_equal [], ed[:in]
      assert_match(/undefined method `trigger_no_method_error' for .*String/, ed[:reason])
    end

    def test_fspec_block
      mod = Module.new do
        def self.foo(&block)
          block.call(1)
        end
      end

      S.fdef(mod.method(:foo),
             :args  => ns(S, :empty),
             :block => S.fspec(:args => S.cat(:x => Integer),
                               :ret  => Integer),
             :ret   => Integer)

      assert S.valid?(mod.method(:foo), mod.method(:foo))
      refute S.valid?(mod.method(:foo), :to_s.to_proc)
      refute S.valid?(mod.method(:foo), "not-a-method")

      val = Gen.generate(S.gen(mod.method(:foo)))
      assert_kind_of(Integer, val.call { |_x| 1 })

      identifier = S.MethodIdentifier(mod.method(:foo))
      f = ->(&b) { b.call("1") }
      ed = S.explain_data(mod.method(:foo), f)
      ed = ed.fetch(ns(S, :problems)).first

      assert_equal [], ed[:path]
      func, args, block = ed[:pred]

      assert_equal f, func
      assert_equal [], args
      assert_kind_of Proc, block

      assert_equal [], ed[:val][:args]
      assert_kind_of Proc, ed[:val][:block]
      assert_equal 'In: [0] val: "1" fails at: [:x] predicate: [Integer, ["1"]]', ed[:reason]
      assert_equal [identifier], ed[:via]
      assert_equal [], ed[:in]
    end

    def test_exercise_fn
      mod = Module.new do
        def self.foo(x)
          yield(x + 1)
        end
      end

      S.fdef(mod.method(:foo),
             :args  => S.cat(:x => Integer),
             :block => S.fspec(:args => S.cat(:x => Integer), :ret => Integer),
             :ret   => Integer)

      exercised = S.exercise_fn(mod.method(:foo), 1)
      assert_equal 1, exercised.count

      args, block, ret = exercised.first

      assert_equal 1, args.count
      assert_kind_of Integer, args.first
      assert_kind_of Proc, block
      assert_kind_of Integer, ret
    end
  end
end
