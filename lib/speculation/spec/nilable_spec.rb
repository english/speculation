# frozen_string_literal: true

module Speculation
  # @private
  class NilableSpec < Spec
    include NamespacedSymbols
    S = Speculation

    def initialize(pred, gen = nil)
      @pred = pred
      @gen = gen
      @delayed_spec = Concurrent::Delay.new { S.send(:specize, pred) }
    end

    def conform(value)
      value.nil? ? value : @delayed_spec.value!.conform(value)
    end

    def explain(path, via, inn, value)
      return if S.pvalid?(@delayed_spec.value!, value) || value.nil?

      Utils.conj(
        S.explain1(@pred, Utils.conj(path, ns(S, :pred)), via, inn, value),
        :path => Utils.conj(path, ns(S, :nil)), :pred => [NilClass, [value]], :val => value, :via => via, :in => inn
      )
    end

    def with_gen(gen)
      self.class.new(@pred, gen)
    end

    def gen(overrides, path, rmap)
      return @gen if @gen

      ->(rantly) do
        rantly.freq([1, Gen.delay { Utils.constantly(nil) }],
                    [9, Gen.delay { S.gensub(@pred, overrides, Utils.conj(path, ns(S, :pred)), rmap) }])
      end
    end
  end
end
