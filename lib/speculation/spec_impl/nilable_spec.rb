# frozen_string_literal: true
module Speculation
  using Conj

  # @private
  class NilableSpec < SpecImpl
    include NamespacedSymbols
    S = Speculation

    def initialize(pred)
      @pred = pred
      @delayed_spec = Concurrent::Delay.new { S.send(:specize, pred) }
    end

    def conform(value)
      value.nil? ? value : @delayed_spec.value!.conform(value)
    end

    def explain(path, via, inn, value)
      return if S.pvalid?(@delayed_spec.value!, value) || value.nil?

      S.
        explain1(@pred, path.conj(ns(S, :pred)), via, inn, value).
        conj(:path => path.conj(ns(S, :nil)), :pred => [NilClass, [value]], :val => value, :via => via, :in => inn)
    end

    def gen(overrides, path, rmap)
      return @gen if @gen

      ->(rantly) do
        rantly.freq([1, Gen.delay { Utils.constantly(nil) }],
                    [9, Gen.delay { S.gensub(@pred, overrides, path.conj(ns(S, :pred)), rmap) }])
      end
    end
  end
end
