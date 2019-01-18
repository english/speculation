# frozen_string_literal: true

# This is a Ruby translation of clojure.spec:
#   https://github.com/clojure/clojure/blob/master/src/clj/clojure/spec.clj
# All credit belongs with Rich Hickey and contributors for their original work.

module Speculation
  # @private
  class NilableSpec < Spec
    include NamespacedSymbols
    S = Speculation

    def initialize(pred, gen = nil, name = nil)
      @pred = pred
      @gen = gen
      @name = name
      @delayed_spec = Concurrent::Delay.new { S.send(:specize, pred) }
    end

    def conform(value)
      value.nil? ? value : @delayed_spec.value!.conform(value)
    end

    def unform(value)
      value.nil? ? nil : @delayed_spec.value!.unform(value)
    end

    def explain(path, via, inn, value)
      return if S.pvalid?(@delayed_spec.value!, value) || value.nil?

      Utils.conj(
        S.explain1(@pred, Utils.conj(path, :pred), via, inn, value),
        :path => Utils.conj(path, :nil), :pred => [NilClass, [value]], :val => value, :via => via, :in => inn
      )
    end

    def with_gen(gen)
      self.class.new(@pred, gen, @name)
    end

    def with_name(name)
      self.class.new(@pred, @gen, name)
    end

    def gen(overrides, path, rmap)
      return @gen.call if @gen

      nil_gen = Gen.delay { Radagen.return(nil) }
      not_nil_gen = Gen.delay { S.gensub(@pred, overrides, Utils.conj(path, :pred), rmap) }
      Radagen.frequency(nil_gen => 1, not_nil_gen => 9)
    end
  end
end
