# frozen_string_literal: true

# This is a Ruby translation of clojure.spec:
#   https://github.com/clojure/clojure/blob/master/src/clj/clojure/spec.clj
# All credit belongs with Rich Hickey and contributors for their original work.

module Speculation
  # @private
  class AndSpec < Spec
    include NamespacedSymbols
    S = Speculation

    def initialize(preds, gen = nil)
      @preds = preds
      @gen   = gen
      @specs = Concurrent::Delay.new do
        preds.map { |pred| S.send(:specize, pred) }
      end
    end

    def conform(value)
      @specs.value!.each do |spec|
        value = spec.conform(value)

        return S::INVALID if S.invalid?(value)
      end

      value
    end

    def unform(value)
      @preds.reverse.reduce(value) { |val, pred| S.unform(pred, val) }
    end

    def explain(path, via, inn, value)
      S.explain_pred_list(@preds, path, via, inn, value)
    end

    def with_gen(gen)
      self.class.new(@preds, gen)
    end

    def gen(overrides, path, rmap)
      if @gen
        @gen
      else
        S.gensub(@preds.first, overrides, path, rmap)
      end
    end
  end
end
