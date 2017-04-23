# frozen_string_literal: true

# This is a Ruby translation of clojure.spec:
#   https://github.com/clojure/clojure/blob/master/src/clj/clojure/spec.clj
# All credit belongs with Rich Hickey and contributors for their original work.

module Speculation
  # @private
  class MergeSpec < Spec
    include NamespacedSymbols
    S = Speculation

    def initialize(preds, gen = nil)
      @preds = preds
      @gen   = gen
    end

    def conform(x)
      ms = @preds.map { |pred| S.dt(pred, x) }

      if ms.any?(&S.method(:invalid?))
        S::INVALID
      else
        ms.reduce(&:merge)
      end
    end

    def unform(x)
      @preds.reverse.map { |pred| S.unform(pred, x) }.reduce(&:merge)
    end

    def explain(path, via, inn, x)
      @preds.
        flat_map { |pred| S.explain1(pred, path, via, inn, x) }.
        compact
    end

    def with_gen(gen)
      self.class.new(@preds, gen)
    end

    def gen(overrides, path, rmap)
      return @gen if @gen

      gens = @preds.
        map { |pred| S.gensub(pred, overrides, path, rmap) }

      ->(r) do
        gens.map { |gen| gen.call(r) }.reduce(&:merge)
      end
    end
  end
end
