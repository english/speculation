# frozen_string_literal: true
module Speculation
  using NamespacedSymbols.refine(self)

  class AndSpec < SpecImpl
    S = Speculation

    def initialize(preds)
      @preds = preds
      @specs = Concurrent::Delay.new do
        preds.map { |pred| S.send(:specize, pred) }
      end
    end

    def conform(value)
      @specs.value.each do |spec|
        value = spec.conform(value)

        return :invalid.ns if S.invalid?(value)
      end

      value
    end

    def explain(path, via, inn, value)
      S.explain_pred_list(@preds, path, via, inn, value)
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
