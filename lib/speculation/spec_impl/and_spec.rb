module Speculation
  using NamespacedSymbols.refine(self)

  class AndSpec < SpecImpl
    S = Speculation

    def initialize(preds, gen = nil)
      @preds = preds
      @gen = gen
      @specs = Concurrent::Delay.new do
        preds.map { |pred| S.specize(pred) }
      end
    end

    def conform(value)
      @specs.value.each do |spec|
        value = spec.conform(value)

        return :invalid.ns if S.invalid?(value)
      end

      value
    end

    def explain(path, via, _in, value)
      S.explain_pred_list(@preds, path, via, _in, value)
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
