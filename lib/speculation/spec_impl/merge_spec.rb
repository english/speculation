module Speculation
  using NamespacedSymbols.refine(self)

  class MergeSpec < SpecImpl
    S = Speculation

    def initialize(preds, gen = nil)
      @preds = preds
      @gen = gen
    end

    def conform(x)
      ms = @preds.map { |pred| S.dt(pred, x) }

      if ms.any?(&S.method(:invalid?))
        :invalid.ns(S)
      else
        ms.reduce(&:merge)
      end
    end

    def explain(path, via, _in, x)
      @preds.
        flat_map { |pred| S.explain1(pred, path, via, _in, x) }.
        compact
    end

    def gen(overrides, path, rmap)
      return @gen if @gen

      gens = @preds.
        map { |pred| S.gensub(pred, overrides, path, rmap) }

      -> (r) do
        gens.map { |gen| gen.call(r) }.reduce(&:merge)
      end
    end
  end
end
