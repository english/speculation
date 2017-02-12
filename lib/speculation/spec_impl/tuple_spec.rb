# frozen_string_literal: true
module Speculation
  using Speculation::NamespacedSymbols.refine(self)
  using Conj

  # @private
  class TupleSpec < SpecImpl
    S = Speculation

    def initialize(preds)
      @preds = preds

      @delayed_specs = Concurrent::Delay.new do
        preds.map { |pred| S.send(:specize, pred) }
      end
    end

    def conform(collection)
      specs = @delayed_specs.value

      unless Utils.array?(collection) && collection.count == specs.count
        return :invalid.ns
      end

      return_value = collection.class.new

      collection.zip(specs).each do |(value, spec)|
        conformed_value = spec.conform(value)

        if S.invalid?(conformed_value)
          return :invalid.ns
        else
          return_value += [conformed_value]
        end
      end

      return_value
    end

    def explain(path, via, inn, value)
      if !Utils.array?(value)
        [{ :path => path, :val => value, :via => via, :in => inn, :pred => "array?" }]
      elsif @preds.count != value.count
        [{ :path => path, :val => value, :via => via, :in => inn, :pred => "count == predicates.count" }]
      else
        probs = @preds.zip(value).each_with_index.flat_map { |(pred, x), index|
          unless S.pvalid?(pred, x)
            S.explain1(pred, path.conj(index), via, inn.conj(index), x)
          end
        }

        probs.compact
      end
    end

    def gen(overrides, path, rmap)
      return @gen if @gen

      gens = @preds.each_with_index.
        map { |p, i| S.gensub(p, overrides, path.conj(i), rmap) }

      ->(rantly) do
        gens.map { |g| g.call(rantly) }
      end
    end
  end
end
