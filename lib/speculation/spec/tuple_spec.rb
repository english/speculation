# frozen_string_literal: true

# This is a Ruby translation of clojure.spec:
#   https://github.com/clojure/clojure/blob/master/src/clj/clojure/spec.clj
# All credit belongs with Rich Hickey and contributors for their original work.

module Speculation
  # @private
  class TupleSpec < Spec
    include NamespacedSymbols
    S = Speculation

    def initialize(preds, gen = nil, name = nil)
      @preds = preds
      @gen = gen
      @name = name

      @delayed_specs = Concurrent::Delay.new do
        preds.map { |pred| S.send(:specize, pred) }
      end
    end

    def conform(collection)
      specs = @delayed_specs.value!

      unless Predicates.array?(collection) && collection.count == specs.count
        return :"Speculation/invalid"
      end

      return_value = collection.class.new

      collection.zip(specs).each do |(value, spec)|
        conformed_value = spec.conform(value)

        if S.invalid?(conformed_value)
          return :"Speculation/invalid"
        else
          return_value += [conformed_value]
        end
      end

      return_value
    end

    def unform(value)
      unless Predicates.array?(value) && value.count == @preds.count
        raise ArgumentError, "unform value must be an array of length #{@preds.count}"
      end

      @preds.zip(value).map { |(pred, val)| S.unform(pred, val) }
    end

    def explain(path, via, inn, value)
      if !Predicates.array?(value)
        [{ :path => path, :val => value, :via => via, :in => inn, :pred => [Predicates.method(:array?), [value]] }]
      elsif @preds.count != value.count
        [{ :path => path, :val => value, :via => via, :in => inn, :pred => [Predicates.method(:count_eq?), [@preds, value.count]] }]
      else
        probs = @preds.zip(value).each_with_index.flat_map { |(pred, x), index|
          unless S.pvalid?(pred, x)
            S.explain1(pred, Utils.conj(path, index), via, Utils.conj(inn, index), x)
          end
        }

        probs.compact
      end
    end

    def with_gen(gen)
      self.class.new(@preds, gen, @name)
    end

    def with_name(name)
      self.class.new(@preds, @gen, name)
    end

    def gen(overrides, path, rmap)
      return @gen.call if @gen

      gens = @preds.each_with_index.
        map { |p, i| S.gensub(p, overrides, Utils.conj(path, i), rmap) }

      Radagen.tuple(*gens)
    end
  end
end
