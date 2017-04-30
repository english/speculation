# frozen_string_literal: true

# This is a Ruby translation of clojure.spec:
#   https://github.com/clojure/clojure/blob/master/src/clj/clojure/spec.clj
# All credit belongs with Rich Hickey and contributors for their original work.

module Speculation
  # @private
  class OrSpec < Spec
    include NamespacedSymbols
    S = Speculation

    attr_reader :id

    def initialize(named_specs, gen = nil)
      @id = SecureRandom.uuid
      @named_specs = named_specs
      @keys = named_specs.keys
      @preds = preds = named_specs.values
      @gen = gen

      @delayed_specs = Concurrent::Delay.new do
        preds.map { |spec| S.send(:specize, spec) }
      end
    end

    def conform(value)
      @delayed_specs.value!.each_with_index do |spec, index|
        conformed = spec.conform(value)

        unless S.invalid?(conformed)
          return [@keys[index], conformed]
        end
      end

      :"Speculation/invalid"
    end

    def unform(value)
      spec_name, conformed_val = value
      spec = @named_specs.fetch(spec_name)

      S.unform(spec, conformed_val)
    end

    def explain(path, via, inn, value)
      return if S.pvalid?(self, value)

      @named_specs.flat_map do |(key, pred)|
        next if S.pvalid?(pred, value)
        S.explain1(pred, Utils.conj(path, key), via, inn, value)
      end
    end

    def with_gen(gen)
      self.class.new(@named_specs, gen)
    end

    def gen(overrides, path, rmap)
      return @gen if @gen

      gs = @keys.zip(@preds).
        map { |(k, p)|
          rmap = S.inck(rmap, @id)

          unless S.recur_limit?(rmap, @id, path, k)
            Gen.delay { S.gensub(p, overrides, Utils.conj(path, k), rmap) }
          end
        }.
        compact

      unless gs.empty?
        ->(rantly) { rantly.branch(*gs) }
      end
    end
  end
end
