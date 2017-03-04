# frozen_string_literal: true
module Speculation
  # @private
  class OrSpec < SpecImpl
    include NamespacedSymbols
    S = Speculation

    attr_reader :id

    def initialize(named_specs)
      @id = SecureRandom.uuid
      @named_specs = named_specs
      @keys = named_specs.keys
      @preds = preds = named_specs.values

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

      ns(S, :invalid)
    end

    def explain(path, via, inn, value)
      return if S.pvalid?(self, value)

      @keys.zip(@preds).flat_map do |(key, pred)|
        next if S.pvalid?(pred, value)
        S.explain1(pred, Utils.conj(path, key), via, inn, value)
      end
    end

    def gen(overrides, path, rmap)
      return gen if @gen

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
