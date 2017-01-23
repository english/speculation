module Speculation
  using NamespacedSymbols.refine(self)
  using Conj

  class OrSpec < SpecImpl
    S = Speculation

    attr_reader :id

    def initialize(named_specs, gen = nil)
      @id = SecureRandom.uuid
      @named_specs = named_specs
      @keys = named_specs.keys
      @preds = preds = named_specs.values
      @gen = gen

      @delayed_specs = Concurrent::Delay.new do
        preds.map { |spec| S.specize(spec) }
      end
    end

    def conform(value)
      @delayed_specs.value.each_with_index do |spec, index|
        conformed = spec.conform(value)

        unless S.invalid?(conformed)
          return [@keys[index], value]
        end
      end

      :invalid.ns
    end

    def explain(path, via, _in, value)
      return unless !S.pvalid?(self, value)

      @keys.zip(@preds).flat_map do |(key, pred)|
        next if S.pvalid?(pred, value)
        S.explain1(pred, path.conj(key), via, _in, value)
      end
    end

    def gen(overrides, path, rmap)
      return gen if @gen

      gs = @keys.zip(@preds).
        map { |(k, p)|
          rmap = S.inck(rmap, @id)

          unless S.recur_limit?(rmap, @id, path, k)
            S.gensub(p, overrides, path.conj(k), rmap)
          end
        }.
        compact

      unless gs.empty?
        -> (rantly) { rantly.branch(*gs) }
      end
    end
  end
end
