# frozen_string_literal: true

module Speculation
  # @private
  class NonconformingSpec < Spec
    include NamespacedSymbols
    S = Speculation

    def initialize(spec, gen = nil)
      @spec = spec
      @gen = gen
      @delayed_spec = Concurrent::Delay.new { S.send(:specize, spec) }
    end

    def conform(value)
      ret = @delayed_spec.value!.conform(value)

      S.invalid?(ret) ? S::INVALID : value
    end

    def explain(path, via, inn, value)
      @delayed_spec.value!.explain(path, via, inn, value)
    end

    def with_gen(gen)
      self.class.new(@spec, gen)
    end

    def gen(overrides, path, rmap)
      @delayed_spec.value!.gen(overrides, path, rmap)
    end
  end
end
