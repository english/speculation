# frozen_string_literal: true
require "set"
require "rantly"
require "rantly/property"
require "rantly/shrinks"
require "concurrent/delay"
require "date"

module Speculation
  using NamespacedSymbols.refine(self)

  module Gen
    # @private
    GEN_BUILTINS = {
      Integer    => ->(r) { r.integer },
      String     => ->(r) { r.sized(r.range(0, 100)) { string(:alpha) } },
      Float      => ->(_r) { rand(Float::MIN..Float::MAX) },
      Numeric    => ->(r) { r.branch(Gen.gen_for_pred(Integer), Gen.gen_for_pred(Float)) },
      Symbol     => ->(r) { r.sized(r.range(0, 100)) { string(:alpha).to_sym } },
      TrueClass  => ->(_r) { true },
      FalseClass => ->(_r) { false },
      Date       => ->(r) { Gen.gen_for_pred(Time).call(r).to_date },
      Time       => ->(r) { Time.at(r.range(-569001744000, 569001744000)) }, # 20k BC => 20k AD
      Array      => ->(r) do
        size = r.range(0, 20)

        r.array(size) do
          gen = Gen.gen_for_pred(r.choose(Integer, String, Float, Symbol, Date, Time, Set[true, false]))
          gen.call(r)
        end
      end,
      Set        => ->(r) do
        gen = Gen.gen_for_pred(Array)
        Set.new(gen.call(r))
      end,
      Hash       => ->(r) do
        kgen = Gen.gen_for_pred(r.choose(Integer, String, Float, Symbol, Date, Time))
        vgen = Gen.gen_for_pred(r.choose(Integer, String, Float, Symbol, Date, Time, Set[true, false]))
        size = r.range(0, 20)

        h = {}
        r.each(size) do
          k = kgen.call(r)
          r.guard(!h.key?(k))
          h[k] = vgen.call(r)
        end
        h
      end,
      Enumerable => ->(r) do
        klass = r.choose(Array, Hash, Set)
        gen = Gen.gen_for_pred(klass)
        gen.call(r)
      end
    }.freeze

    # Adds `pred` as a Rantly `guard` to generator `gen`.
    # @param pred
    # @param gen [Proc]
    # @return [Proc]
    # @see https://github.com/abargnesi/rantly Rantly
    def self.such_that(pred, gen)
      ->(rantly) do
        gen.call(rantly).tap { |val| rantly.guard(pred.call(val)) }
      end
    end

    # @param gen [Proc] Rantly generator
    # @param limit [Integer]  specifies how many times `gen` can fail to produce a valid value.
    # @return single value using gen
    # @see https://github.com/abargnesi/rantly Rantly
    def self.generate(gen, limit = 100)
      Rantly.value(limit, &gen)
    end

    # Generate `n` values using `gen`
    # @param gen [Proc] Rantly generator
    # @param limit [Integer] specifies how many times `gen` can fail to produce a valid value.
    # @return [Array] array of generated values using gne
    # @see https://github.com/abargnesi/rantly Rantly
    def self.sample(gen, n, limit = 100)
      Rantly.map(n, limit, &gen)
    end

    # Given a predicate, returns a built-in generator if one exists.
    # @param pred
    # @return [Proc] built-in generator for pred
    # @return nil if no built-in generator found
    # @see https://github.com/abargnesi/rantly Rantly
    def self.gen_for_pred(pred)
      if pred.is_a?(Set)
        ->(r) { r.choose(*pred) }
      else
        GEN_BUILTINS[pred]
      end
    end

    # @private
    def self.delay(&block)
      delayed = Concurrent::Delay.new(&block)

      ->(rantly) do
        delayed.value.call(rantly)
      end
    end
  end
end
