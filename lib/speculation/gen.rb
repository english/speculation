# frozen_string_literal: true

# This is a Ruby translation of clojure.spec.gen:
#   https://github.com/clojure/clojure/blob/master/src/clj/clojure/spec/gen.clj
# All credit belongs with Rich Hickey and contributors for their original work.

require "set"
require "rantly"
require "rantly/property"
require "rantly/shrinks"
require "concurrent/delay"
require "date"
require "securerandom"
require "uri"
require "date"
require "time"

module Speculation
  module Gen
    # Adds pred block as a Rantly `guard` to generator `gen`.
    # @param gen [Proc]
    # @yield generated value
    # @return [Proc]
    # @see https://github.com/abargnesi/rantly Rantly
    def self.such_that(gen)
      raise ArgumentError, "block required" unless block_given?

      ->(rantly) do
        gen.call(rantly).tap { |val| rantly.guard(yield(val)) }
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
    def self.sample(gen, n = 10, limit = 100)
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
        delayed.value!.call(rantly)
      end
    end

    # @private
    def self.tuple(*generators)
      ->(r) do
        generators.map { |g| g.call(r) }
      end
    end

    # @private
    def self.fmap(gen)
      ->(rantly) do
        yield gen.call(rantly)
      end
    end

    # @private
    GEN_BUILTINS = {
      Integer    => ->(r) { r.integer },
      String     => ->(r) { r.sized(r.range(0, 20)) { string(:alpha) } },
      Float      => ->(_r) { rand(Float::MIN..Float::MAX) },
      Numeric    => ->(r) { r.branch(Gen.gen_for_pred(Integer), Gen.gen_for_pred(Float)) },
      Symbol     => ->(r) { r.sized(r.range(0, 20)) { string(:alpha).to_sym } },
      TrueClass  => ->(_r) { true },
      FalseClass => ->(_r) { false },
      NilClass   => ->(_r) { nil },
      Date       => Speculation.gen(Speculation.date_in(Date.new(1970, 1, 1)..Date.new(3000, 1, 1))),
      Time       => Speculation.gen(Speculation.time_in(Time.new(1970, 1, 1)..Time.new(3000, 1, 1))),
      URI        => ->(_r) { URI("http://#{SecureRandom.uuid}.com") },
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
      SortedSet  => ->(r) do
        gen = Gen.gen_for_pred(Array)
        SortedSet.new(gen.call(r))
      end,
      Enumerator => ->(r) do
        gen = Gen.gen_for_pred(Array)
        gen.call(r).to_enum
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
  end
end
