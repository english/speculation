# frozen_string_literal: true
require "set"
require "rantly"
require "rantly/property"
require "rantly/shrinks"
require "hamster/hash"
require "hamster/vector"

module Speculation
  using NamespacedSymbols.refine(self)

  module Gen
    H = Hamster::Hash
    V = Hamster::Vector

    GEN_BUILTINS = H[
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
    ]

    # TODO: honor max tries
    def self.such_that(pred, gen, _max_tries)
      ->(rantly) do
        gen.call(rantly).tap do |val|
          rantly.guard(pred.call(val))
        end
      end
    end

    def self.gen_for_pred(pred)
      if pred.is_a?(Set)
        ->(r) { r.choose(*pred) }
      else
        GEN_BUILTINS[pred]
      end
    end

    def self.generate(gen)
      Rantly.value(&gen)
    end

    def self.sample(gen, n)
      Rantly.map(n, &gen)
    end
  end
end
