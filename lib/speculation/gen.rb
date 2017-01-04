# require 'speculation/core'
require 'rantly'
require 'rantly/shrinks'
require 'hamster/hash'
require 'hamster/vector'

module Speculation
  using NamespacedSymbols.refine(self)

  module Gen
    H = Hamster::Hash
    V = Hamster::Vector

    @gen_builtins = H[
      Integer => -> (r) { r.integer },
      String  => -> (r) { r.sized(r.range(0, 100)) { string(:alpha) } },
      Float   => -> (r) { rand(Float::MIN..Float::MAX) },
      Numeric => -> (r) { r.choose(rand(Float::MIN..Float::MAX), r.integer) },
      Symbol  => -> (r) { r.sized(r.range(0, 100)) { string(:alpha).to_sym } },
    ]

    # TODO honor max tries
    def self.such_that(pred, gen, max_tries)
      -> (rantly) do
        gen.call(rantly).tap do |val|
          rantly.guard(pred.call(val))
        end
      end
    end

    def self.gen_for_pred(pred)
      if pred.is_a?(Set)
        -> (r) { r.choose(*pred) }
      else
        @gen_builtins[pred]
      end
    end

    def self.generate(gen)
      Rantly.value { gen.call(self) }
    end

    def self.sample(gen, n)
      Rantly.map(n) { gen.call(self) }
    end
  end
end
