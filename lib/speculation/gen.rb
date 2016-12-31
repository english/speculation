require 'speculation/core'
require 'rantly'

module Speculation
  using namespaced_symbols(self)

  module Gen
    H = Hamster::Hash
    V = Hamster::Vector

    def self.make_gen(&block)
      H[gen: block]
    end

    @gen_builtins = H[
      Integer => make_gen { integer },
      String => make_gen { string }
    ]

    # TODO honor max tries
    def self.such_that(pred, gen, max_tries)
      gen = gen.fetch(:gen)

      make_gen do
        instance_exec(&gen).tap do |val|
          guard(pred.call(val))
        end
      end
    end

    # TODO handle pred being a set
    def self.gen_for_pred(pred)
      @gen_builtins[pred]
    end

    def self.generate(gen)
      gen = gen.fetch(:gen)
      Rantly.value { instance_exec(&gen) }
    end

    def self.sample(gen, n)
      gen = gen.fetch(:gen)
      Rantly.map(n) { instance_exec(&gen) }
    end
  end
end
