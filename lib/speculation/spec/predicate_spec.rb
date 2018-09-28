# frozen_string_literal: true

# This is a Ruby translation of clojure.spec:
#   https://github.com/clojure/clojure/blob/master/src/clj/clojure/spec.clj
# All credit belongs with Rich Hickey and contributors for their original work.

module Speculation
  # @private
  class PredicateSpec < Spec
    include NamespacedSymbols
    S = Speculation

    def initialize(predicate, should_conform, gen, unconformer)
      @predicate = predicate
      @should_conform = should_conform
      @gen = gen
      @unconformer = unconformer
    end

    def conform(value)
      ret = @predicate === value

      if @should_conform
        ret
      else
        ret ? value : :"Speculation/invalid"
      end
    end

    def unform(value)
      return value unless @should_conform

      if @unconformer
        @unconformer.call(value)
      else
        raise "no unformer for conformer"
      end
    end

    def explain(path, via, inn, value)
      if S.invalid?(S.dt(@predicate, value))
        [{ :path => path, :val => value, :via => via, :in => inn, :pred => [@predicate, [value]] }]
      end
    end

    def with_gen(gen)
      self.class.new(@predicate, @should_conform, gen, @unconformer)
    end

    def gen(_, _, _)
      if @gen
        @gen.call
      else
        Gen.gen_for_pred(@predicate)
      end
    end

    def inspect
      "#{self.class}(#{@name || @predicate.inspect})"
    end
  end
end
