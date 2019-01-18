# frozen_string_literal: true

# This is a Ruby translation of clojure.spec:
#   https://github.com/clojure/clojure/blob/master/src/clj/clojure/spec.clj
# All credit belongs with Rich Hickey and contributors for their original work.

module Speculation
  # @private
  class RegexSpec < Spec
    include NamespacedSymbols
    S = Speculation

    def initialize(regex, gen = nil, name = nil)
      @regex = regex
      @gen = gen
      @name = name
    end

    def conform(value)
      if value.nil? || Predicates.sequential?(value)
        S.re_conform(@regex, value)
      else
        :"Speculation/invalid"
      end
    end

    def unform(value)
      S.op_unform(@regex, value)
    end

    def explain(path, via, inn, value)
      if Predicates.nil_or_sequential?(value)
        S.re_explain(path, via, inn, @regex, value || [])
      else
        [{ :path => path, :pred => [Predicates.method(:nil_or_sequential?), [value]], :val => value, :via => via, :in => inn }]
      end
    end

    def with_gen(gen)
      self.class.new(@regex, gen, @name)
    end

    def with_name(name)
      self.class.new(@regex, @gen, name)
    end

    def gen(overrides, path, rmap)
      return @gen.call if @gen

      S.re_gen(@regex, overrides, path, rmap)
    end
  end
end
