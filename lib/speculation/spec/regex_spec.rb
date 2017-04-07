# frozen_string_literal: true

module Speculation
  # @private
  class RegexSpec < Spec
    include NamespacedSymbols
    S = Speculation

    def initialize(regex, gen = nil)
      @regex = regex
      @gen = gen
    end

    def conform(value)
      if value.nil? || Utils.collection?(value)
        S.re_conform(@regex, value)
      else
        S::INVALID
      end
    end

    def explain(path, via, inn, value)
      if value.nil? || Utils.collection?(value)
        S.re_explain(path, via, inn, @regex, value || [])
      else
        [{ :path => path, :val => value, :via => via, :in => inn }]
      end
    end

    def with_gen(gen)
      self.class.new(@regex, gen)
    end

    def gen(overrides, path, rmap)
      return @gen if @gen

      S.re_gen(@regex, overrides, path, rmap)
    end
  end
end
