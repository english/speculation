# frozen_string_literal: true
module Speculation
  using Speculation::NamespacedSymbols.refine(self)
  using Conj

  class Spec < SpecImpl
    S = Speculation

    def initialize(predicate, should_conform)
      @predicate = predicate
      @should_conform = should_conform
    end

    def conform(value)
      ret = case @predicate
            when Set            then @predicate.include?(value)
            when Regexp, Module then @predicate === value
            else                     @predicate.call(value)
            end

      if @should_conform
        ret
      else
        ret ? value : :invalid.ns
      end
    end

    def explain(path, via, inn, value)
      if S.invalid?(S.dt(@predicate, value))
        [{ :path => path, :val => value, :via => via, :in => inn, :pred => @predicate }]
      end
    end

    def gen(_, _, _)
      if @gen
        @gen
      else
        Gen.gen_for_pred(@predicate)
      end
    end

    def inspect
      "#{self.class}(#{@name || @predicate.inspect})"
    end
  end
end