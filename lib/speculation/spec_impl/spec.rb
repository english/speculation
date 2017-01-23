module Speculation
  using Speculation::NamespacedSymbols.refine(self)
  using Conj

  class Spec < SpecImpl
    S = Speculation

    def initialize(predicate, should_conform, gen = nil)
      @predicate = predicate
      @should_conform = should_conform
      @gen = gen
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

    def explain(path, via, _in, value)
      if S.invalid?(S.dt(@predicate, value))
        [{ path: path, val: value, via: via, in: _in, pred: @predicate }]
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
      "#{self.class.to_s}(#{@name || @predicate.inspect})"
    end
  end
end
