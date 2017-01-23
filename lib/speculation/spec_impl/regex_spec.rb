module Speculation
  using NamespacedSymbols.refine(self)

  class RegexSpec < SpecImpl
    S = Speculation

    def initialize(regex)
      @regex = regex
    end

    def conform(value)
      if value.nil? || Utils.collection?(value)
        S.re_conform(@regex, value)
      else
        :invalid.ns
      end
    end

    def explain(path, via, _in, value)
      if value.nil? || Utils.collection?(value)
        S.re_explain(path, via, _in, @regex, value || [])
      else
        [{ path: path, val: value, via: via, in: _in }]
      end
    end

    def gen(overrides, path, rmap)
      return @gen if @gen

      S.re_gen(@regex, overrides, path, rmap)
    end
  end
end
