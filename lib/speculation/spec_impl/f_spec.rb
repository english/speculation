# frozen_string_literal: true
module Speculation
  using NamespacedSymbols.refine(self)

  class FSpec < SpecImpl
    S = Speculation

    # TODO: add block spec
    # TODO call_valid?
    # TODO validate_fn

    attr_reader :argspec, :retspec, :fnspec

    def initialize(argspec: nil, retspec: nil, fnspec: nil)
      @argspec = argspec
      @retspec = retspec
      @fnspec = fnspec
    end

    def conform(value)
      raise "Can't conform fspec without args spec: #{inspect}" unless @argspec
      # TODO: value.is_a?(Method) correct? maybe Identifier?
      return :invalid.ns unless value.is_a?(Proc) || value.is_a?(Method)

      # TODO: quick-check the function to determine validity
      #       returning fn here so that fn generation can happen
      #       (since it will can fspec.conform and check it's not
      #       :invalid
      value
    end

    def explain(_path, _via, _inn, _value)
      # TODO: implement me
      raise NotImplementedError
    end

    def gen(overrides, _path, _rmap)
      return @gen if @gen

      ->(_rantly) do
        ->(*args) do
          unless S.pvalid?(@argspec, args)
            raise S.explain(@argspec, args)
          end

          Gen.generate(S.gen(@retspec, overrides))
        end
      end
    end
  end
end
