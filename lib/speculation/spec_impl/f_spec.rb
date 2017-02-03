# frozen_string_literal: true

module Speculation
  using NamespacedSymbols.refine(self)

  class FSpec < SpecImpl
    S = Speculation
    Gen = S::Gen
    STest = S::Test

    # TODO: add block spec
    # TODO call_valid?
    # TODO validate_fn

    attr_reader :argspec, :retspec, :fnspec

    def initialize(argspec: nil, retspec: nil, fnspec: nil)
      @argspec = argspec
      @retspec = retspec
      @fnspec = fnspec
    end

    def conform(f)
      raise "Can't conform fspec without args spec: #{inspect}" unless @argspec

      return :invalid.ns unless f.is_a?(Proc) || f.is_a?(Method)

      specs = { :args => @argspec, :ret => @retspec, :fn => @fnspec }

      if f.equal?(FSpec.validate_fn(f, specs, S::FSPEC_ITERATIONS))
        f
      else
        :invalid.ns
      end
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

    # @private
    # returns f if valid, else smallest
    def self.validate_fn(f, specs, iterations)
      g = S.gen(specs[:args])

      ret = STest.rantly_quick_check(g, iterations) { |args|
        call_valid?(f, specs, args)
      }

      smallest = ret.dig(:shrunk, :smallest)
      smallest || f
    end

    def self.call_valid?(f, specs, args)
      cargs = S.conform(specs[:args], args)
      return if S.invalid?(cargs)

      ret = f.call(*args)

      cret = S.conform(specs[:ret], ret)
      return if S.invalid?(cret)

      return true unless specs[:fn]

      S.pvalid?(specs[:fn], :args => cargs, :ret => cret)
    end
  end
end
