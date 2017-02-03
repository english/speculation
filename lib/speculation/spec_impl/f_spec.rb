# frozen_string_literal: true

module Speculation
  using NamespacedSymbols.refine(self)
  using Conj

  class FSpec < SpecImpl
    S = Speculation
    Gen = S::Gen
    STest = S::Test

    # TODO: add block spec

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

    def explain(path, via, inn, f)
      unless f.respond_to?(:call)
        return [{ :path => path, :pred => 'respond_to?(:call)', :val => f, :via => via, :in => inn }]
      end

      specs = { :args => @argspec, :ret => @retspec, :fn => @fnspec }
      args = FSpec.validate_fn(f, specs, 100)
      return if f.equal?(args)

      ret = begin
              f.call(*args)
            rescue => e
              e
            end

      if ret.is_a?(Exception)
        return [{ :path => path, :pred => 'f.call(*args)', :val => args, :reason => ret.message, :via => via, :in => inn }]
      end

      cret = S.dt(@retspec, ret)
      return S.explain1(@retspec, path.conj(:ret), via, inn, ret) if S.invalid?(cret)

      if @fnspec
        cargs = S.conform(@argspec, args)
        S.explain1(@fnspec, path.conj(:fn), via, inn, :args => cargs, :ret => cret)
      end
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

    private_class_method def self.call_valid?(f, specs, args)
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
