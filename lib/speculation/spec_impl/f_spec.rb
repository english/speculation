# frozen_string_literal: true

module Speculation
  using NamespacedSymbols.refine(self)
  using Conj

  class FSpec < SpecImpl
    S = Speculation

    attr_reader :argspec, :retspec, :fnspec, :blockspec

    def initialize(argspec: nil, retspec: nil, fnspec: nil, blockspec: nil)
      @argspec = argspec
      @retspec = retspec
      @fnspec = fnspec
      @blockspec = blockspec
    end

    def conform(f)
      raise "Can't conform fspec without args spec: #{inspect}" unless @argspec

      return :invalid.ns unless f.is_a?(Proc) || f.is_a?(Method)

      specs = { :args => @argspec, :ret => @retspec, :fn => @fnspec, :block => @blockspec }

      if f.equal?(FSpec.validate_fn(f, specs, S.fspec_iterations))
        f
      else
        :invalid.ns
      end
    end

    def explain(path, via, inn, f)
      unless f.respond_to?(:call)
        return [{ :path => path, :pred => "respond_to?(:call)", :val => f, :via => via, :in => inn }]
      end

      specs = { :args => @argspec, :ret => @retspec, :fn => @fnspec, :block => @blockspec }
      args, block = FSpec.validate_fn(f, specs, 100)
      return if f.equal?(args)

      ret = begin
              f.call(*args, &block)
            rescue => e
              e
            end

      if ret.is_a?(Exception)
        val = block ? [args, block] : args
        return [{ :path => path, :pred => "f.call(*args)", :val => val, :reason => ret.message.chomp, :via => via, :in => inn }]
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
        ->(*args, &block) do
          unless S.pvalid?(@argspec, args)
            raise S.explain(@argspec, args)
          end

          if @blockspec && !S.pvalid?(@blockspec, block)
            raise S.explain(@blockspec, block)
          end

          S::Gen.generate(S.gen(@retspec, overrides))
        end
      end
    end

    # @private
    # returns f if valid, else smallest
    def self.validate_fn(f, specs, iterations)
      args_gen = S.gen(specs[:args])

      block_gen = if specs[:block]
                    S.gen(specs[:block])
                  else
                    Utils.constantly(nil)
                  end

      combined = ->(r) { [args_gen.call(r), block_gen.call(r)] }

      ret = S::Test.rantly_quick_check(combined, iterations) { |(args, block)|
        call_valid?(f, specs, args, block)
      }

      smallest = ret[:shrunk] && ret[:shrunk][:smallest]
      smallest || f
    end

    private_class_method def self.call_valid?(f, specs, args, block)
      cargs = S.conform(specs[:args], args)
      return if S.invalid?(cargs)

      if specs[:block]
        cblock = S.conform(specs[:block], block)
        return if S.invalid?(cblock)
      end

      ret = f.call(*args, &block)

      cret = S.conform(specs[:ret], ret)
      return if S.invalid?(cret)

      return true unless specs[:fn]

      S.pvalid?(specs[:fn], :args => cargs, :block => block, :ret => cret)
    end
  end
end
