# require 'speculation/core'

module Speculation
  using NamespacedSymbols.refine(self)

  module Test
    class DidNotConformError < StandardError
      attr_reader :explain_data

      def initialize(message, explain_data)
        super(message)
        @explain_data = explain_data
      end
    end

    H = Hamster::Hash
    V = Hamster::Vector
    INSTRUMENTED_METHODS = Concurrent::Atom.new(H[])

    @instrument_enabled = true

    def self.with_instrument_disabled
      @instrument_enabled = false
      yield
    ensure
      @instrument_enabled = true
    end

    def self.instrument_enabled?
      @instrument_enabled
    end

    def self.instrument(method)
      # TODO take a colleciton of methods, or all instrumentable methods
      # TODO take options
      instrument1(method)
    end

    def self.instrument1(method)
      spec = Core.get_spec(method)
      return unless spec

      instrumented_method = INSTRUMENTED_METHODS.value.fetch(method.hash, H[])

      to_wrap = if instrumented_method[:wrapped] == method
                  instrumented_method.fetch(:raw)
                else
                  method
                end

      checked = spec_checking_fn(method, spec)

      if method.is_a?(UnboundMethod)
        method.owner.class_eval { define_method(method.name, checked) }
      else
        method.receiver.define_singleton_method(method.name, checked)
      end

      INSTRUMENTED_METHODS.swap do |methods|
        methods.store(method.hash, H[raw: to_wrap, wrapped: checked])
      end

      method
    end

    def self.spec_checking_fn(method, spec)
      spec = Core.maybe_spec(spec) # TODO needed?

      conform = -> (method, role, spec, data, args) do
        conformed = Core.conform(spec, data)

        if conformed == :invalid.ns
          _caller = caller(2, 1).first # TODO stacktrace-relevant-to-instrument

          ed = Core.
            _explain_data(spec, V[role], V[], V[], data).
            store(:args.ns, args).
            store(:failure.ns, :instrument).
            store(:caller.ns, _caller)

          raise DidNotConformError.new("Call to '#{method.name}' did not conform to spec:\n #{Core.explain_str(ed)}", ed)
        else
          conformed
        end
      end

      -> (*args) do
        method = method.bind(self) if method.is_a?(UnboundMethod)

        if Test.instrument_enabled?
          conform.call(method, :args, spec.args, args, args) if spec.args
          Test.with_instrument_disabled { method.call(*args) }
        else
          method.call(*args)
        end
      end
    end

    def self.checkable_methods(opts = {})
      # TODO validate opts
      Core.registry.keys.select(&method(:fn_spec_name?)).to_set.tap do |set|
        set << opts[:spec].keys if opts[:spec]
      end
    end

    def self.check(method_or_methods = checkable_methods, opts = {})
      Array(method_or_methods).
        select { |method| checkable_methods(opts).include?(method) }.
        map { |method| check1(method, opts) } # TODO pmap?
    end

    def self.fn_spec_name?(spec_name)
      spec_name.is_a?(Method) || spec_name.is_a?(UnboundMethod)
    end

    def self.check1(method, opts)
      spec = Core.get_spec(method)
      specd = Core.spec(spec)

      # TODO handle unstrument-ing if already instrumented
      # TODO handle method not existing anymore???

      if spec.args
        check_result = quick_check(method, spec, opts)
        make_check_result(method, spec, check_result)
      else
        {
          failure: { info: "No :args spec", failure.ns => :no_args_spec },
          method: method,
          spec: spec
        }
      end
    end

    def self.quick_check(method, spec, opts)
      gen = opts[:gen]
      num_tests = opts.fetch(:num_tests, 100)

      g = begin
            Core.gen(spec.args, gen)
          rescue => e
            return { result: e }
          end

      rantly_quick_check(g, num_tests) { |args| check_call(method, spec, args) }
    end

    def self.rantly_quick_check(gen, num_tests, &block)
      i = 0
      limit = 10

      Rantly.singleton.generate(num_tests, 10, gen) do |val|
        i += 1

        result = block.call(val)
        unless result == true
          if val.respond_to?(:shrink?)
            shrunk = shrink(val, result, block)

            return {
              fail: val,
              num_tests: i,
              result: result,
              shrunk: shrunk,
            }
          else
            return {
              fail: val,
              num_tests: i,
              result: result,
            }
          end
        end
      end

      {
        num_tests: i,
        result: true,
      }
    end

    def self.shrink(data, result, block, depth = 0, iteration = 0)
      smallest = data
      max_depth = depth

      if data.shrinkable?
        while iteration < 1024
          shrunk_data = data.shrink
          result = block.call(shrunk_data)

          unless result == true
            shrunk = shrink(assertion, shrunk_data, depth + 1, iteration + 1)

            branch_smallest, branch_depth, iteration, branch_result =
              shrunk.values_at(:smallest, :depth, :iteration, :result)

            if branch_depth > max_depth
              smallest = branch_smallest
              max_depth = branch_depth
              branch_result = result
            end
          end

          break unless data.retry?
        end
      end

      {
        depth: max_depth,
        iteration: iteration,
        result: result,
        smallest: smallest,
      }
    end

    class SpecificiationBasedCheckFailed < StandardError
      attr_reader :data

      def initialize(data)
        @data = data
        super
      end
    end

    def self.explain_check(args, spec, v, role)
      data = unless Core.valid?(spec, v)
               Core._explain_data(spec, [role], [], [], v).
                 merge(:args.ns => args,
                       :val.ns => v,
                       :failure.ns => :check_failed)
             end

      {
        backtrace: caller,
        cause: "Specification-based check failed",
        data: data,
      }
    end

    def self.check_call(method, spec, args)
      conformed_args = Core.conform(spec.args, args) if spec.args

      if conformed_args == :invalid.ns
        return explain_check(args, spec.args, args, :args)
      end

      ret = method.call(*args)
      conformed_ret = Core.conform(spec.ret, ret) if spec.ret

      if conformed_ret == :invalid.ns
        return explain_check(args, spec.ret, ret, :ret)
      end

      return true unless spec.args && spec.ret && spec.fn

      if Core.valid?(spec.fn, args: conformed_args, ret: conformed_ret)
        true
      else
        explain_check(args, spec.fn, { args: conformed_args, ret: conformed_ret }, :fn)
      end
    end

    def self.make_check_result(method, spec, check_result)
      result = {
        spec: spec,
        :ret.ns("Speculation::Test::Check") => check_result,
        method: method,
      }

      if check_result[:result] && check_result[:result] != true
        result[:failure] = check_result[:result]
      end

      if check_result[:shrunk]
        result[:failure] = check_result[:shrunk][:result]
      end

      result
    end
  end
end
