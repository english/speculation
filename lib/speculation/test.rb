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
      num_tests = opts.fetch(:num_tests, 10000)

      g = begin
            Core.gen(spec.args, gen)
          rescue => e
            e
          end

      if g.is_a?(Exception)
        { result: g }
      else
        prop = ::Rantly::Property.new(g)

        e = nil
        tests_ran = 0
        begin
          prop.check(num_tests) do |args|
            tests_ran += 1
            conformed_args = Core.conform(spec.args, args) if spec.args

            result =
              if conformed_args == :invalid.ns
                explain_check(args, spec.args, args, :args)
              else
                ret = method.call(*args)
                conformed_ret = Core.conform(spec.ret, ret) if spec.ret

                if conformed_ret == :invalid.ns
                  explain_check(args, spec.ret, ret, :ret)
                else
                  if spec.args && spec.ret && spec.fn
                    if Core.valid?(spec.fn, args: conformed_args, ret: conformed_ret)
                      true
                    else
                      explain_check(args, spec.fn, { args: conformed_args, ret: conformed_ret }, :fn)
                    end
                  else
                    true
                  end
                end
              end

            unless result == true
              raise result
            end
          end
        rescue SpecificiationBasedCheckFailed => ex
          e = ex
        end

        {
          failed_data: prop.failed_data,
          shrunk_failed_data: prop.shrunk_failed_data,
          num_tests: tests_ran
        }.tap do |ret|
          ret.merge!(ex.data.to_h) if ex
        end
      end
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

      SpecificiationBasedCheckFailed.new(data)
    end

    def self.make_check_result(method, spec, check_result)
      {
        spec: spec,
        :ret.ns("Speculation::Test::Check") => check_result,
        method: method,
      }.tap do |result|
        if check_result[:failed_data]
          result[:failure] = check_result[:failed_data]
        end

        if check_result[:shrunk_failed_data]
          result[:failure] = check_result[:shrunk_failed_data]
        end
      end
    end
  end
end
