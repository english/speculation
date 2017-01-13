require 'concurrent/atom'

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

    S = Speculation
    H = Hamster::Hash
    V = Hamster::Vector

    @instrumented_methods = Concurrent::Atom.new(H[])
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
    
    def self.instrumentable_methods(opts = {})
      # TODO validate opts
      S.registry.keys.select(&method(:fn_spec_name?)).to_set.tap do |set|
        set << opts[:spec].keys    if opts[:spec]
        set << opts[:stub]         if opts[:stub]
        set << opts[:replace].keys if opts[:replace]
      end
    end

    def self.instrument(method_or_methods = instrumentable_methods, opts = {})
      Array(method_or_methods).
        map { |method| S::Identifier(method) }.
        uniq.
        map { |ident| instrument1(ident, opts) }.
        compact
    end

    def self.instrument1(ident, opts)
      spec = S.get_spec(ident)
      return unless spec

      raw, wrapped = @instrumented_methods.
        value.
        fetch(ident, H[]).
        values_at(:raw, :wrapped)

      current = ident.get_method
      to_wrap = wrapped == current ? raw : current
      checked = spec_checking_fn(ident, current, spec)

      ident.redefine_method!(checked)

      wrapped = ident.get_method

      @instrumented_methods.swap do |methods|
        methods.store(ident, H[raw: to_wrap, wrapped: wrapped])
      end

      ident
    end

    def self.unstrument(method_or_methods = nil)
      method_or_methods ||= @instrumented_methods.value.keys

      Array(method_or_methods).
        map { |method| S::Identifier(method) }.
        map { |ident| unstrument1(ident) }.
        compact
    end

    def self.unstrument1(ident)
      instrumented = @instrumented_methods.value[ident]
      return unless instrumented

      raw, wrapped = instrumented.fetch_values(:raw, :wrapped)
      @instrumented_methods.swap { |h| h.except(ident) }

      current = ident.get_method

      if wrapped == current
        ident.redefine_method!(raw)
        ident
      end
    end

    def self.spec_checking_fn(ident, method, spec)
      spec = S.maybe_spec(spec) # TODO needed?

      conform = -> (method, role, spec, data, args) do
        conformed = S.conform(spec, data)

        if conformed == :invalid.ns
          _caller = caller(4, 1).first # TODO stacktrace-relevant-to-instrument

          ed = S.
            _explain_data(spec, V[role], V[], V[], data).
            store(:args.ns, args).
            store(:failure.ns, :instrument).
            store(:caller.ns, _caller)

          raise DidNotConformError.new("Call to '#{ident}' did not conform to spec:\n #{S.explain_str(ed)}", ed)
        else
          conformed
        end
      end

      -> (*args) do
        method = method.bind(self) if method.is_a?(UnboundMethod)

        if Test.instrument_enabled?
          Test.with_instrument_disabled do
            conform.call(method, :args, spec.argspec, args, args) if spec.argspec

            begin
              @instrument_enabled = true
              method.call(*args)
            ensure
              @instrument_enabled = false
            end
          end
        else
          method.call(*args)
        end
      end
    end

    def self.checkable_methods(opts = {})
      # TODO validate opts
      S.
        registry.
        keys.
        select(&method(:fn_spec_name?)).
        reject(&:instance_method?).
        to_set.
        tap { |set| set << opts[:spec].keys if opts[:spec] }
    end

    def self.check(method_or_methods = checkable_methods, opts = {})
      Array(method_or_methods).
        map { |method| S::Identifier(method) }.
        select { |ident| checkable_methods(opts).include?(ident) }.
        map { |ident| check1(ident, opts) } # TODO pmap?
    end

    def self.fn_spec_name?(spec_name)
      spec_name.is_a?(S::Identifier)
    end

    def self.check1(ident, opts)
      spec = S.get_spec(ident)
      specd = S.spec(spec)

      reinstrument = unstrument(ident).any?

      # TODO handle method not existing anymore?
      method = ident.get_method

      if spec.argspec
        check_result = quick_check(method, spec, opts)
        make_check_result(method, spec, check_result)
      else
        {
          failure: { info: "No :args spec", failure.ns => :no_args_spec },
          method: method,
          spec: spec
        }
      end
    ensure
      instrument(ident) if reinstrument
    end

    def self.quick_check(method, spec, opts)
      gen = opts[:gen]
      num_tests = opts.fetch(:num_tests, 100)

      g = begin
            S.gen(spec.argspec, gen)
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
      data = unless S.valid?(spec, v)
               S._explain_data(spec, [role], [], [], v).
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
      conformed_args = S.conform(spec.argspec, args) if spec.argspec

      if conformed_args == :invalid.ns
        return explain_check(args, spec.argspec, args, :args)
      end

      ret = method.call(*args)
      conformed_ret = S.conform(spec.retspec, ret) if spec.retspec

      if conformed_ret == :invalid.ns
        return explain_check(args, spec.retspec, ret, :ret)
      end

      return true unless spec.argspec && spec.retspec && spec.fnspec

      if S.valid?(spec.fnspec, args: conformed_args, ret: conformed_ret)
        true
      else
        explain_check(args, spec.fnspec, { args: conformed_args, ret: conformed_ret }, :fn)
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
