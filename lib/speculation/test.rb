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

    @instrumented_methods = Concurrent::Atom.new(H[])
    @instrument_enabled = true

    # if false, instrumented methods call straight through
    def self.instrument_enabled?
      @instrument_enabled
    end

    # Disables instrument's checking of calls within a block
    def self.with_instrument_disabled(&block)
      @instrument_enabled = false
      block.call
    ensure
      @instrument_enabled = true
    end

    private_class_method def self.spec_checking_fn(ident, method, spec)
      spec = S.send(:maybe_spec, spec)

      conform = -> (method, role, spec, data, args) do
        conformed = S.conform(spec, data)

        if conformed == :invalid.ns
          # TODO stacktrace-relevant-to-instrument
          _caller = caller(4, 1).first

          ed = S.
            _explain_data(spec, [role], [], [], data).
            merge(:args.ns => args, :failure.ns(S) => :instrument, :caller.ns => _caller)

          raise DidNotConformError.new("Call to '#{ident}' did not conform to spec:\n #{S.explain_str(ed)}", ed)
        else
          conformed
        end
      end

      # TODO handle method taking a block as an argument
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

    # TODO no-spec?

    private_class_method def self.instrument1(ident, opts)
      spec = S.get_spec(ident)

      raw, wrapped = @instrumented_methods.
        value.
        fetch(ident, H[]).
        values_at(:raw, :wrapped)

      current = ident.get_method
      to_wrap = wrapped == current ? raw : current

      # TODO handle spec/fn overrides
      checked = spec_checking_fn(ident, current, spec)

      ident.redefine_method!(checked)

      wrapped = ident.get_method

      @instrumented_methods.swap do |methods|
        methods.merge(ident => H[raw: to_wrap, wrapped: wrapped])
      end

      ident
    end

    private_class_method def self.unstrument1(ident)
      instrumented = @instrumented_methods.value[ident]
      return unless instrumented

      raw, wrapped = instrumented.fetch_values(:raw, :wrapped)
      @instrumented_methods.swap { |h| h.except(ident) }

      current = ident.get_method

      # Only redefine to original if it has not been modified since it was
      # instrumented.
      if wrapped == current
        ident.redefine_method!(raw)
        ident
      end
    end

    # TODO stacktrace-relevant-to-instrument

    # Given an opts hash as per instrument, returns the set of methods that can
    # be instrumented.
    def self.instrumentable_methods(opts = {})
      # TODO validate opts
      S.registry.keys.select(&method(:fn_spec_name?)).to_set.tap do |set|
        set.merge(opts[:spec].keys)    if opts[:spec]
        set.merge(opts[:stub])         if opts[:stub]
        set.merge(opts[:replace].keys) if opts[:replace]
      end
    end

    private_class_method def self.fn_spec_name?(spec_name)
      spec_name.is_a?(S::Identifier)
    end

    # Instruments the methods named by method-or-methods, a method or collection
    # of methods, or all instrumentable methods if method-or-methods is not
    # specified.
    # 
    # If a method has an :args fn-spec, replaces the method with a method that
    # checks arg conformance (throwing an exception on failure) before
    # delegating to the original method.
    # 
    # The opts hash can be used to override registered specs, and/or to replace
    # method implementations entirely. Opts for methods not included in
    # method-or-methods are ignored. This facilitates sharing a common options
    # hash across many different calls to instrument.
    # 
    # The opts map may have the following keys:
    # 
    #   :spec     a map from methods to override specs
    #   :stub     a set of methods to be replaced by stubs
    #   :gen      a map from spec names to generator overrides
    #   :replace  a map from methods to replacement procs
    # 
    # :spec overrides registered method specs with specs you provide. Use :spec
    # overrides to provide specs for libraries that do not have them, or to
    # constrain your own use of a fn to a subset of its spec'ed contract.
    # 
    # :stub replaces a fn with a stub that checks :args, then uses the :ret spec
    # to generate a return value.
    # 
    # :gen overrides are used only for :stub generation.
    # 
    # :replace replaces a method with a method that checks args conformance,
    # then invokes the method/proc you provide, enabling arbitrary stubbing and
    # mocking.
    # 
    # :spec can be used in combination with :stub or :replace.
    # 
    # Returns a collection of Identifiers naming the methods instrumented.
    def self.instrument(method_or_methods = instrumentable_methods, opts = {})
      Array(method_or_methods).
        map { |method| S.send(:Identifier, method) }.
        uniq.
        map { |ident| instrument1(ident, opts) }.
        compact
    end

    # Undoes instrument on the method_or_methods, specified as in instrument.
    # With no args, unstruments all instrumented methods. Returns a collection
    # of Identifiers naming the methods unstrumented.
    def self.unstrument(method_or_methods = @instrumented_methods.value.keys)
      Array(method_or_methods).
        map { |method| S.send(:Identifier, method) }.
        map { |ident| unstrument1(ident) }.
        compact
    end

    private_class_method def self.explain_check(args, spec, v, role)
      data = unless S.valid?(spec, v)
               S._explain_data(spec, [role], [], [], v).
                 merge(:args.ns       => args,
                       :val.ns        => v,
                       :failure.ns(S) => :check_failed)
             end

      { backtrace: caller,
        cause: "Specification-based check failed",
        data: data }
    end

    # Returns true if call passes specs, otherwise *returns* an exception
    # with explain_data + :Speculation/failure.
    private_class_method def self.check_call(method, spec, args)
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

    private_class_method def self.quick_check(method, spec, opts)
      gen = opts[:gen]
      num_tests = opts.fetch(:num_tests, 100)

      g = begin
            S.gen(spec.argspec, gen)
          rescue => e
            return { result: e }
          end

      rantly_quick_check(g, num_tests) { |args| check_call(method, spec, args) }
    end

    private_class_method def self.make_check_result(method, spec, check_result)
      result = { :spec         => spec,
                 :ret.ns(self) => check_result,
                 :method       => method }

      if check_result[:result] && check_result[:result] != true
        result[:failure] = check_result[:result]
      end

      if check_result[:shrunk]
        result[:failure] = check_result[:shrunk][:result]
      end

      result
    end

    private_class_method def self.check1(ident, opts)
      spec = S.get_spec(ident)
      specd = S.spec(spec)

      reinstrument = unstrument(ident).any?
      method = ident.get_method

      if spec.argspec
        check_result = quick_check(method, spec, opts)
        make_check_result(method, spec, check_result)
      else
        failure = { :info      => "No :args spec",
                    failure.ns => :no_args_spec }

        { :failure => failure,
          :method  => method,
          :spec    => spec }
      end
    ensure
      instrument(ident) if reinstrument
    end

    # Given an opts hash as per check, returns the set of Identifiers that can
    # be checked.
    def self.checkable_methods(opts = {})
      # TODO validate opts
      S.
        registry.
        keys.
        select(&method(:fn_spec_name?)).
        reject(&:instance_method?).
        to_set.
        tap { |set| set.merge(opts[:spec].keys) if opts[:spec] }
    end

    # Run generative tests for spec conformance on method_or_methods. If
    # sym-or-syms is not specified, check all checkable methods.
    # 
    # The opts hash includes the following optional keys:
    # 
    # :num_tests  number of times to generatively test each method
    # :gen        hash map from spec names to generator overrides
    # 
    # Generator overrides are passed to Speculation.gen when generating method
    # args.
    # 
    # Returns an array of check result hashes with the following keys:
    # 
    # :spec       the spec tested
    # :method     optional method tested
    # :failure    optional test failure
    # :result     optional boolean as to whether all generative tests passed
    # :num_tests  optional number of generative tests ran
    # 
    # :failure is a hash that will contain a :"Speculation/failure" key with
    #   possible values:
    # 
    # :check_failed   at least one checked return did not conform
    # :no_args_spec   no :args spec provided
    # TODO no_fspec
    # :no_fspec       no fspec provided
    # TODO no_gen
    # :no_gen         unable to generate :args
    # :instrument     invalid args detected by instrument
    def self.check(method_or_methods = nil, opts = {})
      method_or_methods ||= checkable_methods

      Array(method_or_methods).
        map { |method| S.send(:Identifier, method) }.
        select { |ident| checkable_methods(opts).include?(ident) }.
        map { |ident| check1(ident, opts) } # TODO pmap?
    end

    # Custom quick check implementation since Rantly's prints result to stdout
    def self.rantly_quick_check(gen, num_tests, &block)
      i = 0
      limit = 10

      Rantly.singleton.generate(num_tests, limit, gen) do |val|
        i += 1

        result = block.call(val)
        unless result == true
          # This is a Rantly Tuple.
          # TODO find an alternative to Rantly
          val = ::Tuple.new(val)

          if val.respond_to?(:shrink)
            shrunk = shrink(val, result, block)
            shrunk[:smallest] = shrunk[:smallest].array

            return {
              fail: val.array,
              num_tests: i,
              result: result,
              shrunk: shrunk,
            }
          else
            return { :fail      => val.array,
                     :num_tests => i,
                     :result    => result }
          end
        end
      end

      { :num_tests => i,
        :result    => true }
    end

    def self.shrink(data, result, block, depth = 0, iteration = 0)
      smallest = data
      max_depth = depth

      if data.shrinkable?
        while iteration < 1024
          shrunk_data = data.shrink
          result = block.call(shrunk_data.array)

          unless result == true
            shrunk = shrink(shrunk_data, result, block, depth + 1, iteration + 1)

            branch_smallest, branch_depth, iteration, branch_result =
              shrunk.values_at(:smallest, :depth, :iteration, :result)

            if branch_depth > max_depth
              smallest = branch_smallest
              max_depth = branch_depth
            end
          end

          break unless data.retry?
        end
      end

      { :depth     => max_depth,
        :iteration => iteration,
        :result    => result,
        :smallest  => smallest }
    end
  end
end
