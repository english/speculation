# frozen_string_literal: true
require "concurrent"
require "pp"
require "speculation/pmap"

module Speculation
  module Test
    using NamespacedSymbols.refine(self)
    using Pmap

    S = Speculation
    H = Hamster::Hash

    @instrumented_methods = Concurrent::Atom.new(H[])
    @instrument_enabled = Concurrent::ThreadLocalVar.new(true)

    class << self
      # if false, instrumented methods call straight through
      attr_accessor :instrument_enabled
    end

    # Disables instrument's checking of calls within a block
    def self.with_instrument_disabled(&block)
      instrument_enabled.value = false
      block.call
    ensure
      instrument_enabled.value = true
    end

    private_class_method def self.spec_checking_fn(ident, method, fspec)
      fspec = S.maybe_spec(fspec)

      conform = ->(args, block) do
        conformed_args = S.conform(fspec.argspec, args)
        conformed_block = S.conform(fspec.blockspec, block) if fspec.blockspec

        if conformed_args == :invalid.ns(S)
          # TODO: stacktrace-relevant-to-instrument
          caller = caller(4, 1).first

          ed = S.
            _explain_data(fspec.argspec, [:args], [], [], args).
            merge(:args.ns(S) => args, :failure.ns(S) => :instrument, :caller.ns => caller)

          raise Speculation::Error.new("Call to '#{ident}' did not conform to spec:\n #{S.explain_str(ed)}", ed)
        elsif conformed_block == :invalid.ns(S)
          # TODO: stacktrace-relevant-to-instrument
          caller = caller(4, 1).first

          ed = S.
            _explain_data(fspec.blockspec, [:block], [], [], block).
            merge(:block.ns(S) => block, :failure.ns(S) => :instrument, :caller.ns => caller)

          raise Speculation::Error.new("Call to '#{ident}' did not conform to spec:\n #{S.explain_str(ed)}", ed)
        end
      end

      # TODO: handle method taking a block as an argument
      ->(*args, &block) do
        method = method.bind(self) if method.is_a?(UnboundMethod)

        if Test.instrument_enabled.value
          Test.with_instrument_disabled do
            conform.call(args, block) if fspec.argspec # TODO: and blockspec?

            begin
              Test.instrument_enabled.value = true
              method.call(*args, &block)
            ensure
              Test.instrument_enabled.value = false
            end
          end
        else
          method.call(*args, &block)
        end
      end
    end

    def self.no_fspec(ident, spec)
      S::Error.new("#{ident} not spec'ed", :method        => ident,
                                           :spec          => spec,
                                           :failure.ns(S) => :no_fspec)
    end

    private_class_method def self.instrument1(ident, opts)
      spec = S.get_spec(ident)

      raw, wrapped = @instrumented_methods.
        value.
        fetch(ident, H[]).
        values_at(:raw, :wrapped)

      current = ident.get_method
      to_wrap = wrapped == current ? raw : current

      ospec = instrument_choose_spec(spec, ident, opts[:spec])
      raise no_fspec(ident, spec) unless ospec

      ofn = instrument_choose_fn(to_wrap, ospec, ident, opts)

      checked = spec_checking_fn(ident, ofn, ospec)

      ident.redefine_method!(checked)

      wrapped = ident.get_method

      @instrumented_methods.swap do |methods|
        methods.merge(ident => H[:raw => to_wrap, :wrapped => wrapped])
      end

      ident
    end

    private_class_method def self.instrument_choose_fn(f, spec, ident, opts)
      stubs   = (opts[:stub] || []).map(&S.method(:Identifier))
      over    = opts[:gen] || {}
      replace = (opts[:replace] || {}).reduce({}) { |h, (k, v)| h.merge(S.Identifier(k) => v) }

      if stubs.include?(ident)
        Gen.generate(S.gen(spec, over))
      else
        replace.fetch(ident, f)
      end
    end

    private_class_method def self.instrument_choose_spec(spec, ident, overrides)
      (overrides || {}).
        reduce({}) { |h, (k, v)| h.merge(S.Identifier(k) => v) }.
        fetch(ident, spec)
    end

    private_class_method def self.unstrument1(ident)
      instrumented = @instrumented_methods.value[ident]
      return unless instrumented

      raw, wrapped = instrumented.fetch_values(:raw, :wrapped)

      @instrumented_methods.swap do |h|
        h.except(ident)
      end

      current = ident.get_method

      # Only redefine to original if it has not been modified since it was
      # instrumented.
      if wrapped == current
        ident.tap { |i| i.redefine_method!(raw) }
      end
    end

    # TODO: stacktrace-relevant-to-instrument

    # Given an opts hash as per instrument, returns the set of methods that can
    # be instrumented.
    def self.instrumentable_methods(opts = {})
      if opts[:gen]
        unless opts[:gen].keys.all? { |k| k.is_a?(Method) || k.is_a?(Symbol) }
          raise ArgumentError, "instrument :gen expects Method or Symbol keys"
        end
      end

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
      if opts[:gen]
        gens = opts[:gen].reduce({}) { |h, (k, v)| h.merge(S.Identifier(k) => v) }
        opts = opts.merge(:gen => gens)
      end

      Array(method_or_methods).
        map { |method| S.Identifier(method) }.
        uniq.
        map { |ident| instrument1(ident, opts) }.
        compact
    end

    # Undoes instrument on the method_or_methods, specified as in instrument.
    # With no args, unstruments all instrumented methods. Returns a collection
    # of Identifiers naming the methods unstrumented.
    def self.unstrument(method_or_methods = nil)
      method_or_methods ||= @instrumented_methods.value.keys

      Array(method_or_methods).
        map { |method| S.Identifier(method) }.
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

      S::Error.new("Specification-based check failed", data).tap do |e|
        e.set_backtrace(caller)
      end
    end

    # Returns true if call passes specs, otherwise returns a hash with
    # :backtrace, :cause and :data keys. :data will have a
    # :"Speculation/failure" key.
    private_class_method def self.check_call(method, spec, args, block)
      conformed_args = S.conform(spec.argspec, args) if spec.argspec

      if conformed_args == :invalid.ns(S)
        return explain_check(args, spec.argspec, args, :args)
      end

      conformed_block = S.conform(spec.blockspec, block) if spec.blockspec

      if conformed_block == :invalid.ns(S)
        return explain_check(block, spec.block, block, :block)
      end

      ret = method.call(*args, &block)

      conformed_ret = S.conform(spec.retspec, ret) if spec.retspec

      if conformed_ret == :invalid.ns(S)
        return explain_check(args, spec.retspec, ret, :ret)
      end

      return true unless spec.argspec && spec.retspec && spec.fnspec

      if S.valid?(spec.fnspec, :args => conformed_args, :block => conformed_block, :ret => conformed_ret)
        true
      else
        explain_check(args, spec.fnspec, { :args => conformed_args, :block => conformed_block, :ret => conformed_ret }, :fn)
      end
    end

    private_class_method def self.quick_check(method, spec, opts)
      gen = opts[:gen]
      num_tests = opts.fetch(:num_tests, 1000)

      args_gen = begin
                   S.gen(spec.argspec, gen)
                 rescue => e
                   return { :result => e }
                 end

      block_gen = if spec.blockspec
                    begin
                      S.gen(spec.blockspec, gen)
                    rescue => e
                      return { :result => e }
                    end
                  else
                    Utils.constantly(nil)
                  end

      combined_gen = ->(r) { [args_gen.call(r), block_gen.call(r)] }

      rantly_quick_check(combined_gen, num_tests) { |(args, block)| check_call(method, spec, args, block) }
    end

    private_class_method def self.make_check_result(method, spec, check_result)
      result = { :spec   => spec,
                 :ret.ns => check_result,
                 :method => method }

      if check_result[:result] && check_result[:result] != true
        result[:failure] = check_result[:result]
      end

      if check_result[:shrunk]
        result[:failure] = check_result[:shrunk][:result]
      end

      result
    end

    private_class_method def self.check1(ident, spec, opts)
      specd = S.spec(spec)

      reinstrument = unstrument(ident).any?
      method = ident.get_method

      if specd.argspec # or blockspec?
        check_result = quick_check(method, spec, opts)
        make_check_result(method, spec, check_result)
      else
        failure = { :info         => "No :args spec",
                    failure.ns(S) => :no_args_spec }

        { :failure => failure,
          :method  => method,
          :spec    => spec }
      end
    ensure
      instrument(ident) if reinstrument
    end

    private_class_method def self.validate_check_opts(opts)
      return unless opts[:gen]

      unless opts[:gen].keys.all? { |k| k.is_a?(Method) || k.is_a?(Symbol) }
        raise ArgumentErorr, "check :gen expects Method or Symbol keys"
      end
    end

    # Runs generative tests for method using spec and opts. See 'check' for options and return
    def self.check_method(method, spec, opts = {})
      validate_check_opts(opts)
      check1(S.Identifier(method), spec, opts)
    end

    # Given an opts hash as per `check`, returns the set of Identifiers that can be checked.
    def self.checkable_methods(opts = {})
      validate_check_opts(opts)

      S.
        registry.
        keys.
        select(&method(:fn_spec_name?)).
        reject(&:instance_method?).
        to_set.
        tap { |set| set.merge(opts[:spec].keys) if opts[:spec] }
    end

    # Run generative tests for spec conformance on method_or_methods. If
    # method_or_methods is not specified, check all checkable methods.
    #
    # The opts hash includes the following optional keys:
    #
    # :num_tests  number of times to generatively test each method (default: 1000)
    # :gen        hash map from spec names to generator overrides
    #
    # Generator overrides are passed to Speculation.gen when generating method args.
    #
    # Returns an array of check result hashes with the following keys:
    #
    # :spec       the spec tested
    # :method     optional method tested
    # :failure    optional test failure
    # :result     optional boolean as to whether all generative tests passed
    # :num_tests  optional number of generative tests ran
    #
    # :failure is a hash that will contain a :"Speculation/failure" key with possible values:
    #
    # :check_failed   at least one checked return did not conform
    # :no_args_spec   no :args spec provided
    # :no_fspec       no fspec provided
    # :no_gen         unable to generate :args
    # :instrument     invalid args detected by instrument
    def self.check(method_or_methods = nil, opts = {})
      method_or_methods ||= checkable_methods

      Array(method_or_methods).
        map { |method| S.Identifier(method) }.
        select { |ident| checkable_methods(opts).include?(ident) }.
        pmap { |ident| check1(ident, S.get_spec(ident), opts) }
    end

    # Reimplementation of Rantly's `check` since it does not provide direct access to results
    # (shrunk data etc.), instead printing them to STDOUT.
    def self.rantly_quick_check(gen, num_tests, &block)
      i = 0
      limit = 100

      Rantly.singleton.generate(num_tests, limit, gen) do |val|
        args, blk = val
        i += 1

        result = begin
                   block.call([args, blk])
                 rescue => e
                   e
                 end

        unless result == true
          # This is a Rantly Tuple.
          args = ::Tuple.new(args)

          if args.respond_to?(:shrink)
            shrunk = shrink(args, result, ->(v) { block.call([v, blk]) })

            shrunk[:smallest] = [shrunk[:smallest].array, blk]

            return { :fail      => args.array,
                     :block     => blk,
                     :num_tests => i,
                     :result    => result,
                     :shrunk    => shrunk }
          else
            return { :fail      => args.array,
                     :block     => blk,
                     :num_tests => i,
                     :result    => result }
          end
        end
      end

      { :num_tests => i,
        :result    => true }
    end

    # reimplementation of Rantly's shrinking.
    private_class_method def self.shrink(data, result, block, depth = 0, iteration = 0)
      smallest = data
      max_depth = depth

      if data.shrinkable?
        while iteration < 1024
          shrunk_data = data.shrink
          result = begin
                     block.call(shrunk_data.array)
                   rescue => e
                     e
                   end

          unless result == true
            shrunk = shrink(shrunk_data, result, block, depth + 1, iteration + 1)

            branch_smallest, branch_depth, iteration =
              shrunk.values_at(:smallest, :depth, :iteration)

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

    ### check reporting ###

    private_class_method def self.failure_type(x)
      x.data[:failure.ns(S)] if x.is_a?(S::Error)
    end

    private_class_method def self.unwrap_failure(x)
      failure_type(x) ? x.data : x
    end

    # Returns the type of the check result. This can be any of the
    # :"Speculation/failure" symbols documented in 'check', or:
    #
    # :check_passed   all checked method returns conformed
    # :check_raised   checked fn threw an exception
    private_class_method def self.result_type(ret)
      failure = ret[:failure]

      if failure.nil?
        :check_passed
      else
        failure_type(failure) || :check_raised
      end
    end

    # Given a check result, returns an abbreviated version suitable for summary
    # use.
    def self.abbrev_result(x)
      if x[:failure]
        x.reject { |k, _| k == :ret.ns }.
          merge(:spec    => x[:spec].inspect,
                :failure => unwrap_failure(x[:failure]))
      else
        x.reject { |k, _| [:spec, :ret.ns].include?(k) }
      end
    end

    # Given a collection of check_results, e.g. from `check`, pretty prints the
    # summary_result (default abbrev_result) of each.
    #
    # Returns a hash with :total, the total number of results, plus a key with a
    # count for each different :type of result.
    def self.summarize_results(check_results, &summary_result)
      summary_result ||= method(:abbrev_result)

      check_results.reduce(:total => 0) { |summary, result|
        pp summary_result.call(result)

        result_key = result_type(result)

        summary.merge(
          :total     => summary[:total].next,
          result_key => summary.fetch(result_key, 0).next
        )
      }
    end
  end
end
