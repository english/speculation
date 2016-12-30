require 'speculation/core'

module Speculation
  using namespaced_symbols(self)

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
  end
end
