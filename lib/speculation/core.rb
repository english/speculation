require 'concurrent/atom'
require 'concurrent/delay'
require 'hamster/hash'

module Speculation
  module Core
    REGISTRY = Concurrent::Atom.new(Hamster::Hash[])

    class Spec
    end

    class PredicateSpec
      def initialize(predicate)
        @predicate = predicate
      end

      def conform(value)
        if @predicate.call(value)
          value
        else
          :"Speculation::Core/invalid"
        end
      end
    end

    class AndSpec < Spec
      def initialize(specs)
        @specs = specs
      end

      def conform(value)
        @specs.value.each do |spec|
          value = spec.conform(value)

          if Speculation::Core.invalid?(value)
            return :"Speculation::Core/invalid"
          end
        end

        value
      end
    end

    class OrSpec < Spec
      def initialize(keys, specs)
        @keys = keys
        @specs = specs
      end

      def conform(value)
        @specs.value.each_with_index do |spec, index|
          conformed = spec.conform(value)

          unless Speculation::Core.invalid?(conformed)
            return [@keys[index], value]
          end
        end

        :"Speculation::Core/invalid"
      end
    end

    def self.registry
      REGISTRY
    end

    def self.def(name, spec)
      unless spec.is_a?(Spec)
        # More cases here!
        spec = PredicateSpec.new(spec)
      end

      registry.swap { |reg| reg.put(name, spec) }

      name
    end

    def self.reset_registry!
      registry.swap { Hamster::Hash[] }
    end

    def self.conform(spec, value)
      spec = specize(spec)

      spec.conform(value)
    end

    def self.valid?(spec, value)
      spec = specize(spec)
      value = spec.conform(value)

      !invalid?(value)
    end

    def self.invalid?(value)
      value.equal?(:"Speculation::Core/invalid")
    end

    def self.and(*specs)
      delayed_specs = Concurrent::Delay.new do
        specs.map { |spec| specize(spec) }
      end

      AndSpec.new(delayed_specs)
    end

    def self.or(named_specs)
      keys = named_specs.keys

      delayed_specs = Concurrent::Delay.new do
        named_specs.values.map { |spec| specize(spec) }
      end

      OrSpec.new(keys, delayed_specs)
    end

    ### private ###

    def self.specize(spec)
      case spec
      when Speculation::Core::Spec then spec
      when Symbol then registry.value.fetch(spec)
      else
        if spec.respond_to?(:call)
          PredicateSpec.new(spec)
        else
          raise ArgumentError,
            "spec: #{spec} must be a Spec, Symbol or callable, given #{spec.class}"
        end
      end
    end
  end
end
