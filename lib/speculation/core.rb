require 'concurrent/atom'
require 'hamster/hash'

module Speculation
  module Core
    REGISTRY = Concurrent::Atom.new(Hamster::Hash[])

    class Spec
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

    def self.def(name, &block)
      spec = Spec.new(block)
      REGISTRY.swap { |reg| reg.put(name, spec) }
      name
    end

    def self.conform(spec, value)
      spec = specize(spec)
      spec.conform(value)
    end

    def self.valid?(spec, value)
      spec = specize(spec)
      !spec.conform(value).equal?(:"Speculation::Core/invalid")
    end

    def self.specize(spec)
      case spec
      when Speculation::Core::Spec then spec
      when Symbol then REGISTRY.value.fetch(spec)
      else
        if spec.respond_to?(:call)
          Spec.new(spec)
        else
          raise ArgumentError,
            "spec: #{spec} must be a Spec, Symbol or callable, given #{spec.class}"
        end
      end
    end
  end
end
