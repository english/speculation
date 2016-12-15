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

    def self.conform(spec_name, value)
      spec = REGISTRY.value.fetch(spec_name)
      spec.conform(value)
    end

    def self.valid?(spec_name, value)
      spec = REGISTRY.value.fetch(spec_name)
      spec.conform(value) != :"Speculation::Core/invalid"
    end
  end
end
