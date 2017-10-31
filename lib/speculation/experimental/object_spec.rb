# frozen_string_literal: true

module Speculation
  # @private
  module Experimental
    class ObjectSpec < Spec
      # @private
      def self.assert_fspec_with_args!(method_name, spec)
        unless spec.respond_to?(:args) && Speculation.spec?(spec.args)
          raise ArgumentError, "method spec must be an fspec with :args spec defined - got #{spec} for #{method_name.inspect}"
        end
      end

      # @private
      def self.assert_symbol_names!(specs)
        specs.each_key do |method_name|
          unless method_name.is_a?(Symbol)
            raise ArgumentError, "method names must be Symbols, got #{method_name.inspect}"
          end
        end
      end

      # @private
      def self.specize_fspecs(specs)
        specs.each_with_object({}) do |(method_name, spec), h|
          spec = Speculation.send(:specize, spec)
          assert_fspec_with_args!(method_name, spec)
          h[method_name] = spec
        end
      end

      def initialize(specs, gen = nil)
        self.class.assert_symbol_names!(specs)

        @id = SecureRandom.uuid
        @gen = gen
        @specs = specs
        @delayed_specs = Concurrent::Delay.new { self.class.specize_fspecs(specs) }
      end

      def conform(value)
        @delayed_specs.value!.each do |method_name, fspec|
          unless value.respond_to?(method_name) && Speculation.valid?(fspec, value.method(method_name))
            return :"Speculation/invalid"
          end
        end

        value
      end

      def unform(value)
        value
      end

      def explain(path, via, inn, value)
        @delayed_specs.value!.each_with_object([]) do |(method_name, fspec), problems|
          if value.respond_to?(method_name)
            method = value.method(method_name)

            unless Speculation.valid?(fspec, method)
              problems.concat(Speculation.explain1(fspec, Utils.conj(path, method_name), via, Utils.conj(inn, method_name), method))
            end
          else
            pred = [Predicates.method(:respond_to?), [value, method_name]]
            problems << { :path => path, :pred => pred, :val => value, :via => via, :in => inn }
          end
        end
      end

      def gen(overrides, path, rmap)
        return @gen.call if @gen

        gens = @delayed_specs.value!.each_with_object({}) { |(method_name, fspec), h|
          h[method_name] = Speculation.gensub(fspec, overrides, Utils.conj(path, method_name), rmap)
        }

        ->(rantly) do
          gens.each_with_object(Object.new) do |(method_name, gen), obj|
            obj.define_singleton_method(method_name, gen.call(rantly))
          end
        end
      end

      def with_gen(gen)
        self.class.new(@specs, gen)
      end
    end
  end
end
