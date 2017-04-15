# frozen_string_literal: true

require "set"

module Speculation
  # @private
  module Utils
    def self.itself(x)
      x
    end

    def self.constantly(x)
      ->(*) { x }
    end

    def self.ident?(x)
      x.is_a?(Symbol) || x.is_a?(MethodIdentifier)
    end

    def self.method?(x)
      x.is_a?(Method) || x.is_a?(UnboundMethod)
    end

    def self.empty(coll)
      coll.class.new
    end

    def self.into(to, from)
      from.reduce(to) { |memo, obj| conj(memo, obj) }
    end

    def self.conj(a, b)
      case a
      when Array, Set
        a + [b]
      when Hash
        case b
        when Array then a.merge(b[0] => b[1])
        else            a.merge(b)
        end
      else raise ArgumentError, "#{a}: must be an Array, Set or Hash"
      end
    end
  end
end
