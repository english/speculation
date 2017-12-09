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

    EMPTY_COLL = {
      Array      => [].freeze,
      Set        => Set[].freeze,
      Hash       => {}.freeze,
      Enumerator => [].to_enum.freeze
    }.freeze

    # FIXME: handle enumerators, ranges
    def self.empty(coll)
      if coll.is_a?(Class)
        EMPTY_COLL[coll]
      else
        EMPTY_COLL[coll.class]
      end
    end

    def self.into(to, from)
      from.reduce(to) { |memo, obj| conj(memo, obj) }
    end

    def self.conj(a, b)
      case a
      when Array, Set
        a + [b]
      when Enumerator
        Enumerator.new do |y|
          a.each do |x|
            y << x
          end
          y << b
        end
      when Hash
        case b
        when Array then a.merge(b[0] => b[1])
        else            a.merge(b)
        end
      else raise ArgumentError, "#{a.inspect}: must be an Array, Set or Hash"
      end
    end

    def self.sort_descending(coll)
      coll.sort { |a, b| yield(b) <=> yield(a) }
    end
  end
end
