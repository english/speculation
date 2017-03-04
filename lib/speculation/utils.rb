# frozen_string_literal: true
require "set"

module Speculation
  # @private
  module Utils
    def self.hash?(x)
      x.respond_to?(:store)
    end

    def self.array?(x)
      x.respond_to?(:at)
    end

    def self.collection?(xs)
      xs.respond_to?(:each)
    end

    def self.itself(x)
      x
    end

    def self.constantly(x)
      ->(*) { x }
    end

    def self.complement(&f)
      ->(*args) { !f.call(*args) }
    end

    def self.distinct?(xs)
      seen = Set[]

      xs.each do |x|
        if seen.include?(x)
          return false
        else
          seen << x
        end
      end

      true
    end

    def self.ident?(x)
      x.is_a?(Symbol) || x.is_a?(Identifier)
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

    def self.count_eq?(coll, count)
      coll.count == count
    end

    def self.count_between?(coll, min_count, max_count)
      coll.count.between?(min_count, max_count)
    end

    def self.key?(hash, key)
      hash.key?(key)
    end

    def self.empty?(coll)
      coll.empty?
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
