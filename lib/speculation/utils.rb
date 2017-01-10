require 'set'

module Speculation
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

    def self.identity(x)
      x
    end

    def self.constantly(x)
      -> (*) { x }
    end

    def self.complement(&f)
      -> (*args) { !f.call(*args) }
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
  end
end
