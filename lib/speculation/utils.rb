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
  end
end
