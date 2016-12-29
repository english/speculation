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
  end
end
