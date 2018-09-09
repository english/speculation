module Speculation
  # Collection of predicate methods used within Speculation.
  # These may appear as the value for the `:pred` key in the return value of
  # `Speculation.explain_data`.
  module Predicates
    def self.hash?(x)
      x.respond_to?(:store) && x.respond_to?(:key?) && x.respond_to?(:[])
    end

    def self.array?(x)
      x.respond_to?(:at) && x.respond_to?(:[])
    end

    def self.enumerator?(xs)
      xs.is_a?(Enumerator)
    end

    def self.collection?(xs)
      xs.respond_to?(:each)
    end

    def self.nil_or_sequential?(xs)
      xs.nil? || sequential?(xs)
    end

    def self.sequential?(xs)
      array?(xs) || enumerator?(xs)
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

    def self.count_eq?(count, coll)
      coll.count == count
    end

    def self.count_between?(min_count, max_count, coll)
      coll.count.between?(min_count, max_count)
    end

    def self.key?(hash, key)
      hash.key?(key)
    end

    def self.empty?(coll)
      coll.empty?
    end

    def self.respond_to?(object, method)
      object.respond_to?(method)
    end
  end
end
