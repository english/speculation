module Speculation
  # Collection of predicate methods used within Speculation.
  # These may appear as the value `:pred for the pred key in the return value of
  # `Speculation.explain_data`.
  module Predicates
    def self.hash?(x)
      x.respond_to?(:store) && x.respond_to?(:key?) && x.respond_to?(:[])
    end

    def self.array?(x)
      x.respond_to?(:at) && x.respond_to?(:[])
    end

    def self.collection?(xs)
      xs.respond_to?(:each)
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
  end
end
