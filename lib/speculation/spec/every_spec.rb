# frozen_string_literal: true

# This is a Ruby translation of clojure.spec:
#   https://github.com/clojure/clojure/blob/master/src/clj/clojure/spec.clj
# All credit belongs with Rich Hickey and contributors for their original work.

module Speculation
  # @private
  class EverySpec < Spec
    include NamespacedSymbols
    S = Speculation

    def initialize(predicate, options, gen = nil)
      @predicate = predicate
      @options = options
      @gen = gen

      collection_predicates = [options.fetch(:kind, Enumerable)]

      if options.key?(:count)
        collection_predicates.push(->(coll) { coll.count == options[:count] })
      elsif options.key?(:min_count) || options.key?(:max_count)
        collection_predicates.push(->(coll) do
          min = options.fetch(:min_count, 0)
          max = options.fetch(:max_count, Float::INFINITY)

          coll.count.between?(min, max)
        end)
      end

      @collection_predicate = ->(coll) { collection_predicates.all? { |f| f.respond_to?(:call) ? f.call(coll) : f === coll } }
      @delayed_spec = Concurrent::Delay.new { S.send(:specize, predicate) }
      @kfn = options.fetch(ns(S, :kfn), ->(i, _v) { i })
      @conform_keys, @conform_all, @kind, @gen_into, @gen_max, @distinct, @count, @min_count, @max_count =
        options.values_at(:conform_keys, ns(S, :conform_all), :kind, :into, :gen_max, :distinct, :count, :min_count, :max_count)
      @gen_max ||= 20
      @conform_into = @gen_into

      # returns a tuple of [init add complete] fns
      @cfns = ->(x) do
        if Utils.array?(x) && (!@conform_into || Utils.array?(@conform_into))
          [Utils.method(:itself),
           ->(ret, i, v, cv) { v.equal?(cv) ? ret : ret.tap { |r| r[i] = cv } },
           Utils.method(:itself)]
        elsif Utils.hash?(x) && ((@kind && !@conform_into) || Utils.hash?(@conform_into))
          [@conform_keys ? Utils.method(:empty) : Utils.method(:itself),
           ->(ret, _i, v, cv) {
             if v.equal?(cv) && !@conform_keys
               ret
             else
               ret.merge((@conform_keys ? cv : v).first => cv.last)
             end
           },
           Utils.method(:itself)]
        else
          [->(init) { Utils.empty(@conform_into || init) },
           ->(ret, _i, _v, cv) { Utils.conj(ret, cv) },
           Utils.method(:itself)]
        end
      end
    end

    def conform(value)
      return S::INVALID unless @collection_predicate.call(value)

      spec = @delayed_spec.value!

      if @conform_all
        init, add, complete = @cfns.call(value)

        return_value = init.call(value)

        value.each_with_index do |val, index|
          conformed_value = spec.conform(val)

          if S.invalid?(conformed_value)
            return S::INVALID
          else
            return_value = add.call(return_value, index, val, conformed_value)
          end
        end

        complete.call(return_value)
      else
        # OPTIMIZE: check if value is indexed (array, hash etc.) vs not indexed (list, custom enumerable)
        limit = S.coll_check_limit

        value.each_with_index do |item, index|
          return value if index == limit
          return S::INVALID unless S.valid?(spec, item)
        end

        value
      end
    end

    def explain(path, via, inn, value)
      probs = collection_problems(value, @kind, @distinct, @count, @min_count, @max_count, path, via, inn)
      return probs if probs

      spec = @delayed_spec.value!

      probs = value.lazy.each_with_index.flat_map { |v, i|
        k = @kfn.call(i, v)

        unless S.valid?(spec, v)
          S.explain1(@predicate, path, via, Utils.conj(inn, k), v)
        end
      }

      probs = @conform_all ? probs.to_a : probs.take(S.coll_error_limit)
      probs.compact
    end

    def with_gen(gen)
      self.class.new(@predicate, @options, gen)
    end

    def gen(overrides, path, rmap)
      return @gen if @gen

      pgen = S.gensub(@predicate, overrides, path, rmap)

      ->(rantly) do
        init = if @gen_into
                 Utils.empty(@gen_into)
               elsif @kind
                 Utils.empty(S.gensub(@kind, overrides, path, rmap).call(rantly))
               else
                 []
               end

        val = if @distinct
                if @count
                  rantly.array(@count, &pgen).tap { |arr| rantly.guard(Utils.distinct?(arr)) }
                else
                  min = @min_count || 0
                  max = @max_count || [@gen_max, 2 * min].max
                  count = rantly.range(min, max)

                  rantly.array(count, &pgen).tap { |arr| rantly.guard(Utils.distinct?(arr)) }
                end
              elsif @count
                rantly.array(@count, &pgen)
              elsif @min_count || @max_count
                min = @min_count || 0
                max = @max_count || [@gen_max, 2 * min].max
                count = rantly.range(min, max)

                rantly.array(count, &pgen)
              else
                count = rantly.range(0, @gen_max)
                rantly.array(count, &pgen)
              end

        Utils.into(init, val)
      end
    end

    private

    def collection_problems(x, kfn, distinct, count, min_count, max_count, path, via, inn)
      pred = kfn || Utils.method(:collection?)

      unless S.pvalid?(pred, x)
        return S.explain1(pred, path, via, inn, x)
      end

      if count && !Utils.count_eq?(x, count)
        return [{ :path => path, :pred => [Utils.method(:count_eq?), [x, count]], :val => x, :via => via, :in => inn }]
      end

      if min_count || max_count
        min_count ||= 0
        max_count ||= Float::INFINITY
        unless Utils.count_between?(x, min_count, max_count)
          return [{ :path => path, :pred => [Utils.method(:count_between?), [x, min_count, max_count]], :val => x, :via => via, :in => inn }]
        end
      end

      if distinct && !x.empty? && !Utils.distinct?(x)
        [{ :path => path, :pred => [Utils.method(:distinct?), [x]], :val => x, :via => via, :in => inn }]
      end
    end
  end
end
