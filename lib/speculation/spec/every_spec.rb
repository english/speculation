# frozen_string_literal: true

# This is a Ruby translation of clojure.spec:
#   https://github.com/clojure/clojure/blob/master/src/clj/clojure/spec.clj
# All credit belongs with Rich Hickey and contributors for their original work.

module Speculation
  # @private
  class EverySpec < Spec
    include NamespacedSymbols
    S = Speculation

    def initialize(predicate, options, gen = nil, name = nil)
      @predicate = predicate
      @options = options
      @gen = gen
      @name = name

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

      if options[:distinct]
        collection_predicates.push(->(coll) do
          coll.empty? || Predicates.distinct?(coll)
        end)
      end

      @collection_predicate = ->(coll) { collection_predicates.all? { |f| f.respond_to?(:call) ? f.call(coll) : f === coll } }
      @delayed_spec = Concurrent::Delay.new { S.send(:specize, predicate) }
      @kfn = options.fetch(:kfn, ->(i, _v) { i })
      @conform_keys, @conform_all, @kind, @conform_into, @gen_max, @distinct, @count, @min_count, @max_count =
        options.values_at(:conform_keys, :conform_all, :kind, :into, :gen_max, :distinct, :count, :min_count, :max_count)
      @gen_max ||= 20
      @gen_into = Utils.empty(@conform_into || @kind)

      # returns a tuple of [init add complete] fns
      @cfns = ->(x) do
        if Predicates.array?(x) && (!@conform_into || Predicates.array?(@conform_into))
          [->(init) { init.dup },
           ->(ret, i, v, cv) { v.equal?(cv) ? ret : ret.tap { |r| r[i] = cv } },
           Utils.method(:itself)]
        elsif Predicates.hash?(x) && ((@kind && !@conform_into) || Predicates.hash?(@conform_into))
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
      return :"Speculation/invalid" unless @collection_predicate.call(value)

      spec = @delayed_spec.value!

      if @conform_all
        init, add, complete = @cfns.call(value)

        return_value = init.call(value)

        value.each_with_index do |val, index|
          conformed_value = spec.conform(val)

          if S.invalid?(conformed_value)
            return :"Speculation/invalid"
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
          return :"Speculation/invalid" unless S.valid?(spec, item)
        end

        value
      end
    end

    def unform(value)
      if @conform_all
        spec = @delayed_spec.value!
        init, add, complete = @cfns.call(value)

        ret = value.
          each_with_index.
          reduce(init.call(value)) { |memo, (val, index)|
            add.call(memo, index, val, spec.unform(val))
          }

        complete.call(ret)
      else
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

      probs = probs.select(&Utils.method(:itself))
      probs = @conform_all ? probs : probs.take(S.coll_error_limit)

      probs.to_a
    end

    def with_gen(gen)
      self.class.new(@predicate, @options, gen, @name)
    end

    def with_name(name)
      self.class.new(@predicate, @options, @gen, name)
    end

    def gen(overrides, path, rmap)
      return @gen.call if @gen

      pgen = S.gensub(@predicate, overrides, path, rmap)

      init_gen = if @gen_into
                   Radagen.return(@gen_into)
                 elsif @kind
                   Radagen.fmap(S.gensub(@kind, overrides, path, rmap)) { |genned|
                     genned.empty? ? genned : Utils.empty(genned)
                   }
                 else
                   Radagen.return([])
                 end

      Radagen.bind(init_gen) { |init|
        gen = if @distinct
                if @count
                  Radagen.such_that(Radagen.array(pgen, :min => @count, :max => @count), 100) { |genned|
                    Predicates.distinct?(genned)
                  }
                else
                  min = @min_count || 0
                  max = @max_count || [@gen_max, 2 * min].max

                  Radagen.such_that(Radagen.array(pgen, :min => min, :max => max), 100) { |genned|
                    Predicates.distinct?(genned)
                  }
                end
              elsif @count
                Radagen.array(pgen, :min => @count, :max => @count)
              elsif @min_count || @max_count
                min = @min_count || 0
                max = @max_count || [@gen_max, 2 * min].max

                Radagen.array(pgen, :min => min, :max => max)
              else
                Radagen.array(pgen, :max => @gen_max)
              end

        Radagen.fmap(gen) { |genned|
          Array === init ? genned : Utils.into(init, genned)
        }
      }
    end

    private

    def collection_problems(x, kfn, distinct, count, min_count, max_count, path, via, inn)
      pred = kfn || Predicates.method(:collection?)

      unless S.pvalid?(pred, x)
        return S.explain1(pred, path, via, inn, x)
      end

      if count && !Predicates.count_eq?(count, x)
        return [{ :path => path, :pred => [Predicates.method(:count_eq?), [count, x]], :val => x, :via => via, :in => inn }]
      end

      if min_count || max_count
        min_count ||= 0
        max_count ||= Float::INFINITY
        unless Predicates.count_between?(min_count, max_count, x)
          return [{ :path => path, :pred => [Predicates.method(:count_between?), [min_count, max_count, x]], :val => x, :via => via, :in => inn }]
        end
      end

      if distinct && !x.empty? && !Predicates.distinct?(x)
        [{ :path => path, :pred => [Predicates.method(:distinct?), [x]], :val => x, :via => via, :in => inn }]
      end
    end
  end
end
