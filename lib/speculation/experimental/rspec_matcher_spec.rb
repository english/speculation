# frozen_string_literal: true

module RSpec::Matchers::BuiltIn
  class Eq
    def gen(overrides, path, rmap)
      ->(r) { @expected }
    end
  end

  class Compound
    class Or
      def gen(overrides, path, rmap)
        @id ||= SecureRandom.uuid

        gs = { :matcher1 => @matcher_1, :matcher2 => @matcher_2 }.
          map { |(k, p)|
          rmap = Speculation.inck(rmap, @id)

          unless Speculation.recur_limit?(rmap, @id, path, k)
            Gen.delay { Speculation.gensub(p, overrides, Speculation::Utils.conj(path, k), rmap) }
          end
        }.
        compact

        unless gs.empty?
          ->(rantly) { rantly.branch(*gs) }
        end
      end
    end

    class And
      def gen(overrides, path, rmap)
        S.gensub(@matcher_1, overrides, path, rmap)
      end
    end
  end

  class BeAKindOf
    def gen(overrides, path, rmap)
      Gen.gen_for_pred(@expected)
    end
  end

  class All
    def gen(overrides, path, rmap)
      if @matcher.respond_to?(:gen)
        gen = @matcher.gen(overrides, path, rmap)
        ->(rantly) do
          size = rantly.range(0, 20)
          rantly.array(size) do
            gen.call(rantly)
          end
        end
      end
    end

    def initialize_copy(other)
      @matcher = @matcher.clone
      @failed_objects = @failed_objects.clone
      super
    end
  end

  class Include
    def include_gen_helper(expected, overrides, path, rmap)
      init = Hash === expected ? {} : []
      iter = Hash === expected ? expected.each : expected.each_with_index.map { |v, k| [k, v] }
      iter.each_with_object(init) do |(k, v), gens|
        # update path
        if v.respond_to?(:gen)
          # gens[k] = S.gensub(spec, overrides, path, rmap) - blows up with no name
          gens[k] = v.gen(overrides, path, rmap)
        elsif S.rspec_matcher?(v) || S.spec?(v)
          raise Speculation::Error.new("unable to construct gen at: #{path.inspect} for: #{v.inspect}",
                                       :failure => :no_gen, :path => path)
          # hash? array?
        elsif v.is_a?(Hash) || v.is_a?(Array)
          gens[k] = include_gen_helper(v, overrides, path, rmap)
        else
          gens[k] = ->(r) { v }
        end
      end
    end

    def call_gens(gens, r)
      init = Hash === gens ? {} : []
      iter = Hash === gens ? gens.each : gens.each_with_index.map { |v, k| [k, v] }
      iter.each_with_object(init) { |(k, v), result|
        if Hash === v || Array === v
          result[k] = call_gens(v, r)
        else
          result[k] = v.call(r)
        end
      }
    end

    def gen(overrides, path, rmap)
      if expecteds.count == 1
        expected = expecteds.first
        expected = Array(expected) unless Hash === expected
        gens = include_gen_helper(expected, overrides, path, rmap)
        ->(r) do
          call_gens(gens, r)
        end
      else
        gens = include_gen_helper(expecteds, overrides, path, rmap)
        ->(r) do
          call_gens(gens, r)
        end
      end
    end
  end

  class Equal
    def gen(overrides, path, rmap)
      ->(r) { @expected }
    end
  end

  class BeComparedTo
    def gen(overrides, path, rmap)
      # what about ==, ===, =~
      Gen.gen_for_pred(@expected.class)
    end
  end
end

module Speculation
  module Experimental
    # @private
    class RSpecMatcherSpec < Spec
      def initialize(matcher, gen)
        @matcher = clone_matcher!(matcher)
        @gem = gen
        @lock = Mutex.new
      end

      def conform(value)
        @lock.synchronize do
          matcher = clone_matcher!(@matcher)

          if matcher.matches?(value)
            value
          else
            :"Speculation/invalid"
          end
        end
      end

      def unform(value)
        value
      end

      def explain(path, via, inn, value)
        @lock.synchronize do
          matcher = clone_matcher!(@matcher)

          if S.invalid?(S.dt(matcher, value))
            [{ :path => path, :val => value, :via => via, :in => inn, :pred => [matcher, [value]], :reason => matcher.description }]
          end
        end
      end

      def gen(overrides, path, rmap)
        return @gen if @gen

        if @matcher.respond_to?(:gen)
          @matcher.gen(overrides, path, rmap)
        end
      end

      def with_gen(gen)
        self.class.new(@matcher, gen)
      end

      def clone_matcher!(matcher)
        cloned = matcher.clone
        if cloned.respond_to?(:matcher)
          cloned.instance_variable_set(:@matcher, cloned.matcher.clone)
          cloned.instance_variable_set(:@failed_objects, {})
        end
        cloned
      end
    end
  end
end
