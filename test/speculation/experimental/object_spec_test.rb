# frozen_string_literal: true

require "test_helper"

module Speculation
  module Experimental
    class ObjectSpecTest < Minitest::Test
      S = Speculation
      Gen = S::Gen
      include S::NamespacedSymbols

      def test_conform
        foo_spec = S.fspec(:args => S.cat(:s => String), :ret => Integer)
        bar_spec = S.fspec(:args => S.cat, :ret => String)
        spec = S::Experimental.object_spec(:foo => foo_spec, :bar => bar_spec)

        my_object = Object.new

        def my_object.foo(x)
          x.to_i
        end

        refute S.valid?(spec, my_object)

        def my_object.bar
          :not_a_string
        end

        refute S.valid?(spec, my_object)

        def my_object.bar
          "a string"
        end

        assert S.valid?(spec, my_object)
        assert_equal my_object, S.conform(spec, my_object)
      end

      def test_explain
        foo_spec = S.fspec(:args => S.cat(:s => String), :ret => Integer)
        bar_spec = S.fspec(:args => S.cat, :ret => String)
        spec = S::Experimental.object_spec(:foo => foo_spec, :bar => bar_spec)

        my_object = Object.new

        def my_object.foo(x)
          x.to_i
        end

        explain_data = S.explain_data(spec, my_object)
        assert_equal({ :problems => [{ :path => [],
                                       :pred => [S::Predicates.method(:respond_to?), [my_object, :bar]],
                                       :val  => my_object,
                                       :via  => [],
                                       :in   => [] }],
                       :spec     => spec,
                       :value    => my_object }, explain_data)

        def my_object.bar
          :not_a_string
        end

        explain_data = S.explain_data(spec, my_object)
        assert_equal({ :problems => [{ :path => [:bar, :ret],
                                       :val  => :not_a_string,
                                       :via  => [],
                                       :in   => [:bar],
                                       :pred => [String, [:not_a_string]] }],
                       :spec     => spec,
                       :value    => my_object }, explain_data)

        def my_object.bar
          "a string"
        end

        assert_nil S.explain_data(spec, my_object)
      end

      def test_gen
        foo_spec = S.fspec(:args => S.cat(:s => String), :ret => Integer)
        bar_spec = S.fspec(:args => S.cat, :ret => String)
        spec = S::Experimental.object_spec(:foo => foo_spec, :bar => bar_spec)

        genned = Gen.generate(S.gen(spec))
        assert_kind_of Integer, genned.foo("test")
        assert_kind_of String, genned.bar

        assert_raises(RuntimeError) do
          genned.foo(1)
        end

        assert_raises(RuntimeError) do
          genned.bar("fail")
        end
      end
    end
  end
end
