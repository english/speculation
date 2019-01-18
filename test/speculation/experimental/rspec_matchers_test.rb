# frozen_string_literal: true

require "test_helper"

module Speculation
  module Experimental
    class RSpecMatchersTest < Minitest::Test
      S = Speculation
      Gen = S::Gen
      include RSpec::Matchers

      def test_eq
        spec = eq(1)
        assert S.valid?(spec, 1)
        refute S.valid?(spec, 2)

        assert_equal(<<EOS, S.explain_str(spec, 3))
3 - failed: eq 1
EOS

        assert_equal 1, Gen.generate(S.gen(spec))

        # conform? unform?
      end

      def test_or
        spec = eq(:a) | (eq(:b))
        assert S.valid?(spec, :a)
        assert S.valid?(spec, :b)
        refute S.valid?(spec, :c)

        assert_includes [:a, :b], Gen.generate(S.gen(spec))
      end

      def test_be_a_kind_of
        spec = a_kind_of(Integer)
        assert S.valid?(spec, 1)
        assert S.valid?(spec, -1)
        refute S.valid?(spec, 1.0)
        assert_equal <<EOS, S.explain_str(spec, 1.0)
1.0 - failed: a kind of Integer
EOS

        assert_kind_of Integer, Gen.generate(S.gen(spec))
      end

      def test_predicates
        spec = be_positive
        refute S.valid?(spec, -1)
        refute S.valid?(spec, 0)
        assert S.valid?(spec, 1)

        assert_equal <<EOS, S.explain_str(spec, 0)
0 - failed: be positive
EOS
      end

      def test_all
        spec = all(a_kind_of(Integer))
        assert S.valid?(spec, [1, 2, 3])
        refute S.valid?(spec, {1 => 2})
        refute S.valid?(spec, [1, 2, 3.0])
        refute S.valid?(spec, nil)
        assert S.valid?(spec, [])
        assert_equal <<EOS, S.explain_str(spec, [1, 2, 3.0])
[1, 2, 3.0] - failed: all a kind of Integer
EOS

        Gen.generate(S.gen(spec)).each do |generated|
          assert_kind_of Integer, generated
        end
      end

      def test_contain_exactly
        spec = contain_exactly(a_kind_of(Integer), a_kind_of(String))
        S.explain(spec, [1, "2"])

        S.explain(spec, [1, 2])
      end

      def test_include_hash_simple
        spec = include(:foo => "bar")
        assert S.valid?(spec, :foo => "bar")
        assert_equal({:foo => "bar"}, Gen.generate(S.gen(spec)))
      end

      def test_include_hash_with_matcher
        spec = include(:foo => a_kind_of(String))
        assert S.valid?(spec, :foo => "bar")
        genned = Gen.generate(S.gen(spec))
        assert_equal [:foo], genned.keys
        assert_kind_of String, genned[:foo]
      end

      def test_include_hash_nested
        spec = include(:foo => { :bar => "baz" })
        assert S.valid?(spec, :foo => { :bar => "baz" })
        assert_equal({ :foo => { :bar => "baz" } }, Gen.generate(S.gen(spec)))
      end

      def test_include_hash_nested_with_matcher
        spec = include(:foo => { :bar => a_kind_of(String) })
        assert S.valid?(spec, :foo => { :bar => "baz" })
        genned = Gen.generate(S.gen(spec))
        assert_equal [:foo], genned.keys
        assert_equal [:bar], genned[:foo].keys
        assert_kind_of String, genned[:foo][:bar]
      end

      def test_include_array_simple
        spec = include('foo')
        assert S.valid?(spec, ["foo", "bar"])
        assert_equal(['foo'], Gen.generate(S.gen(spec)))
      end

      def test_include_array_with_matcher
        spec = include(a_kind_of(String))
        assert S.valid?(spec, ["bar"])
        genned = Gen.generate(S.gen(spec))
        assert_equal 1, genned.count
        assert_kind_of String, genned.first
      end

      def test_include_array_nested_with_matcher
        spec = include(a_kind_of(String), [a_kind_of(Integer)])
        assert S.valid?(spec, ["bar", [1]])
        genned = Gen.generate(S.gen(spec))
        assert_equal 2, genned.count
        assert_kind_of String, genned.first
        assert_kind_of Array, genned[1]
        assert_equal 1, genned[1].count
        assert_kind_of Integer, genned[1][0]
      end

      def test_be
        spec = be(1)
        assert S.valid?(spec, 1)
        genned = Gen.generate(S.gen(spec))
        assert_equal 1, genned

        spec = be > 1
        assert S.valid?(spec, 2)
        genned = Gen.generate(S.gen(spec))
        assert_kind_of Integer, genned
      end
    end
  end
end
