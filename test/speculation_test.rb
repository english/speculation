# frozen_string_literal: true
require "test_helper"

module Speculation
  class SpeculationTest < Minitest::Test
    S = Speculation
    STest = S::Test
    Gen = S::Gen
    Utils = S::Utils

    include Speculation::NamespacedSymbols

    def test_conform_with_existing_spec
      S.def(ns(:int?), ->(x) { x.is_a?(Integer) })

      assert_equal 2, S.conform(ns(:int?), 2)
      assert_equal :"Speculation/invalid", S.conform(ns(:int?), "two")

      assert S.valid?(ns(:int?), 2)
      refute S.valid?(ns(:int?), "two")
    end

    def test_def_requires_namespaced_symbol
      assert_raises(ArgumentError) do
        S.def("foo/integer", Integer)
      end

      assert_raises(ArgumentError) do
        S.def(:integer, Integer)
      end
    end

    def test_conform_with_predicate
      predicate = ->(x) { x.is_a?(Integer) }
      assert_equal 2, S.conform(predicate, 2)
      assert_equal :"Speculation/invalid", S.conform(predicate, "two")

      assert S.valid?(predicate, 2)
      refute S.valid?(predicate, "two")
    end

    def test_and_composition
      S.def(ns(:even?), ->(x) { x.even? })

      S.def(ns(:big_even), S.and(Integer, ns(:even?), ->(x) { x > 1000 }))

      assert_equal :"Speculation/invalid", S.conform(ns(:big_even), :foo)
      assert_equal :"Speculation/invalid", S.conform(ns(:big_even), 100)
      assert_equal 1_000_000, S.conform(ns(:big_even), 1_000_000)

      refute S.valid?(ns(:big_even), :foo)
      refute S.valid?(ns(:big_even), 10)
      assert S.valid?(ns(:big_even), 1_000_000)
    end

    def test_or_composition
      S.def(ns(:name_or_id), S.or(:name => String, :id => Integer))

      assert_equal :"Speculation/invalid", S.conform(ns(:name_or_id), :foo)
      assert_equal [:name, "abc"], S.conform(ns(:name_or_id), "abc")
      assert_equal [:id, 100], S.conform(ns(:name_or_id), 100)
    end

    def test_cat_sequence
      S.def(ns(:boolean), ->(x) { [true, false].include?(x) })
      S.def(ns(:ingredient), S.cat(:quantity => Numeric, :unit => Symbol))

      expected = { :quantity => 2, :unit => :teaspoon }
      assert_equal expected, S.conform(ns(:ingredient), [2, :teaspoon])

      S.def(ns(:config), S.cat(:prop => String, :val => S.alt(:s => String, :b => ns(:boolean))))

      assert_equal({ :prop => "-server", :val => [:s, "foo"] }, S.conform(ns(:config), ["-server", "foo"]))
    end

    def test_nested_cat_sequence
      S.def(ns(:nested), S.cat(:names_sym => ->(x) { x == :names },
                               :names     => S.spec(S.cat(:name1 => String, :name2 => String)),
                               :nums_sym  => ->(x) { x == :nums },
                               :nums      => S.spec(S.cat(:num1 => Numeric, :num2 => Numeric))))

      expected = { :names_sym => :names,
                   :nums_sym  => :nums,
                   :nums      => { :num1 => 1, :num2 => 2 },
                   :names     => { :name1 => "a", :name2 => "b" } }

      assert_equal expected, S.conform(ns(:nested), [:names, ["a", "b"], :nums, [1, 2]])
    end

    def test_zero_or_more
      S.def(ns(:seq_of_symbols), S.zero_or_more(Symbol))

      assert_equal [:a, :b, :c], S.conform(ns(:seq_of_symbols), [:a, :b, :c])
      assert_equal [], S.conform(ns(:seq_of_symbols), [])
      assert_equal :"Speculation/invalid", S.conform(ns(:seq_of_symbols), [1, 2, 3])
    end

    def test_nested_seq
      S.def(ns(:nested), S.cat(:names_sym => Set[:names],
                               :names     => S.spec(S.zero_or_more(String)),
                               :nums_sym  => Set[:nums],
                               :nums      => S.spec(S.zero_or_more(Numeric))))

      conformed = S.conform(ns(:nested), [:names, ["a", "b"], :nums, [1, 2]])

      expected = { :names_sym => :names, :names => ["a", "b"],
                   :nums_sym => :nums, :nums => [1, 2] }

      assert_equal expected, conformed
    end

    def test_non_nested
      S.def(ns(:unnested), S.cat(:names_sym => ->(x) { x == :names },
                                 :names     => S.zero_or_more(String),
                                 :nums_sym  => ->(x) { x == :nums },
                                 :nums      => S.zero_or_more(Numeric)))

      expected = { :names_sym => :names, :names => ["a", "b"],
                   :nums_sym => :nums, :nums => [1, 2, 3] }

      assert_equal expected, S.conform(ns(:unnested), [:names, "a", "b", :nums, 1, 2, 3])
    end

    def test_class_predicate
      S.def(ns(:seq_of_symbols), S.zero_or_more(Symbol))
      assert_equal [:foo, :bar], S.conform(ns(:seq_of_symbols), [:foo, :bar])

      S.def(ns(:seq_of_symbols), S.zero_or_more(Symbol))
      assert_equal [:foo, :bar], S.conform(ns(:seq_of_symbols), [:foo, :bar])
    end

    def test_one_or_more
      S.def(ns(:seq_of_symbols), S.one_or_more(Symbol))

      assert_equal [:a, :b, :c], S.conform(ns(:seq_of_symbols), [:a, :b, :c])
      assert_equal :"Speculation/invalid", S.conform(ns(:seq_of_symbols), [])
    end

    def test_zero_or_one
      S.def(ns(:odd), ->(x) { x.odd? })
      S.def(ns(:even), ->(x) { x.even? })

      S.def(ns(:maybe_odd), S.zero_or_one(ns(:odd)))

      assert_equal 1, S.conform(ns(:maybe_odd), [1])
      assert_nil S.conform(ns(:maybe_odd), [])
      assert_equal :"Speculation/invalid", S.conform(ns(:maybe_odd), [2])

      S.def(ns(:odds_then_maybe_even), S.cat(:odds => S.one_or_more(ns(:odd)),
                                             :even => S.zero_or_one(ns(:even))))

      expected = { :odds => [1, 3, 5], :even => 100 }
      assert_equal expected, S.conform(ns(:odds_then_maybe_even), [1, 3, 5, 100])
    end

    def test_alt_zero_or_more
      S.def(ns(:config),
            S.zero_or_more(S.cat(:prop => String,
                                 :val  => S.alt(:s => String,
                                                :b => ->(x) { [true, false].include?(x) }))))

      conformed = S.conform(ns(:config), ["-server", "foo", "-verbose", true, "-user", "joe"])
      expected = [{ :prop => "-server",  :val => [:s, "foo"] },
                  { :prop => "-verbose", :val => [:b, true] },
                  { :prop => "-user",    :val => [:s, "joe"] }]

      assert_equal expected, conformed
    end

    def test_constrained
      S.def(ns(:even_strings),
            S.constrained(S.zero_or_more(String), ->(x) { x.count.even? }))

      refute S.valid?(ns(:even_strings), ["a"])
      assert S.valid?(ns(:even_strings), ["a", "b"])
      refute S.valid?(ns(:even_strings), ["a", "b", "c"])
      assert S.valid?(ns(:even_strings), ["a", "b", "c", "d"])
    end

    def test_hash_keys
      email_regex = /^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,63}$/
      S.def(ns(:email_type), S.and(String, email_regex))

      S.def(ns(:acctid), Integer)
      S.def(ns(:first_name), String)
      S.def(ns(:last_name), String)
      S.def(ns(:email), ns(:email_type))

      S.def(ns(:person),
            S.keys(:req => [ns(:first_name), ns(:last_name), ns(:email)],
                   :opt => [ns(:phone)]))

      assert S.valid?(ns(:person), ns(:first_name) => "Elon",
                                   ns(:last_name)  => "Musk",
                                   ns(:email)      => "elon@example.com")

      # Fails required key check
      refute S.valid?(ns(:person), ns(:first_name) => "Elon")

      # Invalid value for key not specified in `req`
      refute S.valid?(ns(:person), ns(:first_name) => "Elon",
                                   ns(:last_name)  => "Musk",
                                   ns(:email)      => "elon@example.com",
                                   ns(:acctid)     => "123")

      # unqualified keys
      S.def(ns(:person_unq),
            S.keys(:req_un => [ns(:first_name), ns(:last_name), ns(:email)],
                   :opt_un => [ns(:phone)]))

      refute S.valid?(ns(:person_unq), {})

      refute S.valid?(ns(:person_unq), :first_name => "Elon",
                                       :last_name  => "Musk",
                                       :email      => "not-an-email")

      assert S.valid?(ns(:person_unq), :first_name => "Elon",
                                       :last_name  => "Musk",
                                       :email      => "elon@example.com")
    end

    def test_and_keys_or_keys
      spec = S.keys(:req => [ns(:x), ns(:y), S.or_keys(ns(:secret), S.and_keys(ns(:user), ns(:pwd)))])
      S.def(ns(:auth), spec)

      assert S.valid?(ns(:auth), ns(:x) => "foo", ns(:y) => "bar", ns(:secret) => "secret")
      assert S.valid?(ns(:auth), ns(:x) => "foo", ns(:y) => "bar", ns(:user) => "user", ns(:pwd) => "password")

      refute S.valid?(ns(:auth), ns(:x) => "foo", ns(:y) => "bar", ns(:secret) => "secret", ns(:user) => "user", ns(:pwd) => "password")
      refute S.valid?(ns(:auth), ns(:x) => "foo", ns(:y) => "bar", ns(:user) => "user")
      refute S.valid?(ns(:auth), ns(:x) => "foo", ns(:y) => "bar")
    end

    def test_merge
      S.def(:"animal/kind", String)
      S.def(:"animal/says", String)
      S.def(:"animal/common", S.keys(:req => [:"animal/kind", :"animal/says"]))
      S.def(:"dog/tail?", ns(S, :boolean))
      S.def(:"dog/breed", String)
      S.def(:"animal/dog", S.merge(:"animal/common", S.keys(:req => [:"dog/tail?", :"dog/breed"])))

      assert S.valid?(:"animal/dog",
                      :"animal/kind" => "dog",
                      :"animal/says" => "woof",
                      :"dog/tail?"   => true,
                      :"dog/breed"   => "retriever")

      S.explain_str(:"animal/dog",
                    :"animal/kind" => "dog",
                    :"dog/tail?"   => "why yes",
                    :"dog/breed"   => "retriever")
    end

    def test_coll_of
      S.def(ns(:symbol_collection), S.coll_of(Symbol))

      assert_equal [:a, :b, :c], S.conform(ns(:symbol_collection), [:a, :b, :c])
      assert_equal Set[5, 10, 2], S.conform(S.coll_of(Numeric), Set[5, 10, 2])

      expected = { :a => :x, :b => :y, :c => :z }
      assert_equal expected, S.conform(S.coll_of(ns(:symbol_collection)), :a => :x, :b => :y, :c => :z)

      assert S.valid?(S.coll_of(Integer), [1, 2, 3])
      assert S.valid?(S.coll_of(Integer, :kind => ->(coll) { coll.is_a?(Array) }), [1, 2, 3])
      refute S.valid?(S.coll_of(Integer), ["a", "b", "c"])

      assert S.valid?(S.coll_of(Integer, :count => 3), [1, 2, 3])
      refute S.valid?(S.coll_of(Integer, :count => 2), [1, 2, 3])

      refute S.valid?(S.coll_of(Integer, :min_count => 3, :max_count => 4), [1, 2])
      assert S.valid?(S.coll_of(Integer, :min_count => 3, :max_count => 4), [1, 2, 3])
      assert S.valid?(S.coll_of(Integer, :min_count => 3, :max_count => 4), [1, 2, 3, 4])
      refute S.valid?(S.coll_of(Integer, :min_count => 3, :max_count => 4), [1, 2, 3, 4, 5])

      assert_kind_of Set, S.conform(S.coll_of(Integer, :into => Set[]), [1, 2, 3, 4, 5])
      assert_kind_of Hash, S.conform(S.coll_of(S.coll_of(Integer), :into => {}), [[1, 2], [3, 4]])

      Gen.generate(S.gen(ns(:symbol_collection))).each do |x|
        assert_kind_of Symbol, x
      end

      coll = Gen.generate(S.gen(S.coll_of(Integer, :min_count => 3, :max_count => 4, :distinct => true, :into => Set[])))
      assert coll.count.between?(3, 4)
      assert Utils.distinct?(coll)
      coll.each do |x|
        assert_kind_of Integer, x
      end

      assert S.valid?(S.coll_of(Integer, :min_count => 3, :max_count => 4, :distinct => true, :kind => Set), Set[1, 2, 3])
      refute S.valid?(S.coll_of(Integer, :min_count => 3, :max_count => 4, :distinct => true, :kind => Array), Set[1, 2, 3])
      assert S.valid?(S.coll_of(Integer, :min_count => 3, :max_count => 4, :distinct => true, :kind => Enumerable), Set[1, 2, 3])

      assert_kind_of Array, Gen.generate(S.gen(S.coll_of(Integer, :min_count => 3, :max_count => 4, :distinct => true, :kind => Array)))
    end

    def test_tuple
      S.def(ns(:point), S.tuple(Integer, Integer, Integer))

      assert S.valid?(ns(:point), [1, 2, 3])
      refute S.valid?(ns(:point), [1, 2, "3"])

      expected = {
        :"Speculation/problems" => [
          { :path => [2], :val => 3.0, :via => [ns(:point)], :in => [2], :pred => [Integer, [3.0]] }
        ]
      }

      assert_equal expected, S.explain_data(ns(:point), [1, 2, 3.0])

      assert Gen.generate(S.gen(ns(:point))).all? { |x| x.is_a?(Integer) }
    end

    def test_hash_of
      S.def(ns(:scores), S.hash_of(String, Integer))

      expected = { "Sally" => 1000, "Joe" => 500 }
      assert_equal expected, S.conform(ns(:scores), "Sally" => 1000, "Joe" => 500)

      refute S.valid?(ns(:scores), "Sally" => true, "Joe" => 500)

      hash = Gen.generate(S.gen(ns(:scores)))

      hash.keys.each do |key|
        assert_kind_of String, key
      end
      hash.values.each { |value| assert_kind_of Integer, value }
    end

    def test_conformer
      S.def(ns(:wont_conform_keys), S.hash_of(S.and(Symbol, S.conformer(&:to_s)),
                                              S.and(Float, S.conformer(&:to_i))))

      assert_equal({ :foo => 1, :bar => 2 },
                   S.conform(ns(:wont_conform_keys), :foo => 1.0, :bar => 2.0))

      S.def(ns(:will_conform_keys), S.hash_of(S.and(Symbol, S.conformer(&:to_s)),
                                              S.and(Float, S.conformer(&:to_i)),
                                              :conform_keys => true))

      assert_equal({ "foo" => 1, "bar" => 2 },
                   S.conform(ns(:will_conform_keys), :foo => 1.0, :bar => 2.0))
    end

    def test_explain_data
      S.def(ns(:even), ->(x) { x.even? })

      ed = S.explain_data(ns(:even), 1)
      problems = ed.fetch(ns(S, :problems))

      assert_equal 1, problems.count

      problem = problems.first
      assert_equal [], problem[:path]
      assert_equal 1, problem[:val]
      assert_equal [ns(:even)], problem[:via]
      assert_equal [], problem[:in]
      assert_kind_of Proc, problem[:pred].first
      assert_equal [1], problem[:pred].last

      S.def(ns(:integer), Integer)
      S.def(ns(:even), ->(x) { x.even? })
      S.def(ns(:even_integer), S.and(ns(:integer), ns(:even)))

      ed = S.explain_data(ns(:even_integer), "s")
      problems = ed.fetch(ns(S, :problems))

      assert_equal 1, problems.count

      problem = problems.first
      assert_equal [], problem[:path]
      assert_equal "s", problem[:val]
      assert_equal [ns(:even_integer), ns(:integer)], problem[:via]
      assert_equal [], problem[:in]
      assert_equal [Integer, ["s"]], problem[:pred]
    end

    def test_explain_data_map
      email_regex = /^[a-zA-Z1-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,63}$/
      S.def(ns(:email_type), S.and(String, email_regex))

      S.def(ns(:acctid), Integer)
      S.def(ns(:first_name), String)
      S.def(ns(:last_name), String)
      S.def(ns(:email), ns(:email_type))
      S.def(ns(:person),
            S.keys(:req => [ns(:first_name), ns(:last_name), ns(:email)],
                   :opt => [ns(:phone)]))

      input = {
        ns(:first_name) => "Elon",
        ns(:last_name)  => "Musk",
        ns(:email)      => "n/a"
      }

      expected = {
        :"Speculation/problems" => [
          {
            :path => [ns(:email)],
            :val  => "n/a",
            :in   => [ns(:email)],
            :via  => [
              ns(:person),
              ns(:email_type)
            ],
            :pred => [/^[a-zA-Z1-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,63}$/, ["n/a"]]
          }
        ]
      }

      assert_equal expected, S.explain_data(ns(:person), input)
    end

    def test_explain_or
      S.def(ns(:name_or_id), S.or(:name => String, :id => Integer))

      expected = {
        :"Speculation/problems" => [
          { :path => [:name], :val => :foo, :in => [], :via => [ns(:name_or_id)], :pred => [String, [:foo]] },
          { :path => [:id], :val => :foo, :in => [], :via => [ns(:name_or_id)], :pred => [Integer, [:foo]] }
        ]
      }

      assert_equal expected, S.explain_data(ns(:name_or_id), :foo)
      assert_nil S.explain_data(ns(:name_or_id), 1)
    end

    def test_explain_regex
      S.def(ns(:ingredient), S.cat(:quantity => Numeric, :unit => Symbol))

      ed = S.explain_data(ns(:ingredient), [11, "peaches"])
      problems = ed.fetch(ns(S, :problems))

      assert_equal 1, problems.count

      problem = problems.first
      assert_equal [:unit], problem[:path]
      assert_equal "peaches", problem[:val]
      assert_equal [ns(:ingredient)], problem[:via]
      assert_equal [1], problem[:in]
      assert_equal [Symbol, ["peaches"]], problem[:pred]

      even_count = ->(nums) { nums.count.even? }

      S.def(ns(:nested), S.cat(:names_sym => ->(x) { x == :names },
                               :names     => S.spec(S.zero_or_more(String)),
                               :nums_sym  => ->(x) { x == :nums },
                               :nums      => S.spec(S.constrained(S.one_or_more(Numeric), even_count))))

      ed = S.explain_data(ns(:nested), [:names, ["a", "b"], :nums, [1, 2, 3, 4, 5]])
      problems = ed.fetch(ns(S, :problems))

      assert_equal 1, problems.count

      problem = problems.first
      assert_equal [:nums], problem[:path]
      assert_equal [1, 2, 3, 4, 5], problem[:val]
      assert_equal [3], problem[:in]
      assert_equal [ns(:nested)], problem[:via]
      assert_equal [even_count, [[1, 2, 3, 4, 5]]], problem[:pred]
    end

    def test_explain_hash_of
      S.def(ns(:scores), S.hash_of(String, Integer))

      expected = { :"Speculation/problems" => [{ :path => [1],
                                                 :val  => "300",
                                                 :via  => [ns(:scores)],
                                                 :in   => ["Joe", 1],
                                                 :pred => [Integer, ["300"]] }] }

      assert_equal expected, S.explain_data(ns(:scores), "Sally" => 1000, "Joe" => "300")
    end

    def test_explain_alt
      S.def(ns(:nested), S.cat(:names_sym => ->(x) { x == :names },
                               :names     => S.spec(S.zero_or_more(String)),
                               :nums_sym  => ->(x) { x == :nums },
                               :nums      => S.spec(S.alt(:ints   => S.one_or_more(Integer),
                                                          :floats => S.one_or_more(Float)))))

      expected = {
        :"Speculation/problems" => [
          {
            :path => [:nums, :ints],
            :val  => "1",
            :via  => [ns(:nested)],
            :in   => [3, 0],
            :pred => [Integer, ["1"]]
          },
          {
            :path => [:nums, :floats],
            :val  => "1",
            :via  => [ns(:nested)],
            :in   => [3, 0],
            :pred => [Float, ["1"]]
          }
        ]
      }

      assert_equal expected, S.explain_data(ns(:nested), [:names, ["a", "b"], :nums, ["1"]])
    end

    def test_explain
      S.def(ns(:unq, :person),
            S.keys(:req_un => [ns(self.class, :first_name), ns(self.class, :last_name), ns(self.class, :email)],
                   :opt_un => [ns(self.class, :phone)]))

      assert_equal <<-EOS, S.explain_str(ns(:unq, :person), :first_name => "Elon")
val: {:first_name=>"Elon"} fails spec: :"unq/person" predicate: [#{Utils.method(:key?)}, [:"Speculation::SpeculationTest/last_name"]]
val: {:first_name=>"Elon"} fails spec: :"unq/person" predicate: [#{Utils.method(:key?)}, [:"Speculation::SpeculationTest/email"]]
      EOS

      email_regex = /^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,63}$/
      S.def(ns(self.class, :email), S.and(String, email_regex))

      assert_equal <<-EOS, S.explain_str(ns(:unq, :person), :first_name => "Elon", :last_name => "Musk", :email => "elon")
In: [:email] val: "elon" fails spec: :"Speculation::SpeculationTest/email" at: [:email] predicate: [/^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,63}$/, ["elon"]]
      EOS
    end

    def test_explain_and_keys_or_keys
      S.def(ns(:unq, :person),
            S.keys(:req_un => [S.or_keys(S.and_keys(ns(self.class, :first_name), ns(self.class, :last_name)), ns(self.class, :email))],
                   :opt_un => [ns(self.class, :phone)]))

      assert_equal <<-EOS, S.explain_str(ns(:unq, :person), :first_name => "Elon")
val: {:first_name=>"Elon"} fails spec: :"unq/person" predicate: [#{Utils.method(:key?)}, ["(Speculation::SpeculationTest/first_name and Speculation::SpeculationTest/last_name) or Speculation::SpeculationTest/email"]]
      EOS
    end

    def test_explain_data_keys
      S.def(ns(:foo), String)
      S.def(ns(:bar), Integer)
      S.def(ns(:baz), String)

      S.def(ns(:hash), S.keys(:req_un => [ns(:foo), ns(:bar), ns(:baz)]))

      expected = { :"Speculation/problems" => [{ :path => [],
                                                 :pred => [Utils.method(:key?), [ns(:bar)]],
                                                 :val  => { :foo => "bar", :baz => "baz" },
                                                 :via  => [ns(:hash)],
                                                 :in   => [] }] }

      assert_equal expected, S.explain_data(ns(:hash), :foo => "bar", :baz => "baz")
    end

    def test_nilable
      S.def(ns(:maybe_string), S.nilable(String))

      assert S.valid?(ns(:maybe_string), "foo")
      assert S.valid?(ns(:maybe_string), nil)
      refute S.valid?(ns(:maybe_string), 1)

      ed = S.explain_data(ns(:maybe_string), 1)
      expected = [{ :path => [:"Speculation/pred"],
                    :val  => 1,
                    :in   => [],
                    :via  => [ns(:maybe_string)],
                    :pred => [String, [1]] },
                  { :path => [:"Speculation/nil"],
                    :val  => 1,
                    :in   => [],
                    :via  => [ns(:maybe_string)],
                    :pred => [NilClass, [1]] }]

      assert_equal expected, ed.fetch(ns(Speculation, :problems))

      val = Gen.generate(S.gen(ns(:maybe_string)))
      assert val.is_a?(String) || val.nil?
    end

    def test_set_predicate
      S.def(ns(self.class, :suit), Set[:club, :diamond, :heart, :spade])

      assert S.valid?(ns(self.class, :suit), :club)
      assert S.valid?(ns(self.class, :suit), :heart)
      refute S.valid?(ns(self.class, :suit), :lung)

      ed = S.explain_data(ns(self.class, :suit), 1)
      expected = [{ :path => [],
                    :val  => 1,
                    :via  => [:"Speculation::SpeculationTest/suit"],
                    :in   => [],
                    :pred => [Set[:club, :diamond, :heart, :spade], [1]] }]

      assert_equal expected, ed.fetch(ns(Speculation, :problems))

      val = Gen.generate(S.gen(ns(self.class, :suit)))
      assert [:club, :diamond, :heart, :spade].include?(val)
    end

    def test_fspec
      mod = Module.new do
        def self.foo(x)
          x + 1
        end
      end

      S.fdef(mod.method(:foo),
             :args => S.cat(:x => Integer),
             :ret  => Integer)

      assert S.valid?(mod.method(:foo), :next.to_proc)
      refute S.valid?(mod.method(:foo), :to_s.to_proc)
      assert S.valid?(mod.method(:foo), mod.method(:foo))
      refute S.valid?(mod.method(:foo), "not-a-method")

      Gen.generate(S.gen(mod.method(:foo)))

      identifier = S.send(:Identifier, mod.method(:foo))
      expected = { :path => [:ret], :via => [identifier], :in => [], :pred => [Integer, ["0"]] }

      ed = S.explain_data(mod.method(:foo), :to_s.to_proc)
      assert_equal expected, ed.fetch(ns(S, :problems)).first.reject { |k, _v| k == :val }
      assert_kind_of String, ed.fetch(ns(S, :problems)).first[:val]

      S.def(ns(self.class, :foo), S.fspec(:args => S.cat(:x => String), :ret => Integer))

      trigger_no_method_error = :trigger_no_method_error.to_proc
      ed = S.explain_data(ns(self.class, :foo), trigger_no_method_error).fetch(ns(S, :problems)).first
      assert_equal [], ed[:path]
      assert_equal [trigger_no_method_error, [""]], ed[:pred]
      assert_equal [ns(self.class, :foo)], ed[:via]
      assert_equal [], ed[:in]
      assert_match(/undefined method `trigger_no_method_error' for .*String/, ed[:reason])
    end

    def test_fspec_block
      mod = Module.new do
        def self.foo(&block)
          block.call(1)
        end
      end

      S.fdef(mod.method(:foo),
             :args  => ns(S, :empty),
             :block => S.fspec(:args => S.cat(:x => Integer),
                               :ret  => Integer),
             :ret   => Integer)

      assert S.valid?(mod.method(:foo), mod.method(:foo))
      refute S.valid?(mod.method(:foo), :to_s.to_proc)
      refute S.valid?(mod.method(:foo), "not-a-method")

      val = Gen.generate(S.gen(mod.method(:foo)))
      assert_kind_of Integer, val.call { |_x| 1 }

      identifier = S.Identifier(mod.method(:foo))
      f = ->(&b) { b.call("1") }
      ed = S.explain_data(mod.method(:foo), f)
      ed = ed.fetch(ns(S, :problems)).first

      assert_equal [], ed[:path]
      func, args, block = ed[:pred]

      assert_equal f, func
      assert_equal [], args
      assert_kind_of Proc, block

      assert_equal [], ed[:val][:args]
      assert_kind_of Proc, ed[:val][:block]
      assert_equal 'In: [0] val: "1" fails at: [:x] predicate: [Integer, ["1"]]', ed[:reason]
      assert_equal [identifier], ed[:via]
      assert_equal [], ed[:in]
    end
  end
end
