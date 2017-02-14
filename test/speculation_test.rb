# frozen_string_literal: true
require "test_helper"

module Speculation
  class SpeculationTest < Minitest::Test
    S = Speculation
    STest = S::Test
    Gen = S::Gen
    Utils = S::Utils

    using S::NamespacedSymbols.refine(self)

    def test_conform_with_existing_spec
      S.def(:int?.ns, ->(x) { x.is_a?(Integer) })

      assert_equal 2, S.conform(:int?.ns, 2)
      assert_equal :"Speculation/invalid", S.conform(:int?.ns, "two")

      assert S.valid?(:int?.ns, 2)
      refute S.valid?(:int?.ns, "two")
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
      S.def(:even?.ns, ->(x) { x.even? })

      S.def(:big_even.ns, S.and(Integer, :even?.ns, ->(x) { x > 1000 }))

      assert_equal :"Speculation/invalid", S.conform(:big_even.ns, :foo)
      assert_equal :"Speculation/invalid", S.conform(:big_even.ns, 100)
      assert_equal 1_000_000, S.conform(:big_even.ns, 1_000_000)

      refute S.valid?(:big_even.ns, :foo)
      refute S.valid?(:big_even.ns, 10)
      assert S.valid?(:big_even.ns, 1_000_000)
    end

    def test_or_composition
      S.def(:name_or_id.ns, S.or(:name => String, :id => Integer))

      assert_equal :"Speculation/invalid", S.conform(:name_or_id.ns, :foo)
      assert_equal [:name, "abc"], S.conform(:name_or_id.ns, "abc")
      assert_equal [:id, 100], S.conform(:name_or_id.ns, 100)
    end

    def test_cat_sequence
      S.def(:boolean.ns, ->(x) { [true, false].include?(x) })
      S.def(:ingredient.ns, S.cat(:quantity => Numeric, :unit => Symbol))

      expected = { :quantity => 2, :unit => :teaspoon }
      assert_equal expected, S.conform(:ingredient.ns, [2, :teaspoon])

      S.def(:config.ns, S.cat(:prop => String, :val => S.alt(:s => String, :b => :boolean.ns)))

      assert_equal({ :prop => "-server", :val => [:s, "foo"] }, S.conform(:config.ns, ["-server", "foo"]))
    end

    def test_nested_cat_sequence
      S.def(:nested.ns, S.cat(:names_sym => ->(x) { x == :names },
                              :names     => S.spec(S.cat(:name1 => String, :name2 => String)),
                              :nums_sym  => ->(x) { x == :nums },
                              :nums      => S.spec(S.cat(:num1 => Numeric, :num2 => Numeric))))

      expected = { :names_sym => :names,
                   :nums_sym  => :nums,
                   :nums      => { :num1 => 1, :num2 => 2 },
                   :names     => { :name1 => "a", :name2 => "b" } }

      assert_equal expected, S.conform(:nested.ns, [:names, ["a", "b"], :nums, [1, 2]])
    end

    def test_zero_or_more
      S.def(:seq_of_symbols.ns, S.zero_or_more(Symbol))

      assert_equal [:a, :b, :c], S.conform(:seq_of_symbols.ns, [:a, :b, :c])
      assert_equal [], S.conform(:seq_of_symbols.ns, [])
      assert_equal :"Speculation/invalid", S.conform(:seq_of_symbols.ns, [1, 2, 3])
    end

    def test_nested_seq
      S.def(:nested.ns, S.cat(:names_sym => Set[:names],
                              :names     => S.spec(S.zero_or_more(String)),
                              :nums_sym  => Set[:nums],
                              :nums      => S.spec(S.zero_or_more(Numeric))))

      conformed = S.conform(:nested.ns, [:names, ["a", "b"], :nums, [1, 2]])

      expected = { :names_sym => :names, :names => ["a", "b"],
                   :nums_sym => :nums, :nums => [1, 2] }

      assert_equal expected, conformed
    end

    def test_non_nested
      S.def(:unnested.ns, S.cat(:names_sym => ->(x) { x == :names },
                                :names     => S.zero_or_more(String),
                                :nums_sym  => ->(x) { x == :nums },
                                :nums      => S.zero_or_more(Numeric)))

      expected = { :names_sym => :names, :names => ["a", "b"],
                   :nums_sym => :nums, :nums => [1, 2, 3] }

      assert_equal expected, S.conform(:unnested.ns, [:names, "a", "b", :nums, 1, 2, 3])
    end

    def test_class_predicate
      S.def(:seq_of_symbols.ns, S.zero_or_more(Symbol))
      assert_equal [:foo, :bar], S.conform(:seq_of_symbols.ns, [:foo, :bar])

      S.def(:seq_of_symbols.ns, S.zero_or_more(Symbol))
      assert_equal [:foo, :bar], S.conform(:seq_of_symbols.ns, [:foo, :bar])
    end

    def test_one_or_more
      S.def(:seq_of_symbols.ns, S.one_or_more(Symbol))

      assert_equal [:a, :b, :c], S.conform(:seq_of_symbols.ns, [:a, :b, :c])
      assert_equal :"Speculation/invalid", S.conform(:seq_of_symbols.ns, [])
    end

    def test_zero_or_one
      S.def(:odd.ns, ->(x) { x.odd? })
      S.def(:even.ns, ->(x) { x.even? })

      S.def(:maybe_odd.ns, S.zero_or_one(:odd.ns))

      assert_equal 1, S.conform(:maybe_odd.ns, [1])
      assert_nil S.conform(:maybe_odd.ns, [])
      assert_equal :"Speculation/invalid", S.conform(:maybe_odd.ns, [2])

      S.def(:odds_then_maybe_even.ns, S.cat(:odds => S.one_or_more(:odd.ns),
                                            :even => S.zero_or_one(:even.ns)))

      expected = { :odds => [1, 3, 5], :even => 100 }
      assert_equal expected, S.conform(:odds_then_maybe_even.ns, [1, 3, 5, 100])
    end

    def test_alt_zero_or_more
      S.def(:config.ns,
            S.zero_or_more(S.cat(:prop => String,
                                 :val  => S.alt(:s => String,
                                                :b => ->(x) { [true, false].include?(x) }))))

      conformed = S.conform(:config.ns, ["-server", "foo", "-verbose", true, "-user", "joe"])
      expected = [{ :prop => "-server",  :val => [:s, "foo"] },
                  { :prop => "-verbose", :val => [:b, true] },
                  { :prop => "-user",    :val => [:s, "joe"] }]

      assert_equal expected, conformed
    end

    def test_constrained
      S.def(:even_strings.ns,
            S.constrained(S.zero_or_more(String), ->(x) { x.count.even? }))

      refute S.valid?(:even_strings.ns, ["a"])
      assert S.valid?(:even_strings.ns, ["a", "b"])
      refute S.valid?(:even_strings.ns, ["a", "b", "c"])
      assert S.valid?(:even_strings.ns, ["a", "b", "c", "d"])
    end

    def test_hash_keys
      email_regex = /^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,63}$/
      S.def(:email_type.ns, S.and(String, email_regex))

      S.def(:acctid.ns, Integer)
      S.def(:first_name.ns, String)
      S.def(:last_name.ns, String)
      S.def(:email.ns, :email_type.ns)

      S.def(:person.ns,
            S.keys(:req => [:first_name.ns, :last_name.ns, :email.ns],
                   :opt => [:phone.ns]))

      assert S.valid?(:person.ns, :first_name.ns => "Elon",
                                  :last_name.ns  => "Musk",
                                  :email.ns      => "elon@example.com")

      # Fails required key check
      refute S.valid?(:person.ns, :first_name.ns => "Elon")

      # Invalid value for key not specified in `req`
      refute S.valid?(:person.ns, :first_name.ns => "Elon",
                                  :last_name.ns  => "Musk",
                                  :email.ns      => "elon@example.com",
                                  :acctid.ns     => "123")

      # unqualified keys
      S.def(:person_unq.ns,
            S.keys(:req_un => [:first_name.ns, :last_name.ns, :email.ns],
                   :opt_un => [:phone.ns]))

      refute S.valid?(:person_unq.ns, {})

      refute S.valid?(:person_unq.ns, :first_name => "Elon",
                                      :last_name  => "Musk",
                                      :email      => "not-an-email")

      assert S.valid?(:person_unq.ns, :first_name => "Elon",
                                      :last_name  => "Musk",
                                      :email      => "elon@example.com")
    end

    def test_and_keys_or_keys
      spec = S.keys(:req => [:x.ns, :y.ns, S.or_keys(:secret.ns, S.and_keys(:user.ns, :pwd.ns))])
      S.def(:auth.ns, spec)

      assert S.valid?(:auth.ns, :x.ns => "foo", :y.ns => "bar", :secret.ns => "secret")
      assert S.valid?(:auth.ns, :x.ns => "foo", :y.ns => "bar", :user.ns => "user", :pwd.ns => "password")

      refute S.valid?(:auth.ns, :x.ns => "foo", :y.ns => "bar", :secret.ns => "secret", :user.ns => "user", :pwd.ns => "password")
      refute S.valid?(:auth.ns, :x.ns => "foo", :y.ns => "bar", :user.ns => "user")
      refute S.valid?(:auth.ns, :x.ns => "foo", :y.ns => "bar")
    end

    def test_merge
      S.def(:"animal/kind", String)
      S.def(:"animal/says", String)
      S.def(:"animal/common", S.keys(:req => [:"animal/kind", :"animal/says"]))
      S.def(:"dog/tail?", :boolean.ns(S))
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
      S.def(:symbol_collection.ns, S.coll_of(Symbol))

      assert_equal [:a, :b, :c], S.conform(:symbol_collection.ns, [:a, :b, :c])
      assert_equal Set[5, 10, 2], S.conform(S.coll_of(Numeric), Set[5, 10, 2])

      expected = { :a => :x, :b => :y, :c => :z }
      assert_equal expected, S.conform(S.coll_of(:symbol_collection.ns), :a => :x, :b => :y, :c => :z)

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

      Gen.generate(S.gen(:symbol_collection.ns)).each do |x|
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
      S.def(:point.ns, S.tuple(Integer, Integer, Integer))

      assert S.valid?(:point.ns, [1, 2, 3])
      refute S.valid?(:point.ns, [1, 2, "3"])

      expected = {
        :"Speculation/problems" => [
          { :path => [2], :val => 3.0, :via => [:point.ns], :in => [2], :pred => Integer }
        ]
      }

      assert_equal expected, S.explain_data(:point.ns, [1, 2, 3.0])

      assert Gen.generate(S.gen(:point.ns)).all? { |x| x.is_a?(Integer) }
    end

    def test_hash_of
      S.def(:scores.ns, S.hash_of(String, Integer))

      expected = { "Sally" => 1000, "Joe" => 500 }
      assert_equal expected, S.conform(:scores.ns, "Sally" => 1000, "Joe" => 500)

      refute S.valid?(:scores.ns, "Sally" => true, "Joe" => 500)

      hash = Gen.generate(S.gen(:scores.ns))

      hash.keys.each do |key|
        assert_kind_of String, key
      end
      hash.values.each { |value| assert_kind_of Integer, value }
    end

    def test_conformer
      S.def(:wont_conform_keys.ns, S.hash_of(S.and(Symbol, S.conformer(&:to_s)),
                                             S.and(Float, S.conformer(&:to_i))))

      assert_equal({ :foo => 1, :bar => 2 },
                   S.conform(:wont_conform_keys.ns, :foo => 1.0, :bar => 2.0))

      S.def(:will_conform_keys.ns, S.hash_of(S.and(Symbol, S.conformer(&:to_s)),
                                             S.and(Float, S.conformer(&:to_i)),
                                             :conform_keys => true))

      assert_equal({ "foo" => 1, "bar" => 2 },
                   S.conform(:will_conform_keys.ns, :foo => 1.0, :bar => 2.0))
    end

    def test_explain_data
      S.def(:even.ns, ->(x) { x.even? })

      ed = S.explain_data(:even.ns, 1)
      problems = ed.fetch(:problems.ns(S))

      assert_equal 1, problems.count

      problem = problems.first
      assert_equal [], problem[:path]
      assert_equal 1, problem[:val]
      assert_equal [:even.ns], problem[:via]
      assert_equal [], problem[:in]
      assert_kind_of Proc, problem[:pred]

      S.def(:integer.ns, Integer)
      S.def(:even.ns, ->(x) { x.even? })
      S.def(:even_integer.ns, S.and(:integer.ns, :even.ns))

      ed = S.explain_data(:even_integer.ns, "s")
      problems = ed.fetch(:problems.ns(S))

      assert_equal 1, problems.count

      problem = problems.first
      assert_equal [], problem[:path]
      assert_equal "s", problem[:val]
      assert_equal [:even_integer.ns, :integer.ns], problem[:via]
      assert_equal [], problem[:in]
      assert_equal Integer, problem[:pred]
    end

    def test_explain_data_map
      email_regex = /^[a-zA-Z1-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,63}$/
      S.def(:email_type.ns, S.and(String, email_regex))

      S.def(:acctid.ns, Integer)
      S.def(:first_name.ns, String)
      S.def(:last_name.ns, String)
      S.def(:email.ns, :email_type.ns)
      S.def(:person.ns,
            S.keys(:req => [:first_name.ns, :last_name.ns, :email.ns],
                   :opt => [:phone.ns]))

      input = {
        :first_name.ns => "Elon",
        :last_name.ns  => "Musk",
        :email.ns      => "n/a"
      }

      expected = {
        :"Speculation/problems" => [
          {
            :path => [:email.ns],
            :val  => "n/a",
            :in   => [:email.ns],
            :via  => [
              :person.ns,
              :email_type.ns
            ],
            :pred => /^[a-zA-Z1-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,63}$/
          }
        ]
      }

      assert_equal expected, S.explain_data(:person.ns, input)
    end

    def test_explain_or
      S.def(:name_or_id.ns, S.or(:name => String, :id => Integer))

      expected = {
        :"Speculation/problems" => [
          { :path => [:name], :val => :foo, :in => [], :via => [:name_or_id.ns], :pred => String },
          { :path => [:id], :val => :foo, :in => [], :via => [:name_or_id.ns], :pred => Integer }
        ]
      }

      assert_equal expected, S.explain_data(:name_or_id.ns, :foo)
      assert_nil S.explain_data(:name_or_id.ns, 1)
    end

    def test_explain_regex
      S.def(:ingredient.ns, S.cat(:quantity => Numeric, :unit => Symbol))

      ed = S.explain_data(:ingredient.ns, [11, "peaches"])
      problems = ed.fetch(:problems.ns(S))

      assert_equal 1, problems.count

      problem = problems.first
      assert_equal [:unit], problem[:path]
      assert_equal "peaches", problem[:val]
      assert_equal [:ingredient.ns], problem[:via]
      assert_equal [1], problem[:in]
      assert_equal Symbol, problem[:pred]

      S.def(:nested.ns, S.cat(:names_sym => ->(x) { x == :names },
                              :names     => S.spec(S.zero_or_more(String)),
                              :nums_sym  => ->(x) { x == :nums },
                              :nums      => S.spec(S.constrained(S.one_or_more(Numeric),
                                                                 ->(nums) { nums.count.even? }))))

      ed = S.explain_data(:nested.ns, [:names, ["a", "b"], :nums, [1, 2, 3, 4, 5]])
      problems = ed.fetch(:problems.ns(S))

      assert_equal 1, problems.count

      problem = problems.first
      assert_equal [:nums], problem[:path]
      assert_equal [1, 2, 3, 4, 5], problem[:val]
      assert_equal [3], problem[:in]
      assert_equal [:nested.ns], problem[:via]
      assert_kind_of Proc, problem[:pred]
    end

    def test_explain_hash_of
      S.def(:scores.ns, S.hash_of(String, Integer))

      expected = { :"Speculation/problems" => [{ :path => [1],
                                                 :val  => "300",
                                                 :via  => [:scores.ns],
                                                 :in   => ["Joe", 1],
                                                 :pred => Integer }] }

      assert_equal expected, S.explain_data(:scores.ns, "Sally" => 1000, "Joe" => "300")
    end

    def test_explain_alt
      S.def(:nested.ns, S.cat(:names_sym => ->(x) { x == :names },
                              :names     => S.spec(S.zero_or_more(String)),
                              :nums_sym  => ->(x) { x == :nums },
                              :nums      => S.spec(S.alt(:ints   => S.one_or_more(Integer),
                                                         :floats => S.one_or_more(Float)))))

      expected = {
        :"Speculation/problems" => [
          {
            :path => [:nums, :ints],
            :val  => "1",
            :in   => [3, 0],
            :via  => [:nested.ns],
            :pred => Integer
          },
          {
            :path => [:nums, :floats],
            :val  => "1",
            :in   => [3, 0],
            :via  => [:nested.ns],
            :pred => Float
          }
        ]
      }

      assert_equal expected, S.explain_data(:nested.ns, [:names, ["a", "b"], :nums, ["1"]])
    end

    def test_explain
      S.def(:person.ns(:unq),
            S.keys(:req_un => [:first_name.ns, :last_name.ns, :email.ns],
                   :opt_un => [:phone.ns]))

      assert_equal <<-EOS, S.explain_str(:person.ns(:unq), :first_name => "Elon")
val: {:first_name=>\"Elon\"} fails spec: :\"unq/person\" predicate: \":Speculation::SpeculationTest/last_name\"
val: {:first_name=>\"Elon\"} fails spec: :\"unq/person\" predicate: \":Speculation::SpeculationTest/email\"
      EOS

      email_regex = /^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,63}$/
      S.def(:email.ns, S.and(String, email_regex))

      assert_equal <<-EOS, S.explain_str(:person.ns(:unq), :first_name => "Elon", :last_name => "Musk", :email => "elon")
In: [:email] val: "elon" fails spec: :"Speculation::SpeculationTest/email" at: [:email] predicate: /^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,63}$/
      EOS
    end

    def test_explain_and_keys_or_keys
      S.def(:person.ns(:unq),
            S.keys(:req_un => [S.or_keys(S.and_keys(:first_name.ns, :last_name.ns), :email.ns)],
                   :opt_un => [:phone.ns]))

      assert_equal <<-EOS, S.explain_str(:person.ns(:unq), :first_name => "Elon")
val: {:first_name=>"Elon"} fails spec: :"unq/person" predicate: "(:Speculation::SpeculationTest/first_name and :Speculation::SpeculationTest/last_name) or :Speculation::SpeculationTest/email"
      EOS
    end

    def test_nilable
      S.def(:maybe_string.ns, S.nilable(String))

      assert S.valid?(:maybe_string.ns, "foo")
      assert S.valid?(:maybe_string.ns, nil)
      refute S.valid?(:maybe_string.ns, 1)

      ed = S.explain_data(:maybe_string.ns, 1)
      expected = [{ :path => [:"Speculation/pred"],
                    :val  => 1,
                    :in   => [],
                    :via  => [:maybe_string.ns],
                    :pred => String },
                  { :path => [:"Speculation/nil"],
                    :val  => 1,
                    :in   => [],
                    :via  => [:maybe_string.ns],
                    :pred => NilClass }]

      assert_equal expected, ed.fetch(:problems.ns(Speculation))

      val = Gen.generate(S.gen(:maybe_string.ns))
      assert val.is_a?(String) || val.nil?
    end

    def test_set_predicate
      S.def(:suit.ns, Set[:club, :diamond, :heart, :spade])

      assert S.valid?(:suit.ns, :club)
      assert S.valid?(:suit.ns, :heart)
      refute S.valid?(:suit.ns, :lung)

      ed = S.explain_data(:suit.ns, 1)
      expected = [{ :path => [],
                    :val  => 1,
                    :via  => [:"Speculation::SpeculationTest/suit"],
                    :in   => [],
                    :pred => Set[:club, :diamond, :heart, :spade] }]

      assert_equal expected, ed.fetch(:problems.ns(Speculation))

      val = Gen.generate(S.gen(:suit.ns))
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
      expected = { :path => [:ret], :via => [identifier], :in => [], :pred => Integer }

      ed = S.explain_data(mod.method(:foo), :to_s.to_proc)
      assert_equal expected, ed.fetch(:problems.ns(S)).first.reject { |k, _v| k == :val }
      assert_kind_of String, ed.fetch(:problems.ns(S)).first[:val]

      S.def(:foo.ns, S.fspec(:args => S.cat(:x => String), :ret => Integer))

      ed = S.explain_data(:foo.ns, :trigger_no_method_error.to_proc).fetch(:problems.ns(S)).first
      assert_equal [], ed[:path]
      assert_equal "f.call(*args)", ed[:pred]
      assert_equal [:foo.ns], ed[:via]
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
             :args  => :empty.ns(S),
             :block => S.fspec(:args => S.cat(:x => Integer),
                               :ret  => Integer),
             :ret   => Integer)

      assert S.valid?(mod.method(:foo), mod.method(:foo))
      refute S.valid?(mod.method(:foo), :to_s.to_proc)
      refute S.valid?(mod.method(:foo), "not-a-method")

      val = Gen.generate(S.gen(mod.method(:foo)))
      assert_kind_of Integer, val.call { |_x| 1 }

      identifier = S.Identifier(mod.method(:foo))
      ed = S.explain_data(mod.method(:foo), ->(&b) { b.call("1") })
      ed = ed.fetch(:problems.ns(S)).first

      assert_equal [], ed[:path]
      assert_equal "f.call(*args)", ed[:pred]
      assert_equal [], ed[:val].first
      assert_kind_of Proc, ed[:val].last
      assert_equal "In: [0] val: \"1\" fails at: [:x] predicate: Integer", ed[:reason]
      assert_equal [identifier], ed[:via]
      assert_equal [], ed[:in]
    end
  end
end
