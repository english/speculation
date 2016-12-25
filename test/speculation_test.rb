require 'test_helper'
require 'speculation/core'
require 'hamster'

class SpeculationTest < Minitest::Test
  S = Speculation::Core
  H = Hamster::Hash
  V = Hamster::Vector
  HSet = Hamster::Set

  using Speculation::Core.namespaced_symbols(Speculation::Core)

  def setup
    Speculation::Core.reset_registry!
  end

  def test_that_it_has_a_version_number
    refute_nil ::Speculation::VERSION
  end

  def test_conform_with_existing_spec
    S.def(:int?, -> (x) { x.is_a?(Integer) })

    assert_equal 2, S.conform(:int?, 2)
    assert_equal :"Speculation::Core/invalid", S.conform(:int?, "two")

    assert S.valid?(:int?, 2)
    refute S.valid?(:int?, "two")
  end

  def test_conform_with_predicate
    predicate = -> (x) { x.is_a?(Integer) }
    assert_equal 2, S.conform(predicate, 2)
    assert_equal :"Speculation::Core/invalid", S.conform(predicate, "two")

    assert S.valid?(predicate, 2)
    refute S.valid?(predicate, "two")
  end

  def test_and_composition
    S.def(:int?, -> (x) { x.is_a?(Integer) })
    S.def(:even?, -> (x) { x.even? })

    S.def(:big_even, S.and(:int?, :even?, -> (x) { x > 1000 }))

    assert_equal :"Speculation::Core/invalid", S.conform(:big_even, :foo)
    assert_equal :"Speculation::Core/invalid", S.conform(:big_even, 100)
    assert_equal 1_000_000, S.conform(:big_even, 1_000_000)

    refute S.valid?(:big_even, :foo)
    refute S.valid?(:big_even, 10)
    assert S.valid?(:big_even, 1_000_000)
  end

  def test_or_composition
    S.def(:int?, -> (x) { x.is_a?(Integer) })
    S.def(:string?, -> (x) { x.is_a?(String) })

    S.def(:name_or_id, S.or(name: :string?, id: :int?))

    assert_equal :"Speculation::Core/invalid", S.conform(:name_or_id, :foo)
    assert_equal [:name, "abc"], S.conform(:name_or_id, "abc")
    assert_equal [:id, 100], S.conform(:name_or_id, 100)
  end

  def test_cat_sequence
    S.def(:number?, -> (x) { x.is_a?(Numeric) })
    S.def(:symbol?, -> (x) { x.is_a?(Symbol) })
    S.def(:string?, -> (x) { x.is_a?(String) })
    S.def(:boolean?, -> (x) { [true, false].include?(x) })

    S.def(:ingredient, S.cat(quantity: :number?, unit: :symbol?))

    expected = H[quantity: 2, unit: :teaspoon]
    assert_equal(expected, S.conform(:ingredient, [2, :teaspoon]))

    S.def(:config, S.cat(prop: :string?, val: S.alt(s: :string?, b: :boolean?)))

    assert_equal(H[prop: "-server", val: V[:s, "foo"]],
                 S.conform(:config, V["-server", "foo"]))
  end

  def test_nested_cat_sequence
    S.def(:number?, -> (x) { x.is_a?(Numeric) })
    S.def(:string?, -> (x) { x.is_a?(String) })
    S.def(:nested, S.cat(names_sym: -> (x) { x == :names },
                         names: S.spec(S.cat(name1: :string?, name2: :string?)),
                         nums_sym: -> (x) { x == :nums },
                         nums: S.spec(S.cat(num1: :number?, num2: :number?))))

    conformed = S.conform(:nested, [:names, ["a", "b"], :nums, [1, 2]])

    expected = H[names_sym: :names,
                 nums_sym: :nums,
                 nums: H[num1: 1, num2: 2],
                 names: H[name1: "a", name2: "b"]]

    assert_equal expected, conformed
  end

  def test_zero_or_more
    S.def(:symbol?, -> (x) { x.is_a?(Symbol) })
    S.def(:seq_of_symbols, S.zero_or_more(:symbol?))

    assert_equal [:a, :b, :c], S.conform(:seq_of_symbols, [:a, :b, :c])
    assert_equal [], S.conform(:seq_of_symbols, [])
    assert_equal :"Speculation::Core/invalid", S.conform(:seq_of_symbols, [1, 2, 3])
  end

  def test_nested_seq
    S.def(:nested, S.cat(names_sym: -> (x) { x == :names },
                         names: S.spec(S.zero_or_more(String)),
                         nums_sym: -> (x) { x == :nums }, nums: S.spec(S.zero_or_more(Numeric))))

    conformed = S.conform(:nested, [:names, ["a", "b"], :nums, [1, 2]])

    expected = H[names_sym: :names, names: V["a", "b"],
                 nums_sym: :nums, nums: V[1, 2]]

    assert_equal expected, conformed
  end

  def test_non_nested
    S.def(:unnested, S.cat(names_sym: -> (x) { x == :names },
                           names: S.zero_or_more(String),
                           nums_sym: -> (x) { x == :nums },
                           nums: S.zero_or_more(Numeric)))

    expected = H[names_sym: :names, names: V["a", "b"],
                 nums_sym: :nums, nums: V[1, 2, 3]]

    assert_equal expected, S.conform(:unnested, [:names, "a", "b", :nums, 1, 2, 3])
  end

  def test_class_predicate
    S.def(:seq_of_symbols, S.zero_or_more(Symbol))
    assert_equal [:foo, :bar], S.conform(:seq_of_symbols, [:foo, :bar])

    S.def(:symbol?, Symbol)
    S.def(:seq_of_symbols, S.zero_or_more(:symbol?))
    assert_equal [:foo, :bar], S.conform(:seq_of_symbols, [:foo, :bar])
  end

  def test_one_or_more
    S.def(:seq_of_symbols, S.one_or_more(Symbol))

    assert_equal [:a, :b, :c], S.conform(:seq_of_symbols, [:a, :b, :c])
    assert_equal :"Speculation::Core/invalid", S.conform(:seq_of_symbols, [])
  end

  def test_zero_or_one
    S.def(:odd?, -> (x) { x.odd? })
    S.def(:even?, -> (x) { x.even? })

    S.def(:maybe_odd, S.zero_or_one(:odd?))

    assert_equal 1, S.conform(:maybe_odd, [1])
    assert_nil S.conform(:maybe_odd, [])
    assert_equal :"Speculation::Core/invalid", S.conform(:maybe_odd, [2])

    S.def(:odds_then_maybe_even, S.cat(odds: S.one_or_more(:odd?),
                                       even: S.zero_or_one(:even?)))

    expected = H[odds: V[1, 3, 5], even: 100]
    assert_equal expected, S.conform(:odds_then_maybe_even, [1, 3, 5, 100])
  end

  def test_alt_zero_or_more
    S.def(:config, S.zero_or_more(
      S.cat(prop: String,
            val: S.alt(s: String, b: -> (x) { [true, false].include?(x) }))))

    conformed = S.conform(:config, V["-server", "foo", "-verbose", true, "-user", "joe"])
    expected = V[H[prop: "-server",  val: V[:s, "foo"]],
                 H[prop: "-verbose", val: V[:b, true]],
                 H[prop: "-user",    val: V[:s, "joe"]]]

    assert_equal expected, conformed
  end

  def test_constrained
    S.def(:even_strings,
          S.constrained(S.zero_or_more(String), -> (x) { x.count.even? }))

    refute S.valid?(:even_strings, ["a"])
    assert S.valid?(:even_strings, ["a", "b"])
    refute S.valid?(:even_strings, ["a", "b", "c"])
    assert S.valid?(:even_strings, ["a", "b", "c", "d"])
  end

  def test_hash_keys
    email_regex = /^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,63}$/
    S.def(:email_type, S.and(String, email_regex))

    S.def(:acctid.ns(self), Integer)
    S.def(:first_name.ns(self), String)
    S.def(:last_name.ns(self), String)
    S.def(:email.ns(self), :email_type)

    S.def(:person.ns(self),
          S.keys(req: [:first_name.ns(self), :last_name.ns(self), :email.ns(self)],
                 opt: [:phone.ns(self)]))

    assert S.valid?(:person.ns(self), H[:first_name.ns(self) => "Elon",
                                        :last_name.ns(self)  => "Musk",
                                        :email.ns(self)      => "elon@example.com"])

    # Fails required key check
    refute S.valid?(:person.ns(self), H[:first_name.ns(self) => "Elon"])

    # Invalid value for key not specified in `req`
    refute S.valid?(:person.ns(self), H[:first_name.ns(self) => "Elon",
                                        :last_name.ns(self)  => "Musk",
                                        :email.ns(self)      => "elon@example.com",
                                        :acctid.ns(self)     => "123"])

    # unqualified keys
    S.def(:person_unq.ns,
          S.keys(req_un: [:first_name.ns(self), :last_name.ns(self), :email.ns(self)],
                 opt_un: [:phone.ns(self)]))

    refute S.valid?(:person_unq.ns, H[])

    refute S.valid?(:person_unq.ns, H[:first_name => "Elon",
                                      :last_name  => "Musk",
                                      :email      => "not-an-email"])

    assert S.valid?(:person_unq.ns, H[:first_name => "Elon",
                                      :last_name  => "Musk",
                                      :email      => "elon@example.com"])
  end

  def test_coll_of
    S.def(:symbol_collection, S.coll_of(Symbol))

    assert_equal V[:a, :b, :c], S.conform(:symbol_collection, V[:a, :b, :c])
    assert_equal HSet[5, 10, 2], S.conform(S.coll_of(Numeric), HSet[5, 10, 2])

    assert_equal [:a, :b, :c], S.conform(:symbol_collection, [:a, :b, :c])
    assert_equal Set[5, 10, 2], S.conform(S.coll_of(Numeric), Set[5, 10, 2])

    expected = { a: :x, b: :y, c: :z }
    assert_equal expected, S.conform(S.coll_of(:symbol_collection), { a: :x, b: :y, c: :z })

    assert S.valid?(S.coll_of(Integer), [1, 2, 3])
    assert S.valid?(S.coll_of(Integer, kind: -> (coll) { coll.is_a?(Array) }), [1, 2, 3])
    refute S.valid?(S.coll_of(Integer), ['a', 'b', 'c'])
    refute S.valid?(S.coll_of(Integer, kind: -> (coll) { coll.is_a?(V) }), [1, 2, 3])

    assert S.valid?(S.coll_of(Integer, count: 3), [1, 2, 3])
    refute S.valid?(S.coll_of(Integer, count: 2), [1, 2, 3])

    refute S.valid?(S.coll_of(Integer, min_count: 3, max_count: 4), [1, 2])
    assert S.valid?(S.coll_of(Integer, min_count: 3, max_count: 4), [1, 2, 3])
    assert S.valid?(S.coll_of(Integer, min_count: 3, max_count: 4), [1, 2, 3, 4])
    refute S.valid?(S.coll_of(Integer, min_count: 3, max_count: 4), [1, 2, 3, 4, 5])
  end

  def test_tuple
    S.def(:point, S.tuple(Integer, Integer, Integer))

    assert S.valid?(:point, [1, 2, 3])
    refute S.valid?(:point, [1, 2, "3"])
  end

  def test_map_of
    S.def(:scores, S.map_of(String, Integer))

    expected = { "Sally" => 1000, "Joe" => 500 }
    assert_equal expected, S.conform(:scores, { "Sally" => 1000, "Joe" => 500 })

    expected = H["Sally" => 1000, "Joe" => 500]
    assert_equal expected, S.conform(:scores, H["Sally" => 1000, "Joe" => 500])

    refute S.valid?(:scores, H["Sally" => 1000, :Joe => 500])
    refute S.valid?(:scores, { "Sally" => true, "Joe" => 500 })
  end

  def test_explain_data
    S.def(:even, -> (x) { x.even? })

    expected = H[
      :"Speculation::Core/problems" => V[
        H[path: V[], val: 1, via: V[:even], in: V[], pred: "<proc>"]
      ]
    ]
    assert_equal expected, S.explain_data(:even, 1)

    S.def(:integer, Integer)
    S.def(:even, -> (x) { x.even? })
    S.def(:even_integer, S.and(:integer, :even))

    expected = H[
      :"Speculation::Core/problems" => V[
        H[path: V[], val: "s", in: V[], via: V[:even_integer, :integer], pred: "Integer"]
      ]
    ]
    assert_equal expected, S.explain_data(:even_integer, "s")
  end

  def test_explain_data_map
    email_regex = /^[a-zA-Z1-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,63}$/
    S.def(:email_type, S.and(String, email_regex))

    S.def(:acctid.ns(self), Integer)
    S.def(:first_name.ns(self), String)
    S.def(:last_name.ns(self), String)
    S.def(:email.ns(self), :email_type)
    S.def(:person.ns(self),
          S.keys(req: [:first_name.ns(self), :last_name.ns(self), :email.ns(self)],
                 opt: [:phone.ns(self)]))

    input = {
      :first_name.ns(self) => "Elon",
      :last_name.ns(self)  => "Musk",
      :email.ns(self)      => "n/a"
    }

    expected = H[
      :"Speculation::Core/problems" => V[
        H[
          path: V[:"SpeculationTest#test_explain_data_map/email"],
          val: "n/a",
          in: V[:"SpeculationTest#test_explain_data_map/email"],
          via: V[
            :"SpeculationTest#test_explain_data_map/person",
            :"SpeculationTest#test_explain_data_map/email" # clojure returns email-type for this...
          ],
          pred: "/^[a-zA-Z1-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,63}$/"
        ]
      ]
    ]
    assert_equal expected, S.explain_data(:person.ns(self), input)
  end

  def test_explain_or
    S.def(:name_or_id, S.or(name: String, id: Integer))

    expected = H[
      :"Speculation::Core/problems" => V[
        H[path: V[:name], val: :foo, in: V[], via: V[:name_or_id], pred: "String"],
        H[path: V[:id], val: :foo, in: V[], via: V[:name_or_id], pred: "Integer"],
      ]
    ]

    assert_equal expected, S.explain_data(:name_or_id, :foo)
    assert_nil S.explain_data(:name_or_id, 1)
  end

  def test_explain_regex
    S.def(:ingredient, S.cat(quantity: Numeric, unit: Symbol))
    expected = H[:"Speculation::Core/problems" => V[H[path: V[:unit],
                                                      val: "peaches",
                                                      via: V[:ingredient],
                                                      in: V[1],
                                                      pred: "Symbol"]]]


    assert_equal expected, S.explain_data(:ingredient, V[11, "peaches"])

    S.def(:nested, S.cat(names_sym: -> (x) { x == :names },
                         names: S.spec(S.zero_or_more(String)),
                         nums_sym: -> (x) { x == :nums },
                         nums: S.spec(S.constrained(S.one_or_more(Numeric),
                                                    -> (nums) { nums.count.even? }))))

    expected = H[:"Speculation::Core/problems" => V[
      H[:path => V[:nums],
        :val => V[1, 2, 3, 4, 5],
        :in => V[3],
        :via => V[:nested],
        pred: "<proc>"]]]

    assert_equal expected, S.explain_data(:nested, [:names, ["a", "b"], :nums, [1, 2, 3, 4, 5]])
  end

  def test_explain_tuple
    S.def(:point, S.tuple(Float, Float, Float))

    expected = H[:"Speculation::Core/problems" => V[H[path: V[2],
                                                      val: 3,
                                                      via: V[:point],
                                                      in: V[2],
                                                      pred: "Float"]]]


    assert_equal expected, S.explain_data(:point, V[1.1, 2.2, 3])
  end

  def test_explain_map_of
    S.def(:scores, S.map_of(String, Integer))

    expected = H[:"Speculation::Core/problems" => V[H[path: V[1],
                                                      val: "300",
                                                      via: V[:scores],
                                                      in: V["Joe", 1],
                                                      pred: "Integer"]]]
    
    assert_equal expected, S.explain_data(:scores, H["Sally" => 1000, "Joe" => "300"])
  end

  def test_explain_alt
    S.def(:nested, S.cat(names_sym: -> (x) { x == :names },
                         names: S.spec(S.zero_or_more(String)),
                         nums_sym: -> (x) { x == :nums },
                         nums: S.spec(S.alt(
                           ints: S.one_or_more(Integer),
                           floats: S.one_or_more(Float)))))

    expected = H[:"Speculation::Core/problems" => V[
       H[:path => V[:nums, :ints],
        :val => "1",
        :in => V[3, 0],
        :via => V[:nested],
        :pred => "Integer"],
       H[:path => V[:nums, :floats],
        :val => "1",
        :in => V[3, 0],
        :via => V[:nested],
        :pred => "Float"]]]

    assert_equal expected, S.explain_data(:nested, [:names, ["a", "b"], :nums, ["1"]])
  end
end
