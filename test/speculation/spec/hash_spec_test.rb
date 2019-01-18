# frozen_string_literal: true

require "test_helper"

module Speculation
  class HashSpecTest < Minitest::Test
    S = Speculation
    Utils = S::Utils
    include S::NamespacedSymbols

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

    def test_explain_and_keys_or_keys
      S.def(ns(:unq, :person),
            S.keys(:req_un => [S.or_keys(S.and_keys(ns(:first_name), ns(:last_name)), ns(:email))],
                   :opt_un => [ns(:phone)]))

      ed = S.explain_data ns(:unq, :person), :first_name => "Elon"
      problems = ed[:problems]
      pred = problems.first[:pred]

      assert_equal [Predicates.method(:key?), [S.or_keys(S.and_keys(:first_name, :last_name), :email)]], pred

      assert_equal <<-EOS, S.explain_str(ns(:unq, :person), :first_name => "Elon")
{:first_name=>"Elon"} - failed: [#<Method: Speculation::Predicates.key?>, [[:"Speculation/or", [:"Speculation/and", :first_name, :last_name], :email]]] spec: :"unq/person"
      EOS
    end

    def test_explain_foo
      S.def :"foo/bar", Integer

      hash = {
        :"foo/bar" => "not-an-integer",
        :"baz/qux" => "irrelevant"
      }

      assert_equal <<-EOS, S.explain_str(S.keys, hash)
"not-an-integer" - failed: [Integer, ["not-an-integer"]] in: [:"foo/bar"] at: [:"foo/bar"] spec: :"foo/bar"
      EOS
    end

    def test_and_keys_or_keys
      spec = S.keys(:req => [ns(:x), ns(:y), S.or_keys(ns(:secret), S.and_keys(ns(:user), ns(:pwd)))])
      S.def(ns(:auth), spec)

      assert S.valid?(ns(:auth), ns(:x) => "foo", ns(:y) => "bar", ns(:secret) => "secret")
      assert S.valid?(ns(:auth), ns(:x) => "foo", ns(:y) => "bar", ns(:user) => "user", ns(:pwd) => "password")
      assert S.valid?(ns(:auth), ns(:x) => "foo", ns(:y) => "bar", ns(:secret) => "secret", ns(:user) => "user", ns(:pwd) => "password")

      refute S.valid?(ns(:auth), ns(:x) => "foo", ns(:y) => "bar", ns(:user) => "user")
      refute S.valid?(ns(:auth), ns(:x) => "foo", ns(:y) => "bar")
    end

    def test_merge
      S.def(:"animal/kind", String)
      S.def(:"animal/says", S.and(String, S.conformer(:upcase.to_proc, :downcase.to_proc)))
      S.def(:"animal/common", S.keys(:req => [:"animal/kind", :"animal/says"]))
      S.def(:"dog/tail?", ns(S, :boolean))
      S.def(:"dog/breed", String)
      S.def(:"animal/dog", S.merge(:"animal/common", S.keys(:req => [:"dog/tail?", :"dog/breed"])))

      good_dog = { :"animal/kind" => "dog",
                   :"animal/says" => "woof",
                   :"dog/tail?"   => true,
                   :"dog/breed"   => "retriever" }

      assert_equal Hash[:"animal/kind" => "dog",
                        :"animal/says" => "WOOF",
                        :"dog/tail?"   => true,
                        :"dog/breed"   => "retriever"], S.conform(:"animal/dog", good_dog)

      assert_equal good_dog, S.unform(:"animal/dog", S.conform(:"animal/dog", good_dog))

      bad_dog = { :"animal/kind" => "dog",
                  :"animal/says" => "woof",
                  :"dog/tail?"   => "why yes",
                  :"dog/breed"   => "retriever" }

      # Although weird at first glance, this is the desired behaviour since :"dog/tail" is invalid
      # in both merged S.keys.
      expected = <<EOS
"why yes" - failed: [#<Set: {true, false}>, ["why yes"]] in: [:"dog/tail?"] at: [:"dog/tail?"] spec: :"Speculation/boolean"
"why yes" - failed: [#<Set: {true, false}>, ["why yes"]] in: [:"dog/tail?"] at: [:"dog/tail?"] spec: :"Speculation/boolean"
EOS

      assert_equal expected, S.explain_str(:"animal/dog", bad_dog)
    end

    def test_explain
      S.def(ns(:unq, :person),
            S.keys(:req_un => [ns(:first_name), ns(:last_name), ns(:email)],
                   :opt_un => [ns(:phone)]))

      assert_equal <<-EOS, S.explain_str(ns(:unq, :person), :first_name => "Elon")
{:first_name=>\"Elon\"} - failed: [#<Method: Speculation::Predicates.key?>, [:last_name]] spec: :\"unq/person\"
{:first_name=>\"Elon\"} - failed: [#<Method: Speculation::Predicates.key?>, [:email]] spec: :\"unq/person\"
      EOS

      email_regex = /^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,63}$/
      S.def(ns(:email), S.and(String, email_regex))

      assert_equal <<-EOS, S.explain_str(ns(:unq, :person), :first_name => "Elon", :last_name => "Musk", :email => "elon")
"elon" - failed: [/^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,63}$/, ["elon"]] in: [:email] at: [:email] spec: :"Speculation::HashSpecTest/email"
      EOS
    end

    def test_explain_data_keys
      S.def(ns(:foo), String)
      S.def(ns(:bar), Integer)
      S.def(ns(:baz), String)

      S.def(ns(:hash), S.keys(:req_un => [ns(:foo), ns(:bar), ns(:baz)]))

      expected = { :problems => [{ :path => [],
                                   :pred => [Predicates.method(:key?), [:bar]],
                                   :val  => { :foo => "bar", :baz => "baz" },
                                   :via  => [ns(:hash)],
                                   :in   => [] }],
                   :spec     => ns(:hash),
                   :value    => { :foo => "bar", :baz => "baz" } }

      assert_equal expected, S.explain_data(ns(:hash), :foo => "bar", :baz => "baz")
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
        :problems => [
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
        ],
        :spec     => ns(:person),
        :value    => input
      }

      assert_equal expected, S.explain_data(ns(:person), input)
    end

    def test_conform_unform
      email_regex = /^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,63}$/
      S.def(ns(:email_type), S.and(String, email_regex))

      S.def(ns(:acctid), Integer)
      S.def(ns(:first_name), String)
      S.def(ns(:last_name), String)
      S.def(ns(:email), ns(:email_type))

      S.def(ns(:person),
            S.keys(:req => [ns(:first_name), ns(:last_name), ns(:email)],
                   :opt => [ns(:phone)]))

      val = { ns(:first_name) => "Elon",
              ns(:last_name)  => "Musk",
              ns(:email)      => "elon@example.com" }

      assert_equal val, S.unform(ns(:person), S.conform(ns(:person), val))
    end
  end
end
