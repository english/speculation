# Exercise from Ruby Quiz: http://rubyquiz.com/quiz155.html
# Borrowed heavily from TreeTop parser used in http://learnruby.com/examples/ruby-quiz-155.shtml

require "bundler/setup"
require "minitest/autorun"
require "speculation"
require "speculation/gen"
require "speculation/test"

S = Speculation
Gen = S::Gen
STest = S::Test

module JSONParser
  extend S::NamespacedSymbols

  def self.re_literal(string)
    kvs = string.each_char.each_with_index.reduce({}) { |h, (v, i)| h.merge(i => Set[v]) }
    S.cat(kvs)
  end

  S.def ns(:maybe_spaces), S.zero_or_more(Set[" "])

  S.def ns(:boolean), S.alt(:true  => re_literal("true"),
                            :false => re_literal("false"))

  S.def ns(:number), S.alt(:integer => ns(:integer),
                           :float   => ns(:float))

  S.def ns(:digit), /\A[[:digit:]]\z/
  S.def ns(:digits), S.one_or_more(ns(:digit))
  S.def ns(:exp), Set["e", "E"]
  S.def ns(:maybe_sign), S.zero_or_one(Set["-", "+"])
  S.def ns(:exponent), S.cat(:pre    => ns(:exp),
                             :sign   => ns(:maybe_sign),
                             :digits => ns(:digits))
  S.def ns(:maybe_exponent), S.zero_or_one(ns(:exponent))

  S.def ns(:integer), S.cat(:neg      => S.zero_or_one(Set["-"]),
                            :digits   => ns(:digits),
                            :exponent => ns(:maybe_exponent))

  S.def ns(:maybe_neg), S.zero_or_one(Set["-"])
  S.def ns(:float), S.cat(:neg         => ns(:maybe_neg),
                          :digits      => ns(:digits),
                          :dot         => Set["."],
                          :more_digits => ns(:digits),
                          :exponent    => ns(:maybe_exponent))

  S.def ns(:quote), Set['"']
  S.def ns(:maybe_characters), S.zero_or_more(ns(:character))
  S.def ns(:string), S.cat(:open     => ns(:quote),
                           :contents => ns(:maybe_characters),
                           :close    => ns(:quote))


  S.def ns(:character), S.alt(:escaped => ns(:escaped_character),
                              :special => ns(:special_character),
                              :unicode => ns(:unicode_character),
                              :regular => ns(:regular_character))

  S.def ns(:unicode_character), S.cat(:escape => Set['\\'],
                                      :u      => Set['u'],
                                      :digits => S.constrained(S.one_or_more(ns(:hex_digit)), ->(digits) { digits.count == 4 }))

  S.def ns(:hex_digit), /[0-9a-fA-F]/

  S.def ns(:escaped_character), S.cat(:escape => Set['\\'],
                                      :val    => Set['\\', '"'])

  special_character_map = {
    "b" => "\b",
    "f" => "\f",
    "n" => "\n",
    "r" => "\r",
    "t" => "\t",
  }

  S.def ns(:special_character), S.cat(:escape => Set['\\'],
                                      :val    => Set['b', 'f', 'n', 'r', 't'])

  S.def ns(:regular_character), ->(s) { !['\\', '"'].include?(s) }

  S.def ns(:open_square_bracket), Set["["]
  S.def ns(:close_square_bracket), Set["]"]
  S.def ns(:empty_array), S.cat(:open   => ns(:open_square_bracket),
                                :spaces => ns(:maybe_spaces),
                                :close  => ns(:close_square_bracket))

  S.def ns(:non_empty_array), S.cat(:open       => ns(:open_square_bracket),
                                    :space      => ns(:maybe_spaces),
                                    :value_list => ns(:value_list),
                                    :more_space => ns(:maybe_spaces),
                                    :close      => ns(:close_square_bracket))

  S.def ns(:array), S.alt(:non_empty => ns(:non_empty_array),
                          :empty     => ns(:empty_array))

  S.def ns(:value_list), S.alt(:val  => ns(:json),
                               :rest => S.cat(:val   => ns(:json),
                                              :comma => Set[","],
                                              :space => ns(:maybe_spaces),
                                              :tail  => ns(:value_list)))

  S.def ns(:open_brace), Set["{"]
  S.def ns(:close_brace), Set["}"]
  S.def ns(:empty_object), S.cat(:open   => ns(:open_brace),
                                 :spaces => ns(:maybe_spaces),
                                 :close  => ns(:close_brace))

  S.def ns(:non_empty_object), S.cat(:open        => ns(:open_brace),
                                     :spaces      => ns(:maybe_spaces),
                                     :contents    => ns(:object_contents),
                                     :more_spaces => ns(:maybe_spaces),
                                     :close       => ns(:close_brace))

  S.def ns(:object), S.alt(:empty_object     => ns(:empty_object),
                           :non_empty_object => ns(:non_empty_object))

  S.def ns(:kv), S.cat(:before => ns(:maybe_spaces),
                       :key    => ns(:string),
                       :colon  => Set[":"],
                       :after  => ns(:maybe_spaces),
                       :val    => ns(:json))

  S.def ns(:object_contents), S.alt(:kv   => ns(:kv),
                                    :rest => S.cat(:kv    => ns(:kv),
                                                   :comma => Set[","],
                                                   :space => ns(:maybe_spaces),
                                                   :tail  => ns(:object_contents)))

  S.def ns(:json), S.cat(:pre => ns(:maybe_spaces),
                         :val => S.alt(:null    => re_literal("null"),
                                       :boolean => ns(:boolean),
                                       :number  => ns(:number),
                                       :string  => ns(:string),
                                       :array   => ns(:array),
                                       :object  => ns(:object)),
                         :post => ns(:maybe_spaces))

  def self.parse(s)
    chars = s.split("")
    json_data = S.conform(ns(:json), chars)

    if S.invalid?(json_data)
      fail S.explain_str(ns(:json), chars)
    else
      transform_json(json_data)
    end
  end

  def self.transform_json(data)
    tag, val = data[:val]

    case tag
    when :boolean then transform_boolean(val)
    when :null    then nil
    when :number  then transform_number(val)
    when :string  then transform_string(val)
    when :array   then transform_array(val)
    when :object  then transform_object(val)
    end
  end

  def self.transform_boolean(tagged_val)
    tag, val = tagged_val

    case tag
    when :true  then true
    when :false then false
    end
  end

  def self.transform_number(tagged_val)
    tag, val = tagged_val

    case tag
    when :integer
      Integer(val.values_at(:neg, :digits, :exponent).join)
    when :float
      exp = Hash(val[:exponent]).values_at(:pre, :sign, :digits).join
      val = val.merge(:exp => exp)
      Float(val.values_at(:neg, :digits, :dot, :more_digits, :exp).join)
    end
  end

  SPECIAL_CHARACTER_MAP = {
    "b" => "\b",
    "f" => "\f",
    "n" => "\n",
    "r" => "\r",
    "t" => "\t",
  }

  def self.transform_string(val)
    Array(val[:contents]).map { |(tag, char)|
      case tag
      when :regular then char
      when :special then SPECIAL_CHARACTER_MAP.fetch(char[:val])
      when :escaped then char[:val]
      when :unicode then [char[:digits].join.hex].pack("U")
      end
    }.join
  end

  def self.transform_array(tagged_val)
    tag, val = tagged_val

    case tag
    when :empty     then []
    when :non_empty then transform_value_list(val[:value_list])
    end
  end

  def self.transform_value_list(tagged_val)
    tag, val = tagged_val

    case tag
    when :rest
      head = [transform_json(val[:val])]
      tail = transform_value_list(val[:tail])
      head + tail
    when :val
      [transform_json(val)]
    end
  end

  def self.transform_object(tagged_val)
    tag, val = tagged_val

    case tag
    when :empty_object     then {}
    when :non_empty_object then transform_object_contents(val[:contents])
    end
  end

  def self.transform_object_contents(tagged_val)
    tag, val = tagged_val

    case tag
    when :kv
      { transform_string(val[:key]) => transform_json(val[:val]) }
    when :rest
      transform_object_contents([:kv, val[:kv]]).
        merge(transform_object_contents(val[:tail]))
    end
  end
end

class TestJSONParser < Minitest::Test
  def test_keyword_parsing
    assert_parses true,  "true"
    assert_parses false, "false"
    assert_parses nil,   "null"
  end

  def test_number_parsing
    assert_parses 42,     "42"
    assert_parses -13,    "-13"
    assert_parses 3.1415, "3.1415"
    assert_parses -0.01,  "-0.01"

    assert_parses 0.2e1,  "0.2e1"
    assert_parses 0.2e+1, "0.2e+1"
    assert_parses 0.2e-1, "0.2e-1"
    assert_parses 0.2E1,  "0.2e1"
  end

  def test_string_parsing
    assert_parses String.new, '""'
    assert_parses "JSON",     '"JSON"'

    assert_parses 'nested "quotes"', '"nested \"quotes\""'
    assert_parses "\n",                '"\\n"'

    assert_parses "µ", '"\\u00b5"'
  end

  def test_array_parsing
    assert_parses [], '[]'

    assert_parses ["foo", "bar", "baz"],  '["foo", "bar", "baz"]'
    assert_parses ["JSON", 3.1415, true], '["JSON", 3.1415, true]'
    assert_parses [1, [2, [3]]],          '[1, [2, [3]]]'
  end

  def test_object_parsing
    assert_parses Hash[], '{}'
    assert_parses Hash["foo" => "bar"], '{"foo": "bar"}'
    assert_parses Hash["foo" => "bar", "baz" => "qux"], '{"foo": "bar", "baz": "qux"}'
    assert_parses Hash["JSON" => 3.1415, "data" => true], '{"JSON": 3.1415, "data": true}'

    assert_parses Hash["Array" => [1, 2, 3], "Object" => {"nested" => "objects"}],
      '{"Array": [1, 2, 3], "Object": {"nested": "objects"}}'
  end

  def test_parse_errors
    assert_invalid "{"
    assert_invalid %q{{"key": true false}}

    assert_invalid "["
    assert_invalid "[1,,2]" 
    assert_invalid '"'
    assert_invalid '"\\i"'

    assert_invalid "$1,000"
    assert_invalid "1_000"
    assert_invalid "1K"

    assert_invalid "unknown"
  end

  def assert_invalid(json_string)
    assert_raises(RuntimeError) { JSONParser.parse(json_string) }
  end

  def assert_parses(expected_val, json_string)
    if expected_val.nil?
      assert_nil(JSONParser.parse(json_string))
    else
      assert_equal(expected_val, JSONParser.parse(json_string))
    end
  end
end
