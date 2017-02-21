# See https://clojure.org/guides/spec (2017-18-02)

require "set"
require "date"
require "speculation"

S = Speculation
using S::NamespacedSymbols.refine(self)

## Predicates

# Each spec describes a set of allowed values. There are several ways to build
# specs and all of them can be composed to build more sophisticated specs.

# A Ruby proc that takes a single argument and returns a truthy value is a
# valid predicate spec. We can check whether a particular data value conforms
# to a spec using conform:

S.conform :even?.to_proc, 1000 # => 1000

# The conform function takes something that can be a spec and a data value.
# Here we are passing a predicate which is implicitly converted into a spec.
# The return value is "conformed". Here, the conformed value is the same as the
# original value - we’ll see later where that starts to deviate. If the value
# does not conform to the spec, the special value :"Speculation/invalid" is
# returned.

# If you don’t want to use the conformed value or check for
# :"Speculation/invalid", the helper valid? can be used instead to return a
# boolean.

S.valid? :even?.to_proc, 10 # => true

# Note that again valid? implicitly converts the predicate function into a
# spec. The spec library allows you to leverage all of the functions you
# already have - there is no special dictionary of predicates. Some more
# examples:

S.valid? :nil?.to_proc, nil               # => true
S.valid? ->(x) { x.is_a?(String) }, "abc" # => true
S.valid? ->(x) { x > 5 }, 10              # => true
S.valid? ->(x) { x > 5 }, 0               # => false

# Regexps, Classes and Modules can be used as predicates.

S.valid? /^\d+$/, "123"        # => true
S.valid? String, "abc"         # => true
S.valid? Enumerable, [1, 2, 3] # => true
S.valid? Date, Date.new        # => true

# Sets can also be used as predicates that match one or more literal values:

S.valid? Set[:club, :diamond, :heart, :spade], :club # => true
S.valid? Set[:club, :diamond, :heart, :spade], 42    # => false
S.valid? Set[42], 42                                 # => true

## Registry

# Until now, we’ve been using specs directly. However, spec provides a central
# registry for globally declaring reusable specs. The registry associates a
# namespaced symbol with a specification. The use of namespaces ensures that
# we can define reusable non-conflicting specs across libraries or
# applications.

# Specs are registered using def. It’s up to you to register the specification
# in a namespace that makes sense (typically a namespace you control).

S.def :date.ns, Date                                 # => :"main/date"
S.def :suit.ns, Set[:club, :diamond, :heart, :spade] # => :"main/suit"

# A registered spec identifier can be used in place of a spec definition in the
# operations we’ve seen so far - conform and valid?.

S.valid? :date.ns, Date.new # => true
S.conform :suit.ns, :club   # => :club

# You will see later that registered specs can (and should) be used anywhere we
# compose specs.

## Composing predicates

# The simplest way to compose specs is with and and or. Let’s create a spec
# that combines several predicates into a composite spec with S.and:

S.def :big_even.ns, S.and(Integer, :even?.to_proc, ->(x) { x > 1000 })
S.valid? :big_even.ns, :foo   # => false
S.valid? :big_even.ns, 10     # => false
S.valid? :big_even.ns, 100000 # => true

# We can also use S.or to specify two alternatives:

S.def :name_or_id.ns, S.or(:name => String, :id => Integer)
S.valid? :name_or_id.ns, "abc" # => true
S.valid? :name_or_id.ns, 100   # => true
S.valid? :name_or_id.ns, :foo  # => false

# This or spec is the first case we’ve seen that involves a choice during
# validity checking. Each choice is annotated with a tag (here, between :name
# and :id) and those tags give the branches names that can be used to
# understand or enrich the data returned from conform and other spec functions.

# When an or is conformed, it returns an array with the tag name and conformed
# value:

S.conform :name_or_id.ns, "abc" # => [:name, "abc"]
S.conform :name_or_id.ns, 100   # => [:id, 100]

# Many predicates that check an instance’s type do not allow nil as a valid
# value (String, ->(x) { x.even? }, /foo/, etc). To include nil as a valid
# value, use the provided function nilable to make a spec:

S.valid? String, nil            # => false
S.valid? S.nilable(String), nil # => true

## Explain

# explain is another high-level operation in spec that can be used to report
# (to STDOUT) why a value does not conform to a spec. Let’s see what explain
# says about some non-conforming examples we’ve seen so far.

S.explain :suit.ns, 42
# val: 42 fails spec: :"main/suit" predicate: #<Set: {:club, :diamond, :heart, :spade}>

S.explain :big_even.ns, 5
# val: 5 fails spec: :"main/big_even" predicate: #<Proc:0x007fcae5e95630(&:even?)>

S.explain :name_or_id.ns, :foo
# val: :foo fails spec: :"main/name_or_id" at: [:name] predicate: String
# val: :foo fails spec: :"main/name_or_id" at: [:id] predicate: Integer

# Let’s examine the output of the final example more closely. First note that
# there are two errors being reported - spec will evaluate all possible
# alternatives and report errors on every path. The parts of each error are:

# - val - the value in the user’s input that does not match
# - spec - the spec that was being evaluated
# - at - a path (an array of symbols) indicating the location within the spec
#   where the error occurred - the tags in the path correspond to any tagged part
#   in a spec (the alternatives in an or or alt, the parts of a cat, the keys in
#   a map, etc)
# - predicate - the actual predicate that was not satsified by val
# - in - the key path through a nested data val to the failing value. In this
#   example, the top-level value is the one that is failing so this is
#   essentially an empty path and is omitted.
# - For the first reported error we can see that the value :foo did not satisfy
#   the predicate String at the path :name in the spec :name-or-id.ns. The second
#   reported error is similar but fails on the :id path instead. The actual value
#   is a Symbol so neither is a match.

# In addition to explain, you can use explain_str to receive the error messages
# as a string or explain_data to receive the errors as data.

S.explain_data :name_or_id.ns, :foo
# => {:"Speculation/problems"=>
#      [{:path=>[:name],
#        :val=>:foo,
#        :via=>[:"main/name_or_id"],
#        :in=>[],
#        :pred=>String},
#       {:path=>[:id],
#        :val=>:foo,
#        :via=>[:"main/name_or_id"],
#        :in=>[],
#        :pred=>Integer}]}

## Entity hashes

# Ruby programs rely heavily on passing around hashes of data. (That may not be
# true...) A common approach in other libraries is to describe each entity
# type, combining both the keys it contains and the structure of their values.
# Rather than define attribute (key+value) specifications in the scope of the
# entity (the hash), specs assign meaning to individual attributes, then
# collect them into shahes using set semantics (on the keys). This approach
# allows us to start assigning (and sharing) semantics at the attribute level
# across our libraries and applications.

# This statement isn't quite true for Ruby's Ring equivalent, Rack:
# ~~For example, most Ring middleware functions modify the request or response
# map with unqualified keys. However, each middleware could instead use
# namespaced keys with registered semantics for those keys. The keys could then
# be checked for conformance, creating a system with greater opportunities for
# collaboration and consistency.~~

# Entity maps in spec are defined with keys:

email_regex = /^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,63}$/

S.def :email_type.ns, S.and(String, email_regex)

S.def :acctid.ns, Integer
S.def :first_name.ns, String
S.def :last_name.ns, String
S.def :email.ns, :email_type.ns

S.def :person.ns, S.keys(:req => [:first_name.ns, :last_name.ns, :email.ns], :opt => [:phone.ns])

# This registers a :person.ns spec with the required keys :first-name.ns,
# :last_name.ns, and :email.ns, with optional key :phone.ns. The hash spec
# never specifies the value spec for the attributes, only what attributes are
# required or optional.

# When conformance is checked on a hash, it does two things - checking that the
# required attributes are included, and checking that every registered key has
# a conforming value. We’ll see later where optional attributes can be useful.
# Also note that ALL attributes are checked via keys, not just those listed in
# the :req and :opt keys. Thus a bare S.keys is valid and will check all
# attributes of a map without checking which keys are required or optional.

S.valid? :person.ns, :first_name.ns => "Elon", :last_name.ns => "Musk", :email.ns => "elon@example.com" # => true

# Fails required key check
S.explain :person.ns, :first_name.ns => "Elon"
# val: {:"main/first_name"=>"Elon"} fails spec: :"main/person" predicate: [:key?, :"main/last_name"]
# val: {:"main/first_name"=>"Elon"} fails spec: :"main/person" predicate: [:key?, :"main/email"]

# Fails attribute conformance
S.explain :person.ns, :first_name.ns => "Elon", :last_name.ns => "Musk", :email.ns => "n/a"
# In: [:"main/email"] val: "n/a" fails spec: :"main/email_type" at: [:"main/email"] predicate: /^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,63}$/

# Let’s take a moment to examine the explain error output on that final example:

# - in - the path within the data to the failing value (here, a key in the person instance)
# - val - the failing value, here "n/a"
# - spec - the spec that failed, here :my.domain/email
# - at - the path in the spec where the failing value is located
# - predicate - the predicate that failed, here (re-matches email-regex %)

# Much existing Ruby code does not use hashes with namespaced keys and so keys
# can also specify :req_un and :opt_un for required and optional unqualified
# keys. These variants specify namespaced keys used to find their
# specification, but the map only checks for the unqualified version of the
# keys.

# Let’s consider a person map that uses unqualified keys but checks conformance
# against the namespaced specs we registered earlier:

S.def :"unq/person", S.keys(:req_un => [:first_name.ns, :last_name.ns, :email.ns],
                            :opt_un => [:phone.ns])

S.conform :"unq/person", :first_name => "Elon", :last_name => "Musk", :email => "elon@example.com"
# => {:first_name=>"Elon", :last_name=>"Musk", :email=>"elon@example.com"}

S.explain :"unq/person", :first_name => "Elon", :last_name => "Musk", :email => "n/a"
# In: [:email] val: "n/a" fails spec: :"main/email_type" at: [:email] predicate: /^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,63}$/

S.explain :"unq/person", :first_name => "Elon"
# val: {:first_name=>"Elon"} fails spec: :"unq/person" predicate: [:key?, :"main/last_nam
# val: {:first_name=>"Elon"} fails spec: :"unq/person" predicate: [:key?, :"main/email"]

# Unqualified keys can also be used to validate record attributes # TODO for objects/structs
# Keyword args keys* - don't support

# Sometimes it will be convenient to declare entity maps in parts, either
# because there are different sources for requirements on an entity map or
# because there is a common set of keys and variant-specific parts. The S.merge
# spec can be used to combine multiple S.keys specs into a single spec that
# combines their requirements. For example consider two keys specs that define
# common animal attributes and some dog-specific ones. The dog entity itself
# can be described as a merge of those two attribute sets:

S.def :"animal/kind", String
S.def :"animal/says", String
S.def :"animal/common", S.keys(:req => [:"animal/kind", :"animal/says"])
S.def :"dog/tail?", :boolean.ns(S)
S.def :"dog/breed", String
S.def :"animal/dog", S.merge(:"animal/common", S.keys(:req => [:"dog/tail?", :"dog/breed"]))

S.valid? :"animal/dog", :"animal/kind" => "dog", :"animal/says" => "woof", :"dog/tail?" => true, :"dog/breed" => "retriever" # => true

## Multi-spec - don't support

## Collections

# A few helpers are provided for other special collection cases - coll_of,
# tuple, and hash_of.

# For the special case of a homogenous collection of arbitrary size, you can
# use coll_of to specify a collection of elements satisfying a predicate.

S.conform S.coll_of(Symbol), [:a, :b, :c] # => [:a, :b, :c]
S.conform S.coll_of(Numeric), Set[5, 10, 2] # => #<Set: {5, 10, 2}>

# Additionally, coll-of can be passed a number of keyword arg options:

# :kind - a predicate or spec that the incoming collection must satisfy, such as `Array`
# :count - specifies exact expected count
# :min_count, :max_count - checks that collection has `count.between?(min_count, max_count)`
# :distinct - checks that all elements are distinct
# :into - one of [], {}, or Set[] for output conformed value. If :into is not specified, the input collection type will be used.

# Following is an example utilizing some of these options to spec an array
# containing three distinct numbers conformed as a set and some of the errors
# for different kinds of invalid values:

S.def :vnum3.ns, S.coll_of(Numeric, :kind => Array, :count => 3, :distinct => true, :into => Set[])
S.conform :vnum3.ns, [1, 2, 3] # => #<Set: {1, 2, 3}>
S.explain :vnum3.ns, Set[1, 2, 3] # not an array
# val: #<Set: {1, 2, 3}> fails spec: :"main/vnum3" predicate: Array
S.explain :vnum3.ns, [1, 1, 1] # not distinct
# val: [1, 1, 1] fails spec: :"main/vnum3" predicate: "distinct?"
S.explain :vnum3.ns, [1, 2, :a] # not a number
# In: [2] val: :a fails spec: :"main/vnum3" predicate: Numeric

# NOTE: Both coll-of and map-of will conform all of their elements, which may
# make them unsuitable for large collections. In that case, consider every or
# for maps every-kv.

# While coll-of is good for homogenous collections of any size, another case is
# a fixed-size positional collection with fields of known type at different
# positions. For that we have tuple.

S.def :point.ns, S.tuple(Float, Float, Float)
S.conform :point.ns, [1.5, 2.5, -0.5] # => [1.5, 2.5, -0.5]

# Note that in this case of a "point" structure with x/y/z values we actually
# had a choice of three possible specs:

# - Regular expression - S.cat :x => Float, :y => Float, :z => Float
#   - Allows for matching nested structure (not needed here)
#   - Conforms to hash with named keys based on the cat tags
# - Collection - S.coll_of Float
#   - Designed for arbitrary size homogenous collections
#   - Conforms to an array of the values
# - Tuple - S.tuple Float, Float, Float
#   - Designed for fixed size with known positional "fields"
#   - Conforms to an array of the values

# In this example, coll_of will match other (invalid) values as well (like
# [1.0] or [1.0 2.0 3.0 4.0]), so it is not a suitable choice - we want fixed
# fields. The choice between a regular expression and tuple here is to some
# degree a matter of taste, possibly informed by whether you expect either the
# tagged return values or error output to be better with one or the other.

# In addition to the support for information hashes via keys, spec also
# provides hash_of for maps with homogenous key and value predicates.

S.def :scores.ns, S.hash_of(String, Integer)
S.conform :scores.ns, "Sally" => 1000, "Joe" => 300 # => {"Sally"=>1000, "Joe"=>300}

# By default hash_of will validate but not conform keys because conformed keys
# might create key duplicates that would cause entries in the map to be
# overridden. If conformed keys are desired, pass the option
# `:conform_keys => # true`.

# You can also use the various count-related options on hash_of that you have
# with coll_of.


## Sequences

# Sometimes sequential data is used to encode additional structure.  spec
# provides the standard regular expression operators to describe the structure
# of a sequential data value:

# - cat - concatenation of predicates/patterns
# - alt - choice among alternative predicates/patterns
# - zero_or_more - 0 or more of a predicate/pattern
# - one_or_more - 1 or more of a predicate/pattern
# - zero_or_one - 0 or 1 of a predicate/pattern

# Like or, both cat and alt tag their "parts" - these tags are then used in the
# conformed value to identify what was matched, to report errors, and more.

# Consider an ingredient represented by an array containing a quantity (number)
# and a unit (symbol). The spec for this data uses cat to specify the right
# components in the right order. Like predicates, regex operators are
# implicitly converted to specs when passed to functions like conform, valid?,
# etc.

S.def :ingredient.ns, S.cat(:quantity => Numeric, :unit => Symbol)
S.conform :ingredient.ns, [2, :teaspoon] # => {:quantity=>2, :unit=>:teaspoon}

# The data is conformed as a hash with the tags as keys. We can use explain to
# examine non-conforming data.

# pass string for unit instead of keyword
S.explain :ingredient.ns, [11, "peaches"]
# In: [1] val: "peaches" fails spec: :"main/ingredient" at: [:unit] predicate: Symbol

# leave out the unit
S.explain :ingredient.ns, [2]
# val: [] fails spec: :"main/ingredient" at: [:unit] predicate: Symbol, "Insufficient input"

# Let’s now see the various occurence operators zero_or_more, one_or_more, and zero_or_one:

S.def :seq_of_symbols.ns, S.zero_or_more(Symbol)
S.conform :seq_of_symbols.ns, [:a, :b, :c] # => [:a, :b, :c]
S.explain :seq_of_symbols.ns, [10, 20]
# In: [0] val: 10 fails spec: :"main/seq_of_symbols" predicate: Symbol

S.def :odds_then_maybe_even.ns, S.cat(:odds => S.one_or_more(:odd?.to_proc),
                                      :even => S.zero_or_one(:even?.to_proc))
S.conform :odds_then_maybe_even.ns, [1, 3, 5, 100] # => {:odds=>[1, 3, 5], :even=>100}

S.conform :odds_then_maybe_even.ns, [1] # => {:odds=>[1]}
S.explain :odds_then_maybe_even.ns, [100]
# In: [0] val: 100 fails spec: :"main/odds_then_maybe_even" at: [:odds] predicate: #<Proc:0x007fcae40f12c8(&:odd?)>

# opts are alternating symbols and booleans
S.def :opts.ns, S.zero_or_more(S.cat(:opt => Symbol, :val => :boolean.ns(S)))
S.conform :opts.ns, [:silent?, false, :verbose, true]
# => [{:opt=>:silent?, :val=>false}, {:opt=>:verbose, :val=>true}]

# Finally, we can use alt to specify alternatives within the sequential data.
# Like cat, alt requires you to tag each alternative but the conformed data is
# a vector of tag and value.

S.def :config.ns, S.zero_or_more(S.cat(:prop => String,
                                       :val => S.alt(:s => String, :b => :boolean.ns(S))))
S.conform :config.ns, ["-server", "foo", "-verbose", true, "-user", "joe"]
# => [{:prop=>"-server", :val=>[:s, "foo"]},
#     {:prop=>"-verbose", :val=>[:b, true]},
#     {:prop=>"-user", :val=>[:s, "joe"]}]

# TODO If you need a description of a specification, use describe to retrieve one.

# Spec also defines one additional regex operator, `constrained`, which takes a
# regex operator and constrains it with one or more additional predicates. This
# can be used to create regular expressions with additional constraints that
# would otherwise require custom predicates. For example, consider wanting to
# match only sequences with an even number of strings:

S.def :even_strings.ns, S.constrained(S.zero_or_more(String), ->(coll) { coll.count.even? })

S.valid? :even_strings.ns, ["a"] # => false
S.valid? :even_strings.ns, ["a", "b"] # => true
S.valid? :even_strings.ns, ["a", "b", "c"] # => false
S.valid? :even_strings.ns, ["a", "b", "c", "d"] # => true

# When regex ops are combined, they describe a single sequence. If you need to
# spec a nested sequential collection, you must use an explicit call to spec to
# start a new nested regex context. For example to describe a sequence like
# [:names, ["a", "b"], :nums, [1 2 3]], you need nested regular expressions to
# describe the inner sequential data:

S.def :nested.ns, S.cat(:names_sym => Set[:names],
                        :names => S.spec(S.zero_or_more(String)),
                        :nums_sym => Set[:nums],
                        :nums => S.spec(S.zero_or_more(Numeric)))

S.conform :nested.ns, [:names, ["a", "b"], :nums, [1, 2, 3]]
# => {:names_sym=>:names,
#     :names=>["a", "b"],
#     :nums_sym=>:nums,
#     :nums=>[1, 2, 3]}


# If the specs were removed this spec would instead match a sequence like
# [:names, "a", "b", :nums, 1, 2, 3].

S.def :unnested.ns, S.cat(:names_sym => Set[:names],
                          :names => S.zero_or_more(String),
                          :nums_sym => Set[:nums],
                          :nums => S.zero_or_more(Numeric))

S.conform :unnested.ns, [:names, "a", "b", :nums, 1, 2, 3]
# => {:names_sym=>:names,
#     :names=>["a", "b"],
#     :nums_sym=>:nums,
#     :nums=>[1, 2, 3]}

## Using spec for validation

# Now is a good time to step back and think about how spec can be used for
# runtime data validation.

# One way to use spec is to explicitly call valid? to verify input data passed
# to a function. ~~You can, for example, use the existing pre- and post-condition
# support built into defn:~~

def self.person_name(person)
  raise "invalid" unless S.valid? :person.ns, person
  name = "#{person[:first_name.ns]} #{person[:last_name.ns]}"
  raise "invalid" unless S.valid? String, name
  name
end

person_name 43 rescue $! # => #<RuntimeError: invalid>
person_name :first_name.ns => "Elon", :last_name.ns => "Musk", :email.ns => "elon@example.com"
# => "Elon Musk"

# When the function is invoked with something that isn’t valid :person.ns data,
# the pre-condition fails. Similarly, if there was a bug in our code and the
# output was not a string, the post-condition would fail.

# Another option is to use S.assert within your code to assert that a value
# satisfies a spec. On success the value is returned and on failure an
# assertion error is thrown. By default assertion checking is off - this can be
# changed by setting S.check_asserts or having the environment variable
# "SPECULATION_CHECK_ASSERTS=true".

def self.person_name(person)
  p = S.assert :person.ns, person
  "#{p[:first_name.ns]} #{p[:last_name.ns]}"
end

S.check_asserts = true
person_name 100 rescue $!
# => #<Speculation::Error: Spec assertion failed
#    val: 100 fails predicate: #<Method: Speculation::Utils.hash?>
#    Speculation/failure :assertion_failed
#     {:"Speculation/problems"=>
#      [{:path=>[],
#        :pred=>#<Method: Speculation::Utils.hash?>,
#        :val=>100,
#        :via=>[],
#        :in=>[]}],
#     :"Speculation/failure"=>:assertion_failed}
#    >

# A deeper level of integration is to call conform and use the return value to
# destructure the input. This will be particularly useful for complex inputs
# with alternate options.

# Here we conform using the config specification defined above:

def self.set_config(prop, val)
  # dummy fn
  puts "set #{prop} #{val}"
end

def self.configure(input)
  parsed = S.conform(:config.ns, input)
  if parsed == :invalid.ns(S)
    raise "Invalid input\n#{S.explain_str(:config.ns, input)}"
  else
    parsed.each do |config|
      prop, val = config.values_at(:prop, :val)
      _type, val = val

      set_config(prop[1..-1], val)
    end
  end
end

configure ["-server", "foo", "-verbose", true, "-user", "joe"]
# set server foo
# set verbose true
# set user joe

# Here configure calls conform to destructure the config input. The result is
# either the special :invalid.n(S) value or a destructured form of the result:

[{:prop=>"-server", :val=>[:s, "foo"]},
 {:prop=>"-verbose", :val=>[:b, true]},
 {:prop=>"-user", :val=>[:s, "joe"]}]

# In the success case, the parsed input is transformed into the desired shape
# for further processing. In the error case, we call explain_str to generate
# an error message. The explain string contains information about what
# expression failed to conform, the path to that expression in the
# specification, and the predicate it was attempting to match.

## Spec’ing methods

# The pre- and post-condition example in the previous section hinted at an
# interesting question - how do we define the input and output specifications
# for a method.

# Spec has explicit support for this using fdef, which defines specifications
# for a function - the arguments and/or the return value spec, and optionally a
# function that can specify a relationship between args and return.

# Let’s consider a ranged-rand function that produces a random number in a
# range:

def self.ranged_rand(from, to)
  rand(from...to)
end

# We can then provide a specification for that function:

S.fdef(method(:ranged_rand),
       :args => S.and(S.cat(:start => Integer, :end => Integer), ->(args) { args[:start] < args[:end] }),
       :ret => Integer,
       :fn => S.and(->(fn) { fn[:ret] >= fn[:args][:start] },
                    ->(fn) { fn[:ret] < fn[:args][:end] }))

# This function spec demonstrates a number of features. First the :args is a
# compound spec that describes the function arguments. This spec is invoked
# with the args in an array, as if they were invoked like `method.call(*args)`
# Because the args are sequential and the args are positional fields, they are
# almost always described using a regex op, like cat, alt, or zero_or_more.

# The second :args predicate takes as input the conformed result of the first
# predicate and verifies that start < end. The :ret spec indicates the return
# is also an integer. Finally, the :fn spec checks that the return value is >=
# start and < end.

# We’ll see later how we can use a function spec for development and testing.

## Higher order functions

# Higher order functions are common in ~Clojure~ Ruby and spec provides fspec
# to support spec’ing them.

# For example, consider the adder function:

def self.adder(x)
  ->(y) { x + y }
end

# adder returns a proc that adds x. We can declare a function spec for adder
# using fspec for the return value:

S.fdef method(:adder),
  :args => S.cat(:x => Numeric),
  :ret => S.fspec(:args => S.cat(:y => Numeric), :ret => Numeric),
  :fn => ->(fn) { fn[:args][:x] == fn[:ret].call(0) }

# The :ret spec uses fspec to declare that the returning function takes and
# returns a number. Even more interesting, the :fn spec can state a general
# property that relates the :args (where we know x) and the result we get from
# invoking the function returned from adder, namely that adding 0 to it should
# return x.

## Macros - noop

## A game of cards

# Here’s a bigger set of specs to model a game of cards:

suit = Set[:club, :diamond, :heart, :spade]
rank = Set[:jack, :queen, :king, :ace].merge(2..10)
deck = rank.to_a.product(suit.to_a)

S.def :card.ns, S.tuple(rank, suit)
S.def :hand.ns, S.zero_or_more(:card.ns)

S.def :name.ns, String
S.def :score.ns, Integer
S.def :player.ns, S.keys(:req => [:name.ns, :score.ns, :hand.ns])

S.def :players.ns, S.zero_or_more(:player.ns)
S.def :deck.ns, S.zero_or_more(:card.ns)
S.def :game.ns, S.keys(:req => [:players.ns, :deck.ns])

# We can validate a piece of this data against the schema:

kenny = { :name.ns => "Kenny Rogers",
          :score.ns => 100,
          :hand.ns => [] }
S.valid? :player.ns, kenny
# => true

# Or look at the errors we’ll get from some bad data:

S.explain :game.ns,
  :deck.ns => deck,
  :players.ns => [{:name.ns => "Kenny Rogers",
                   :score.ns => 100,
                   :hand.ns => [[2, :banana]]}]
# In: [:"main/players", 0, :"main/hand", 0, 1] val: :banana fails spec: :"main/card"
#   at: [:"main/players", :"main/hand", 1] predicate: #<Set: {:club, :diamond, :heart, :spade}>

# The error indicates the key path in the data structure down to the invalid
# value, the non-matching value, the spec part it’s trying to match, the path
# in that spec, and the predicate that failed.

# If we have a function `deal` that doles out some cards to the players we can
# spec that function to verify the arg and return value are both suitable
# data values. We can also specify a :fn spec to verify that the count of
# cards in the game before the deal equals the count of cards after the deal.

def self.total_cards(game)
  game, players = game.values_at(:game.ns, :players.ns)
  players.map { |player| player[:hand.ns].count }.reduce(deck.count, &:+)
end

def self.deal(game)
  # ...
end

S.fdef method(:deal),
  :args => S.cat(:game => :game.ns),
  :ret => :game.ns,
  :fn => ->(fn) { total_cards(fn[:args][:game]) == total_cards(fn[:ret]) }

## Generators

# A key design constraint of spec is that all specs are also designed to act as
# generators of sample data that conforms to the spec (a critical requirement
# for property-based testing).

## ~~Project Setup~~

# Nothing to do, Rantly is included by default. May look at removing the hard
# dependency in the future.

# In your code you also need to require the speculation/gen lib.

require 'speculation/gen'
Gen = S::Gen

## Sampling Generators

# The gen function can be used to obtain the generator for any spec.

# Once you have obtained a generator with gen, there are several ways to use
# it. You can generate a single sample value with generate or a series of
# samples with sample. Let’s see some basic examples:

Gen.generate S.gen(Integer) # => 101533682366278051
Gen.generate S.gen(NilClass) # => nil
Gen.sample S.gen(String)
# => ["vKAIxzHpwPFXanvXtzBhUCjiuaIYuWGQtDKzEnlLIkkJvS",
#     "KADEcthLoNmLUhADjvWgNORLVbRIkvUgtBPKyacWXLLuIcBaxRLNDHRmALgtHfMnaecdXBlip",
#     "JzFCVrmVwwuorGFptejmSjyrieg",
#     "AYfYJqJpryHNiDKAVDfKzFxvAzfrIALpZUjrhSBELyFtjTzsruaIRNXWQTBpgcfdrbTu",
#     "VdltmRYHWALsRaKDtALQKYMrijGud",
#     "KPOPcHQobCOIzLbYeVSkagxfTtXrOwIEaGPfFnxGAyyYXAmUKEiCaRKxPEiQOwZMWqrMBIdpnMQEV",
#     "nrYVSgGKjQxNSxvdPavNqmdUUsjtOnoMRbxYrlqWPFDHyfqSGNkDLvFMQggNhfkDRfJldQmlayjczARXGUAuWLED",
#     "mGpPwAcNEJOYokyqeOuUMPWlwEUUwcXaiwoHbrwAfgtrnBGEJAqDlDzxwnzTyvFQwliJPmkHelRGeUiwoXA",
#     "XcjULALoTDUsjxlAiJIoUsoRE",
#     "QbzotQnqTCqvJBKw"]
Gen.sample S.gen(Set[:club, :diamond, :heart, :spade])
# => [:spade,
#     :diamond,
#     :club,
#     :diamond,
#     :spade,
#     :diamond,
#     :club,
#     :diamond,
#     :club,
#     :club]

Gen.sample S.gen(S.cat(:k => Symbol, :ns => S.one_or_more(Numeric))), 4
# => [[:mfkTaVwEtqvqQkd,
#      785038047325449334,
#      1.3917109946487061e+308,
#      -1521062231591229587,
#      1.2763219750347474e+308,
#      1.0841264271653725e+308,
#      2263279212308344509,
#      2024574300205329126,
#      1596361936401711048,
#      1.4768157060128298e+308,
#      2181185415794069894,
#      -317509953325950941,
#      1.8902659197111178e+307,
#      8.345515337122378e+307,
#      -1057856305120743584,
#      352736258842677408,
#      1.6504557173130976e+307,
#      1.6403270281173023e+308,
#      6.401620982986638e+307],
#     [:"",
#      766854880238302246,
#      942011850378881840,
#      929481294280159485,
#      9.772995599358412e+307,
#      1755878457464632355,
#      9.620417546697921e+306,
#      -2078646389841231742,
#      1973513907214206944,
#      1.3115503401331354e+308,
#      9.691072571071856e+307,
#      461788041197905132,
#      1.3292773658639617e+307,
#      1.4855839749703923e+308],
#     [:rXhQPEtVK,
#      8.484310546252002e+307,
#      1.4253347072743232e+308,
#      694562331015493637,
#      -1544158012224760424,
#      451768232373123609,
#      -854021458827412027,
#      327788231443556725,
#      1835548298263216042,
#      6.119439415612837e+307,
#      1.0994431194900366e+307,
#      1.0989494128109166e+308,
#      -1250242486936812343,
#      2.393715471161461e+307,
#      1.7753518305080706e+308,
#      1.3806537839770025e+308,
#      5.554061810942785e+307,
#      9.557252602875436e+307,
#      1.3896678201297536e+308,
#      5.763035951315998e+307],
#     [:ZFHAsTAytRJrHxqqVMSJcwhYpviGWHSLzVoXuckIqxnynAEIOVydaUvYXzOkIMkbcFhEEPclnqYtncsRjJHG,
#      -1187362297247568538,
#      2.539492624075924e+307,
#      -909880958371928784,
#      1946378556065633017,
#      -807202725605527243,
#      1.2821661929334862e+308,
#      -1091082753506123629,
#      -1516892403112377293,
#      -766875069233010644,
#      -901264045194403643,
#      9.31210805000969e+307,
#      6.666658940971819e+307,
#      8.920130300062669e+307,
#      -1090597336365396779]]

# What about generating a random player in our card game?

Gen.generate S.gen(:player.ns)
# => {:"main/name"=>
#      "UjjaYmTOoOsvjoRMLBRbnwSPFFEBivdAHwHGUCPrUoBujGCAxQxdqtNwoxWQrFVvZG",
#     :"main/score"=>-2033370506903562773,
#     :"main/hand"=>
#      [[:jack, :diamond],
#       [:queen, :spade],
#       [:jack, :club],
#       [6, :diamond],
#       [9, :diamond],
#       [9, :heart],
#       [7, :diamond],
#       [:jack, :diamond],
#       [2, :spade],
#       [4, :heart],
#       [:queen, :club],
#       [:jack, :heart],
#       [:king, :club]]}

# What about generating a whole game?

Gen.generate S.gen(:game.ns)
# it works! but the output is really long, so not including it here

# So we can now start with a spec, extract a generator, and generate some data.
# All generated data will conform to the spec we used as a generator. For specs
# that have a conformed value different than the original value (anything using
# S.or, S.cat, S.alt, etc) it can be useful to see a set of generated samples
# plus the result of conforming that sample data.

## Exercise

# For this we have `exercise`, which returns pairs of generated and conformed
# values for a spec. exercise by default produces 10 samples (like sample) but
# you can pass both functions a number indicating the number of samples to
# produce.

S.exercise S.cat(:k => Symbol, :ns => S.one_or_more(Numeric)), :n => 5
# => [[[:bo,
#       1463892444701664627,
#       -223413012796100214,
#       1.5792433294015037e+308,
#       1.610923064142933e+308,
#       1657766008412340583,
#       1.4179850795007242e+306,
#       1.6577343480290446e+308,
#       2276418178125115184,
#       1.3910880063346549e+308],
#      {:k=>:bo,
#       :ns=>
#        [1463892444701664627,
#         -223413012796100214,
#         1.5792433294015037e+308,
#         1.610923064142933e+308,
#         1657766008412340583,
#         1.4179850795007242e+306,
#         1.6577343480290446e+308,
#         2276418178125115184,
#         1.3910880063346549e+308]}],
#     [[:hApatZTkDAMieEbfFmJcvLwcWlrMIVmXJiSlRWsgYXMULlIZqokLeCLvFbrylWwOalxMw,
#       -514176434692853886,
#       1865627020417189350,
#       959108596114486577,
#       2216272197846720013,
#       6.659674129679428e+307,
#       1199543011401067135,
#       1.5257960638708663e+308,
#       -637347056109277797,
#       1.5551259304659426e+308,
#       1.0684716832172856e+308,
#       7.361646931240551e+307],
#      {:k=>
#        :hApatZTkDAMieEbfFmJcvLwcWlrMIVmXJiSlRWsgYXMULlIZqokLeCLvFbrylWwOalxMw,
#       :ns=>
#        [-514176434692853886,
#         1865627020417189350,
#         959108596114486577,
#         2216272197846720013,
#         6.659674129679428e+307,
#         1199543011401067135,
#         1.5257960638708663e+308,
#         -637347056109277797,
#         1.5551259304659426e+308,
#         1.0684716832172856e+308,
#         7.361646931240551e+307]}],
#     [[:yvyaFGEldujDYjwTgHGwFGhJIypXkFNZTK,
#       314488453087577894,
#       5.15094067372987e+307,
#       2232468854633935622,
#       1.11256687009303e+307,
#       8.935244014175055e+307,
#       560354306548680298,
#       1.0366873873238362e+305,
#       1.2109415737889356e+307],
#      {:k=>:yvyaFGEldujDYjwTgHGwFGhJIypXkFNZTK,
#       :ns=>
#        [314488453087577894,
#         5.15094067372987e+307,
#         2232468854633935622,
#         1.11256687009303e+307,
#         8.935244014175055e+307,
#         560354306548680298,
#         1.0366873873238362e+305,
#         1.2109415737889356e+307]}],
#     [[:iFtWncuSVKtXeTDshNarwFyEaiKPDkQ,
#       7.479487276695644e+307,
#       4.821863713974112e+307,
#       1.4732751753845218e+308,
#       -1559532711167233828,
#       1.6020124909781268e+308,
#       971666718121470770,
#       1.7911382456125838e+308,
#       1116724619180799030,
#       523524334743677229,
#       1116558779091435689,
#       1.8096995085140938e+307,
#       1240362303674259373,
#       7.272490279773022e+307,
#       7.443721108202123e+307,
#       1.9978948644818407e+307,
#       5.538278067848909e+307,
#       -2161555562597923938,
#       1.6898569954549462e+308,
#       -989934453942479874,
#       -1516237200335339771,
#       1440227459508576372],
#      {:k=>:iFtWncuSVKtXeTDshNarwFyEaiKPDkQ,
#       :ns=>
#        [7.479487276695644e+307,
#         4.821863713974112e+307,
#         1.4732751753845218e+308,
#         -1559532711167233828,
#         1.6020124909781268e+308,
#         971666718121470770,
#         1.7911382456125838e+308,
#         1116724619180799030,
#         523524334743677229,
#         1116558779091435689,
#         1.8096995085140938e+307,
#         1240362303674259373,
#         7.272490279773022e+307,
#         7.443721108202123e+307,
#         1.9978948644818407e+307,
#         5.538278067848909e+307,
#         -2161555562597923938,
#         1.6898569954549462e+308,
#         -989934453942479874,
#         -1516237200335339771,
#         1440227459508576372]}],
#     [[:nNGSxnNexBcejmDxEfzwjBziDKglXucxJTJZfXfTzZVTUDyWJFsoyzBXTs,
#       -1114563816114502457,
#       4.737744516854147e+307,
#       -2237880295799672307,
#       -1272066546100747522,
#       1.7788359046351407e+307,
#       -1742766254376832245,
#       1.3739295101881748e+308,
#       1685431259756761318,
#       9.257193513095757e+307,
#       1.0332108988295095e+308,
#       1.1576820563713511e+308,
#       1.3014496564984251e+308,
#       881940375380007673,
#       1196335558718210142,
#       456727343505389696,
#       3.146138337741874e+307,
#       6.43797127890011e+307,
#       -179418724523541049],
#      {:k=>:nNGSxnNexBcejmDxEfzwjBziDKglXucxJTJZfXfTzZVTUDyWJFsoyzBXTs,
#       :ns=>
#        [-1114563816114502457,
#         4.737744516854147e+307,
#         -2237880295799672307,
#         -1272066546100747522,
#         1.7788359046351407e+307,
#         -1742766254376832245,
#         1.3739295101881748e+308,
#         1685431259756761318,
#         9.257193513095757e+307,
#         1.0332108988295095e+308,
#         1.1576820563713511e+308,
#         1.3014496564984251e+308,
#         881940375380007673,
#         1196335558718210142,
#         456727343505389696,
#         3.146138337741874e+307,
#         6.43797127890011e+307,
#         -179418724523541049]}]]

S.exercise S.or(:k => Symbol, :s => String, :n => Numeric), :n => 5
# => [["guATIoZfeTBNOFOyTlUMsz", [:s, "guATIoZfeTBNOFOyTlUMsz"]],
#     [:tjoDrWkRzcvDyPmDDAQNMGAgOxbRMItpGihPWDgxjshwUczyI,
#      [:k, :tjoDrWkRzcvDyPmDDAQNMGAgOxbRMItpGihPWDgxjshwUczyI]],
#     [:BVCVLuaKUssXZFdXsbOCGqFDIylwhXxCMoMPSpshJJiKmwtFijtckOonLCcnEKZtidMj,
#      [:k,
#       :BVCVLuaKUssXZFdXsbOCGqFDIylwhXxCMoMPSpshJJiKmwtFijtckOonLCcnEKZtidMj]],
#     ["IGUD", [:s, "IGUD"]],
#     ["uTMKikcMOKDVdLcscCaGactqpKvDKaExjsesTMUounsAClWNdScDRXCNXGHosSUWbjAviUxgTmxWTNszoUwCEgdWZMVdvbgEor",
#      [:s,
#       "uTMKikcMOKDVdLcscCaGactqpKvDKaExjsesTMUounsAClWNdScDRXCNXGHosSUWbjAviUxgTmxWTNszoUwCEgdWZMVdvbgEor"]]]

# For spec’ed functions we also have exercise_fn, which generates sample args,
# invokes the spec’ed function and returns the args and the return value.

S.exercise_fn(method(:ranged_rand))
# => [[[105523704152181308, 902524208580306271], 417575463673633645],
#     [[-1474033643201677753, 1524874458384520781], 564749411953910859],
#     [[14335282605047499, 1273485159970538490], 175852715324228102],
#     [[-1840928134676403412, 1338626244236774656], -354255994108763803],
#     [[360581186623634300, 1719426307113616014], 1099191926715032411],
#     [[-2224600893524833614, 1377610100502056156], -981931028223592171],
#     [[-2263933558146382486, 166200818275433569], -2161455286334391552],
#     [[661601872547441632, 2055004444838350727], 1555316469528393805],
#     [[-1807812605514883868, 2162684301857665059], -932983154168774228],
#     [[109992734067055886, 756034754795999173], 677372220967499657]]

## Using S.and Generators

# All of the generators we’ve seen worked fine but there are a number of cases
# where they will need some additional help. One common case is when the
# predicate implicitly presumes values of a particular type but the spec does
# not specify them:

Gen.generate S.gen(:even?.to_proc) rescue $! # => #<Speculation::Error: unable to construct gen at: [] for: Speculation::Spec(#<Proc:0x007f8133386510(&:even?)>) {:"Speculation/failure"=>:no_gen, :"Speculation/path"=>[]}\n>

# In this case spec was not able to find a generator for the even? predicate.
# Most of the primitive generators in spec are mapped to the common type
# predicates (classes, modules, built-in specs).

# However, spec is designed to support this case via `and` - the first
# predicate will determine the generator and subsequent branches will act as
# filters by applying the predicate to the produced values.

# If we modify our predicate to use an `and` and a predicate with a mapped
# generator, the even? can be used as a filter for generated values instead:

Gen.generate S.gen(S.and(Integer, :even?.to_proc))
# => -768509797648624122

# We can use many predicates to further refine the generated values. For
# example, say we only wanted to generate numbers that were positive multiples
# of 3:

def self.divisible_by(n)
  ->(x) { (x % n).zero? }
end

Gen.sample S.gen(S.and(Integer, :positive?.to_proc, divisible_by(3)))
# => [2279348553340341591,
#     383614552056188997,
#     661980116865706620,
#     1155839998718034606,
#     1327642894510908738,
#     643657279472815338,
#     2302095558878653278,
#     193501256410698852,
#     554626534578511173,
#     2192416193987096517]


# However, it is possible to go too far with refinement and make something that
# fails to produce any values. The Rantly `guard` that implements the
# refinement will throw an error if the refinement predicate cannot be resolved
# within a relatively small number of attempts. For example, consider trying to
# generate strings that happen to contain the world "hello":

# hello, are you the one I'm looking for?
Gen.sample S.gen(S.and(String, ->(s) { s.include?("hello") })) rescue $!
# => #<Rantly::TooManyTries: Exceed gen limit 100: 101 failed guards)>

# Given enough time (maybe a lot of time), the generator probably would come up
# with a string like this, but the underlying `guard` will make only 100
# attempts to generate a value that passes the filter. This is a case where you
# will need to step in and provide a custom generator.

## Custom Generators

# Building your own generator gives you the freedom to be either narrower
# and/or be more explicit about what values you want to generate. Alternately,
# custom generators can be used in cases where conformant values can be
# generated more efficiently than using a base predicate plus filtering. Spec
# does not trust custom generators and any values they produce will also be
# checked by their associated spec to guarantee they pass conformance.

# There are three ways to build up custom generators - in decreasing order of
# preference:

# - Let spec create a generator based on a predicate/spec
# - Create your own generator using Rantly directly

# First consider a spec with a predicate to specify symbols from a particular
# namespace:

S.def :syms.ns, S.and(Symbol, ->(s) { s.namespace == "my.domain" })
S.valid? :syms.ns, :"my.domain/name" # => true
Gen.sample S.gen(:syms.ns) rescue $! # => #<Rantly::TooManyTries: Exceed gen limit 100: 101 failed guards)>

# The simplest way to start generating values for this spec is to have spec
# create a generator from a fixed set of options. A set is a valid predicate
# spec so we can create one and ask for it’s generator:

sym_gen = S.gen(Set[:"my.domain/name", :"my.domain/occupation", :"my.domain/id"])
Gen.sample sym_gen, 5
# => [:"my.domain/id",
#     :"my.domain/id",
#     :"my.domain/id",
#     :"my.domain/name",
#     :"my.domain/name"]

# To redefine our spec using this custom generator, use with_gen which takes a
# spec and a replacement generator as a block:

gen = S.gen(Set[:"my.domain/name", :"my.domain/occupation", :"my.domain/id"])
S.def(:syms.ns, S.with_gen(S.and(Symbol, ->(s) { s.namespace == "my.domain" }), gen))

S.valid? :syms.ns, :"my.domain/name"
Gen.sample S.gen(:syms.ns), 5
# => [:"my.domain/id",
#     :"my.domain/name",
#     :"my.domain/name",
#     :"my.domain/occupation",
#     :"my.domain/id"]

# TODO: make gens no-arg functions???
# Note that with_gen (and other places that take a custom generator) take a
# one-arg function that returns the generator, allowing it to be lazily
# realized.

# One downside to this approach is we are missing what property testing is
# really good at: automatically generating data across a wide search space to
# find unexpected problems.

# Rantly has a small library of generators that can be utilized.

# In this case we want our keyword to have open names but fixed namespaces.
# There are many ways to accomplish this but one of the simplest is to use fmap
# to build up a keyword based on generated strings:

sym_gen_2 = ->(rantly) do
  size = rantly.range(1, 10)
  string = rantly.sized(size) { rantly.string(:alpha) }
  :"my.domain/#{string}"
end
Gen.sample sym_gen_2, 5 # => [:"my.domain/hBnVBsXw", :"my.domain/Wd", :"my.domain/ghGO", :"my.domain/FVIFcK", :"my.domain/Ulgu"]

# Returning to our "hello" example, we now have the tools to make that
# generator:

S.def :hello.ns, S.with_gen(->(s) { s.include?("hello") }, ->(rantly) {
  s1 = rantly.sized(rantly.range(0, 10)) { rantly.string(:alpha) }
  s2 = rantly.sized(rantly.range(0, 10)) { rantly.string(:alpha) }
  "#{s1}hello#{s2}"
})

Gen.sample S.gen(:hello.ns)
# => ["FlBAUShelloUXbEJUkYos",
#     "ObhellojWzf",
#     "vhelloNHcFFMd",
#     "NhelloXb",
#     "HJhQeCBhellopGte",
#     "AYVDihelloCPQnwdxln",
#     "hello",
#     "HOggTyFLzhelloIsQwzPH",
#     "DOKEsulmLFhellowX",
#     "TdiBhelloSl"]

# Here we generate a tuple of a random prefix and random suffix strings, then
# insert "hello" bewteen them.

## Range Specs and Generators

# There are several cases where it’s useful to spec (and generate) values in a
# range and spec provides helpers for these cases.

# For example, in the case of a range of integer values (for example, a bowling
# roll), use int_in to spec a range:

S.def :roll.ns, S.int_in(0..10)
Gen.sample S.gen(:roll.ns)
# => [8, 5, 4, 0, 5, 6, 8, 9, 7, 4]

# spec also includes date_in for a range of dates:

S.def :the_aughts.ns, S.date_in(Date.new(2000, 1, 1)..Date.new(2010))
Gen.sample S.gen(:the_aughts.ns), 5
# => [#<Date: 2006-07-16 ((2453933j,0s,0n),+0s,2299161j)>,
#     #<Date: 2009-06-28 ((2455011j,0s,0n),+0s,2299161j)>,
#     #<Date: 2008-01-28 ((2454494j,0s,0n),+0s,2299161j)>,
#     #<Date: 2001-08-28 ((2452150j,0s,0n),+0s,2299161j)>,
#     #<Date: 2003-10-08 ((2452921j,0s,0n),+0s,2299161j)>]

# spec also includes time_in for a range of times:

S.def :the_aughts.ns, S.time_in(Time.new(2000)..Time.new(2010))
Gen.sample S.gen(:the_aughts.ns), 5
# => [2005-09-19 12:06:12 -0700,
#     2001-11-28 10:20:15 -0800,
#     2001-01-31 20:53:43 -0800,
#     2005-08-09 19:43:13 -0700,
#     2006-11-30 14:36:57 -0800]

# Finally, float_in has support for double ranges and special options for
# checking special float values like NaN (not a number), Infinity, and
# -Infinity.

S.def :floats.ns, S.float_in(:min => -100.0, :max => 100.0, :nan => false, :infinite => false)
S.valid? :floats.ns, 2.9             # => true
S.valid? :floats.ns, Float::INFINITY # => false
Gen.sample S.gen(:floats.ns), 5      # => [64.25166050363904, 41.955853107291716, 51.36584394868757, -33.25772454323295, -24.26376015534997]

## Instrumentation and Testing

# spec provides a set of development and testing functionality in the
# Speculation::Test namespace, which we can include with:

require 'speculation/test'
STest = Speculation::Test

## Instrumentation

# Instrumentation validates that the :args spec is being invoked on
# instrumented functions and thus provides validation for external uses of a
# function. Let’s turn on instrumentation for our previously spec’ed
# ranged-rand function:

STest.instrument method(:ranged_rand)

# If the function is invoked with args that do not conform with the :args spec
# you will see an error like this:

ranged_rand 8, 5 rescue $!
# => #<Speculation::Error: Call to 'main.ranged_rand' did not conform to spec:
#     val: {:start=>8, :end=>5} fails at: [:args] predicate: #<Proc:0x007f8134057e88@/var/folders/4l/j2mycv0j4rx7z47sp01r93vc3kfxzs/T/seeing_is_believing_temp_dir20170220-7539-1q9r5n7/program.rb:546 (lambda)>
#    Speculation/args [8, 5]
#    Speculation/failure :instrument
#    Speculation::Test/caller "/var/folders/4l/j2mycv0j4rx7z47sp01r93vc3kfxzs/T/seeing_is_believing_temp_dir20170220-7539-1q9r5n7/program.rb:900:in `<main>'"
#     {:"Speculation/problems"=>
#      [{:path=>[:args],
#        :val=>{:start=>8, :end=>5},
#        :via=>[],
#        :in=>[],
#        :pred=>
#         #<Proc:0x007f8134057e88@/var/folders/4l/j2mycv0j4rx7z47sp01r93vc3kfxzs/T/seeing_is_believing_temp_dir20170220-7539-1q9r5n7/program.rb:546 (lambda)>}],
#     :"Speculation/args"=>[8, 5],
#     :"Speculation/failure"=>:instrument,
#     :"Speculation::Test/caller"=>
#      "/var/folders/4l/j2mycv0j4rx7z47sp01r93vc3kfxzs/T/seeing_is_believing_temp_dir20170220-7539-1q9r5n7/program.rb:900:in `<main>'"}
#    >

# The error fails in the second args predicate that checks `start < end`. Note
# that the :ret and :fn specs are not checked with instrumentation as
# validating the implementation should occur at testing time.

# Instrumentation can be turned off using the complementary function
# unstrument. Instrumentation is likely to be useful at both development time
# and during testing to discover errors in calling code. It is not recommended
# to use instrumentation in production due to the overhead involved with
# checking args specs.

## Testing

# We mentioned earlier that ~~clojure.spec.test~~ Speculation provides tools
# for automatically testing functions. When functions have specs, we can use
# check, to automatically generate tests that check the function using the
# specs.

# check will generate arguments based on the :args spec for a function, invoke
# the function, and check that the :ret and :fn specs were satisfied.

STest.check method(:ranged_rand)
# => [{:spec=>Speculation::FSpec(main.ranged_rand),
#      :"Speculation::Test/ret"=>{:num_tests=>1000, :result=>true},
#      :method=>#<Method: main.ranged_rand>}]

# check also takes a number of options ~~that can be passed to test.check to
# influence the test run~~, as well as the option to override generators for
# parts of the spec, by either name or path.

# Imagine instead that we made an error in the ranged-rand code and swapped
# start and end:

def self.ranged_rand(from, to) ## broken!
  (from + rand(to)).to_i
end

# This broken function will still create random integers, just not in the
# expected range. Our :fn spec will detect the problem when checking the var:

STest.abbrev_result STest.check(method(:ranged_rand)).first
# {:spec=>"Speculation::FSpec(main.ranged_rand)",
#  :method=>#<Method: main.ranged_rand>,
#  :failure=>
#   {:"Speculation/problems"=>[{:path=>[:fn], :val=>{:args=>{:start=>-1, :end=>0}, :block=>nil, :ret=>0}, :via=>[], :in=>[], :pred=>#<Proc:0x007fa48e051df8@(pry):424 (lambda)>}],
#    :"Speculation::Test/args"=>[-1, 0],
#    :"Speculation::Test/val"=>{:args=>{:start=>-1, :end=>0}, :block=>nil, :ret=>0},
#    :"Speculation/failure"=>:check_failed}}

# check has reported an error in the :fn spec. We can see the arguments passed
# were -1 and 0 and the return value was -0, which is out of the expected
# range (it should be less that the `end` argument).

# To test all of the spec’ed functions in a module/class (or multiple
# module/classes), use enumerate-methods to generate the set of symbols
# naming vars in the namespace:

STest.check STest.enumerate_methods(self)

# And you can check all of the spec’ed functions by calling STest.check without any arguments.


## Combining check and instrument
