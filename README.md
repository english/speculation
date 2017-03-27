# Speculation [![Build Status](https://travis-ci.org/english/speculation.svg?branch=master)](https://travis-ci.org/english/speculation)

A Ruby port of Clojure's `clojure.spec`. See [clojure.spec - Rationale and Overview](https://clojure.org/about/spec). All advantages/disadvantages for clojure.spec should apply to Speculation too. This library is largely a copy-and-paste from clojure.spec so all credit goes to the clojure.spec authors.

## Project Goals

The goal of this project is to match clojure.spec as closely as possible, from design to features to API. This decision comes with the trade-off that the library may not necessarily be idiomatic Ruby, however there's nothing stopping other libraries from being built on top of Speculation to bring a more Ruby-like feel. This library won't introduce features that do not exist in clojure.spec.

## Examples

- [sinatra-web-app](examples/sinatra-web-app): A small sinatra web application demonstrating model validation and API error message generation.
- [spec_guide.rb](examples/spec_guide.rb): Speculation port of Clojure's [spec guide](https://clojure.org/guides/spec)
- [codebreaker.rb](examples/codebreaker.rb): Speculation port of the 'codebreaker' game described in [Interactive development with clojure.spec](http://blog.cognitect.com/blog/2016/10/5/interactive-development-with-clojurespec)
- [json_parser.rb](examples/json_parser.rb): JSON parser using Speculation.

## Usage

Documentation is available at [RubyDoc](http://www.rubydoc.info/github/english/speculation). The API is more-or-less the same as `clojure.spec`. If you're already familiar clojure.spec with then you should feel at home with Speculation. Most guides, talks and discussion around clojure.spec should apply equally well to Speculation. Clojure and Ruby and quite different languages, so naturally there are some differences:

## Differences with clojure.spec

### Built in predicates

clojure.spec leans on its macro system and rich standard library of predicate functions when writing specs. Ruby has neither of those, so we must be creative with what we define as a 'predicate' in Speculation. Each of the following are valid Speculation predicates:

```rb
S.valid?(->(x) { x > 0 }, 2)
S.valid?(:even?.to_proc, 2)
S.valid?(String, "foo")
S.valid?(Enumerable, [1, 2, 3])
S.valid?(/^\d+$/, "123")
S.valid?(Set[:foo, :bar, :baz], :foo)
```

### Namespaced keywords/symbols

Namespaced keywords are at the core of `clojure.spec`. Since clojure.spec utilises a global spec registry, namespaced keywords allow libraries to register specs with the same names but under different namespaces, thus removing accidental collisions. Ruby's equivalent to Clojure's keywords are Symbols. Ruby Symbol's don't have namespaces.

In order keep the global spec registry architecture in Speculation, we utilise a helper method `ns` to achieve similar behaviour:

```rb
module MyModule
  extend Speculation::NamespacedSymbols

  p ns(:foo)
  # => :"MyModule/foo"

  p ns(AnotherModule, :foo)
  # => :"AnotherModule/foo"
end
```

### FSpecs

#### Symbols/Methods

Clojure uses Symbols to refer to functions. To refer to a method in Ruby, we must use the `method` method.

```rb
def self.hello(name)
  "Hello #{name}"
end

S.fdef(method(:hello), :args => S.cat(:name => String), :ret => String)
```

#### Block args

In addition to regular arguments which can easily be described as a list, Ruby methods can take blocks. In Speculation, we spec a method's block separately to its args:

```rb
def self.hello(name, &block)
  "Hello #{block.call(name)}"
end

S.fdef(method(:hello), :args => S.cat(:name => String),
                       :block => S.fspec(:args => S.cat(:s => String), :ret => String),
                       :ret => String)
```

#### Generators and quick check

Speculation uses [`Rantly`](https://github.com/abargnesi/rantly) for random data generation. Generator functions in Speculation are Procs that take one argument (Rantly instance) and return a random value. While clojure's test.check generators generate values that start small and continue to grow and get more complex as a property holds true, Rantly always generates random values.

Rantly gives Speculation the ability to shrink a failing test case down to its smallest failing case, however in Speculation we limit this to Integers and Strings. This is an area where Speculation may currently be significantly weaker than clojure.spec.

## Development

After checking out the repo, run `bin/setup` to install dependencies. Then, run `rake` to run rubocop and the tests. You can also run `bin/console` for an interactive prompt that will allow you to experiment.

## Contributing

Bug reports and pull requests are welcome on GitHub at https://github.com/english/speculation.

## TODO

- tidy up tests

### clojure.spec features

- [`unform`](https://clojuredocs.org/clojure.spec/unform)
- [`form`](https://clojuredocs.org/clojure.spec/form)
- [`abbrev`](https://clojuredocs.org/clojure.spec/abbrev)
- [`describe`](https://clojuredocs.org/clojure.spec/describe)

### Improvements

- Explore alternative generator library
  - Build up a library of generators around Rantly in the meantime?
- Generate documentation from specs
  - perhaps integrating with [Pry's documentation browsing](https://github.com/pry/pry/wiki/Documentation-browsing)?
- Profile and optimise
