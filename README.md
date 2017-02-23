# Speculation

A Ruby port of Clojure's `clojure.spec`. See [clojure.spec - Rationale and Overview](https://clojure.org/about/spec). The `Speculation` library is largely a copy-and-paste from `clojure.spec`. All credit goes to the clojure.spec authors.

## Installation

Add this line to your application's Gemfile:

```ruby
gem 'speculation'
```

And then execute:

    $ bundle

Or install it yourself as:

    $ gem install speculation

## Usage

The API is more-or-less the same as `clojure.spec`. If you're already familiar with `clojure.spec` then you should be write at home with `Speculation`. Clojure and Ruby and quite different languages, so naturally there are some differences:

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

### Namespaced keywords

Namespaced keywords are at the core of `clojure.spec`. Since spec utilises a global spec registry, namespaced keywords allow libraries to register specs with the same names but under different namespaces, thus removing accidental collisions. Ruby's equivalent to Clojure's keywords are Symbols. Ruby Symbol's don't have namespaces.

In order keep the global spec registry architecture in Speculation, we utilise refinements achieve similar behaviour:

```rb
using Speculation::NamespacedSymbols.refine(MyModule)

p :foo.ns
# => :"MyModule/foo"

p :foo.ns(AnotherModule)
# => :"AnotherModule/foo"
```

### FSpecs

#### Symbols

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

Speculation uses [`Rantly`](https://github.com/abargnesi/rantly) for random data generation. Generator functions in Speculation are Procs that take one argument (Rantly instance) and return random value. While clojure's test.check generators generate values that start small and continue to grow and get more complex as a property holds true, Rantly always generates random values.

Rantly gives Speculation the ability to shrink a failing test case down to a its smallest failing case, however in Speculation we limit this to Integers and Strings. This is an area where Speculation may currently be significantly weaker than clojure.spec.

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

## License

The gem is available as open source under the terms of the [MIT License](http://opensource.org/licenses/MIT).
