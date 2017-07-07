# Speculation [![Build Status](https://travis-ci.org/english/speculation.svg?branch=master)](https://travis-ci.org/english/speculation)

A Ruby port of Clojure's `clojure.spec`. This library is largely a copy-and-paste from clojure.spec so all credit goes to Rich Hickey and contributors for their original work.

See [clojure.spec - Rationale and Overview](https://clojure.org/about/spec) for a thorough outline of the problems clojure.spec (and thus Speculation) was built to address and how it addresses them. As a brief summary, Speculation allows you to describe the structure of datastructures and methods, enabling:

* declarative data validation and destructuring
* error reporting
* runtime method instrumentation, argument checking and stubbing
* generative testing

## Project Goals

The goal of this project is to match clojure.spec as closely as possible, from design to features to API. There aren't, and won't be, any significant departures from clojure.spec.

## Usage

API Documentation is available at [RubyDoc](http://www.rubydoc.info/github/english/speculation) and [the wiki](https://github.com/english/speculation/wiki) covers features at a higher level. The API is more-or-less the same as `clojure.spec` so if you're already familiar clojure.spec with then you should feel at home with Speculation. Most guides, talks and discussions around clojure.spec should apply equally well to Speculation.

To demonstrate most of the features of Speculation we can explore an implementation of [Conway's Game of Life](https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life):

### Game of Life demonstration

First, we'll require the [speculation][speculation] library along with the [generator][gen] and [test][test] modules. We'll be referring to `Speculation` a lot, so we'll add a shorthand to save us some typing:

```ruby
require 'speculation'
require 'speculation/test'
require 'speculation/gen'

S = Speculation
```

The Game of Life can be modelled with just a few simple entities. Let's describe them up front:

```ruby
# Our 'world' is a set of 'cells'.
S.def :"gol/world", S.coll_of(:"gol/cell", kind: Set)

# A cell is a tuple of coordinates.
S.def :"gol/cell", S.tuple(:"gol/coordinate", :"gol/coordinate")

# A coordinate is just an integer.
S.def :"gol/coordinate", S.with_gen(Integer) { S.gen(S.int_in(-5..5)) }
```

Let's unpick what we've done so far:

- We've registered 'specs' (via [`S.def`][s-def]) to a global registry of specs, naming then with [namespaced Symbols][ns-symbols].
- We've created both complex ([`S.coll_of`][coll_of] and [`S.tuple`][tuple]) and simple (`Integer`) specs.
- We've also leveraged the built-in generators for `S.coll_of` and `S.tuple` but swapped out the `Integer` `:"gol/coordinate"` generator for another built-in: [`S.int_in`][int_in]. While we don't have an upper or lower bound on a valid coordinate, using a more restrictive generator allows us to experiment with smaller worlds.

Before we move on to implementing the logic of the Game of Life, let's get an idea of the kind of data we'll be working with.

```ruby
S::Gen.generate(S.gen(:"gol/world"))
# => #<Set: {[2, -3], [-3, 5], [1, 1], [0, -3], [-5, 2], [-2, -3]}>
S::Gen.generate(S.gen(:"gol/world"))
# => #<Set: {}>
```

Our up front definition of specs is already paying off. We can generate random, valid examples of our expected domain entities and play around with them, either in tests or in a REPL (e.g. Pry, IRB). Without this kind of exploration we may not have initially considered the case where the world is empty!

Now to the logic of the game. Before we can implement [the rules](https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life#Rules), we must be able to find the neighbouring cells for a given cell.

```ruby
def self.neighbours(cell)
  cell_x, cell_y = cell
  (-1..1).to_a.repeated_permutation(2).map { |(x, y)| [cell_x - x, cell_y - y] }
end
```

Now I think that should work... But I'm not so confident. Let's write a spec for the method. Its argument should be a cell, which we already have a spec for. We'll assume our world has no boundaries, so any cell should have a set of 8 neighbours.

```ruby
S.fdef method(:neighbours),
  args: S.cat(cell: :"gol/cell"),
  ret: S.coll_of(:"gol/cell", count: 8, kind: Set)
```

Now that we've described the inputs and outputs of the method, we can generatively test it:

```ruby
S::Test.summarize_results S::Test.check(method(:neighbours))
# {:spec=>"Speculation::FSpec(main.neighbours)",
#  :method=>#<Method: main.neighbours>,
#  :failure=>
#   {:problems=>
#     [{:path=>[:ret],
#       :val=>[[-1, 6], [-1, 5], [-1, 4], [-2, 6], [-2, 5], [-2, 4], [-3, 6], [-3, 5], [-3, 4]],
#       :via=>[],
#       :in=>[],
#       :pred=>[Set,[[[-1, 6], [-1, 5], [-1, 4], [-2, 6], [-2, 5], [-2, 4], [-3, 6], [-3, 5], [-3, 4]]]]}],
#    :spec=>Speculation::EverySpec(),
#    :value=>[[-1, 6], [-1, 5], [-1, 4], [-2, 6], [-2, 5], [-2, 4], [-3, 6], [-3, 5], [-3, 4]],
#    :args=>[[-2, 5]],
#    :val=> [[-1, 6], [-1, 5], [-1, 4], [-2, 6], [-2, 5], [-2, 4], [-3, 6], [-3, 5], [-3, 4]],
#    :failure=>:check_failed}}
# => {:total=>1, :check_failed=>1}
```

Great, it's found a problem! This is saying that an input case was generated that failed our spec. The `:problems` values let's us know exactly what it found wrong. It's saying that the `:ret` part of our `neighbours` spec failed the 'Set' predicate. We can see that the value at `:val` is an Array, not a Set! There's our problem! Let's fix it by calling `to_set` before we return the collection of cells:

```ruby
def self.neighbours(cell)
  cell_x, cell_y = cell
  (-1..1).to_a.repeated_permutation(2).map { |(x, y)| [cell_x - x, cell_y - y] }.to_set
end

S::Test.summarize_results S::Test.check(method(:neighbours))
# {:spec=>"Speculation::FSpec(main.neighbours)",
#  :method=>#<Method: main.neighbours>,
#  :failure=>
#   {:problems=>
#     [{:path=>[:ret],
#       :pred=>
#        [#<Method: Speculation::Predicates.count_eq?>,
#         [8, #<Set: {[-4, -1], [-4, -2], [-4, -3], [-5, -1], [-5, -2], [-5, -3], [-6, -1], [-6, -2], [-6, -3]}>]],
#       :val=>
#        #<Set: {[-4, -1], [-4, -2], [-4, -3], [-5, -1], [-5, -2], [-5, -3], [-6, -1], [-6, -2], [-6, -3]}>,
#       :via=>[],
#       :in=>[]}],
#    :spec=>Speculation::EverySpec(),
#    :value=>
#     #<Set: {[-4, -1], [-4, -2], [-4, -3], [-5, -1], [-5, -2], [-5, -3], [-6, -1], [-6, -2], [-6, -3]}>,
#    :args=>[[-5, -2]],
#    :val=>
#     #<Set: {[-4, -1], [-4, -2], [-4, -3], [-5, -1], [-5, -2], [-5, -3], [-6, -1], [-6, -2], [-6, -3]}>,
#    :failure=>:check_failed}}
# => {:total=>1, :check_failed=>1}
```

So we've fixed our Set problem, but now we have another. The `:ret` spec has failed once again, but this time the `:pred` is `Speculation::Predicates.count_eq?`, with an argument of 8 and then a set of 9 cells. Aha! We're including the given cell in this set of neighbours, therefore getting one too neighbouring cells back! That's an easy fix:

```ruby
def self.neighbours(cell)
  cell_x, cell_y = cell
  block = (-1..1).to_a.repeated_permutation(2).map { |(x, y)| [cell_x - x, cell_y - y] }.to_set
  block - Set[cell]
end

S::Test.summarize_results S::Test.check(method(:neighbours))
# => {:total=>1, :check_passed=>1}
```

Great, we've managed to verify that, after generating many random inputs (1,000 by default), our method's return value satisfies the properties we defined in its spec. That gives me more confidence than a small handful of hand-written example tests would!

We can gain additional leverage from our spec: we can [`instrument`][instrument] the `neighbours` method so that it lets us know when it's been invoked with arguments that do not conform to its `:args` spec.

Before we do that, let's observe the method's current behavior when we provide deceptively invalid arguments:

```ruby
neighbours([1.0, 2.0])
# => #<Set: {[2.0, 3.0], [2.0, 2.0], [2.0, 1.0], [1.0, 3.0], [1.0, 1.0], [0.0, 3.0], [0.0, 2.0], [0.0, 1.0]}>
```

Here, we've provided a tuple of floats as a coordinate. This didn't raise any errors with our current implementation; it has dutifully gone to work and returned a value. However, the return value doesn't make sense: our program deals with integer pair coordinates. This situation would most likely lead to either invalid data or an exception at a later stage in our program, far away from the root cause of the problem (calling a method with incorrect types).

Let's address that by instrumenting our method so that it verifies its arguments at invocation time:

```ruby
S::Test.instrument method(:neighbours)
neighbours([1.0, 2.0])
# Speculation::Error: Call to 'main.neighbours' did not conform to spec:
# In: [0, 1] val: 2.0 fails spec: :"gol/coordinate" at: [:args, :cell, 1] predicate: [Integer, [2.0]]
# In: [0, 0] val: 1.0 fails spec: :"gol/coordinate" at: [:args, :cell, 0] predicate: [Integer, [1.0]]
# :spec Speculation::RegexSpec()
# :value [[1.0, 2.0]]
# :args [[1.0, 2.0]]
# :failure :instrument
# :caller "(pry):44:in `<main>'"
# from /Users/jamie/Projects/speculation/lib/speculation/test.rb:237:in `block in spec_checking_fn'
```

We can see from the error message that our argument has two problems: both elements of our array argument have failed the Integer predicate for the `:"gol/coordinate"` spec.  This is arguably much better feedback than if we hadn't instrumented this method.

We've demonstrated several Speculation features, so we'll leave this demo here. See the full [Game of Life example](examples/game_of_life.rb) where we take this idea further.

## Examples

* [sinatra-web-app](examples/sinatra-web-app): A small Sinatra web application demonstrating parameter validation and API error message generation.
* [spec_guide.rb](examples/spec_guide.rb): Speculation port of Clojure's [spec guide](https://clojure.org/guides/spec), demonstrating most features.
* [codebreaker.rb](examples/codebreaker.rb): Speculation port of the 'codebreaker' game described in [Interactive development with clojure.spec](http://blog.cognitect.com/blog/2016/10/5/interactive-development-with-clojurespec).
* [game_of_life.rb](examples/game_of_life.rb): [Conway's Game of Life](https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life) implementation.
* [json_parser.rb](examples/json_parser.rb): (toy) JSON parser using Speculation.

## Project status

Speculation will mirror any changes made to clojure.spec. clojure.spec is still in alpha so breaking changes should be expected.

While most of features of clojure.spec are implemented in Speculation, a few remain:

- [`form`](https://clojuredocs.org/clojure.spec/form)
- [`abbrev`](https://clojuredocs.org/clojure.spec/abbrev)
- [`describe`](https://clojuredocs.org/clojure.spec/describe)

## Improvements

Some things I hope to focus on in the near future:

- Explore alternative generator library
  - Build up a library of generators around Rantly in the meantime?
- Generate documentation from specs
  - perhaps integrating with [Pry's documentation browsing](https://github.com/pry/pry/wiki/Documentation-browsing)?
- Profile and optimise

## Development

After checking out the repo, run `bin/setup` to install dependencies. Then, run `rake` to run Rubocop and the test suite. You can also run `bin/console` for an interactive prompt that will allow you to experiment.

## Contributing

Bug reports and pull requests are welcome on GitHub at https://github.com/english/speculation.

## License

The gem is available as open source under the terms of the [MIT License](http://opensource.org/licenses/MIT).

[def]: http://www.rubydoc.info/github/english/speculation/master/Speculation#def-class_method
[coll_of]: http://www.rubydoc.info/github/english/speculation/master/Speculation#coll_of-class_method
[tuple]: http://www.rubydoc.info/github/english/speculation/master/Speculation#tuple-class_method
[int_in]: http://www.rubydoc.info/github/english/speculation/master/Speculation#int_in-class_method
[ns-symbols]: https://github.com/english/speculation/wiki/Namespaced-Symbols
[speculation]: http://www.rubydoc.info/github/english/speculation/master/Speculation
[gen]: http://www.rubydoc.info/github/english/speculation/master/Speculation/Gen
[test]: http://www.rubydoc.info/github/english/speculation/master/Speculation/Test
[instrument]: http://www.rubydoc.info/github/english/speculation/master/Speculation/Test#instrument-class_method
