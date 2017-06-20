# Speculation [![Build Status](https://travis-ci.org/english/speculation.svg?branch=master)](https://travis-ci.org/english/speculation)

A Ruby port of Clojure's `clojure.spec`. This library is largely a copy-and-paste from clojure.spec so all credit goes to Rich Hickey and contributors for their original work.

See [clojure.spec - Rationale and Overview](https://clojure.org/about/spec) for a thorough outline of the problems clojure.spec (and thus Speculation) was built to address and how it addresses them. As a brief summary, Speculation allows you to write predicate specs (nothing to do with RSpec!) which enable:

* declarative data validation and destructuring
* error reporting
* runtime method instrumentation, argument checking and stubbing
* generative testing

## Project Goals

The goal of this project is to match clojure.spec as closely as possible, from design to features to API. There aren't, and won't be any, significant departures from clojure.spec.

## Examples

* [sinatra-web-app](examples/sinatra-web-app): A small Sinatra web application demonstrating parameter validation and API error message generation.
* [spec_guide.rb](examples/spec_guide.rb): Speculation port of Clojure's [spec guide](https://clojure.org/guides/spec), demonstrating most features.
* [codebreaker.rb](examples/codebreaker.rb): Speculation port of the 'codebreaker' game described in [Interactive development with clojure.spec](http://blog.cognitect.com/blog/2016/10/5/interactive-development-with-clojurespec)
* [json_parser.rb](examples/json_parser.rb): JSON parser using Speculation.

## Usage

API Documentation is available at [RubyDoc](http://www.rubydoc.info/github/english/speculation) and [the wiki](https://github.com/english/speculation/wiki) covers features at a higher level. The API is more-or-less the same as `clojure.spec` so if you're already familiar clojure.spec with then you should feel at home with Speculation. Most guides, talks and discussions around clojure.spec should apply equally well to Speculation.

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

After checking out the repo, run `bin/setup` to install dependencies. Then, run `rake` to run rubocop and the tests. You can also run `bin/console` for an interactive prompt that will allow you to experiment.

## Contributing

Bug reports and pull requests are welcome on GitHub at https://github.com/english/speculation.

## License

The gem is available as open source under the terms of the [MIT License](http://opensource.org/licenses/MIT).
